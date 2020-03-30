(ns com.yetanalytics.datasim.sim
  "Given input, compose a simulation model"
  (:require
   [clojure.spec.alpha :as s]
   [com.yetanalytics.datasim.input :as input]
   [com.yetanalytics.datasim.timeseries :as ts]
   [com.yetanalytics.datasim.xapi :as xapi]
   [com.yetanalytics.datasim.xapi.profile :as p]
   [com.yetanalytics.datasim.xapi.activity :as activity]
   [com.yetanalytics.datasim.xapi.statement :as statement]
   [com.yetanalytics.datasim.util.xapi :as xapiu]
   [com.yetanalytics.datasim.util.maths :as maths]
   [com.yetanalytics.datasim.util.sequence :as su]
   [com.yetanalytics.datasim.random :as random]
   [com.yetanalytics.pan.objects.template :as template]
   [xapi-schema.spec :as xs]
   [java-time :as t])
  (:import [java.time Instant ZoneRegion]
           [java.util Random]))


;; this don't work
(def unrealized-lazy-seq-spec
  (s/and #(instance? clojure.lang.LazySeq %)
          (complement realized?)))

;; "skeleton" is a map of agent ids to maps with setup info for the agent.

;; A tuple of [timestamp probability]
(s/def :prob-seq/moment
  (s/tuple pos-int?
           (s/double-in
            :min 0.0
            :max 1.0
            :infinite? false
            :NaN? false)))

(s/def :actor-map/prob-seq
  (s/every :prob-seq/moment))


(s/def :stub/registration
  ::xs/uuid)

(s/def :stub/seed
  int?)

(s/def :reg-seq/stub
  (s/keys :req-un [:stub/registration
                   ::template/template
                   :stub/seed]))

(s/def :actor-map/reg-seq
  (s/every :reg-seq/stub))

(s/def :skeleton/actor-map
  (s/keys :req-un [:actor-map/prob-seq
                   :actor-map/reg-seq
                   :actor-map/seed
                   ]))

;; Based on the probability of activity at a given minute, and an infinite seq
;; of profile walks, emit statements for one actor
(s/def :skeleton/statement-seq
  (s/every
   ;; stubbed
   map?
   #_::xs/statement))

(s/fdef statement-seq
  :args (s/cat
         :input :com.yetanalytics.datasim/input
         :iri-map ::p/iri-map
         :activities ::activity/cosmos
         :actor ::xs/agent
         :alignment :alignment-map/actor-alignment
         :actor-map :skeleton/actor-map)
  :ret :skeleton/statement-seq)

(defn statement-seq
  [input
   iri-map
   activities
   actor
   alignment
   {:keys [prob-seq
           reg-seq
           seed
           rng]
    :as actor-map}]
  (lazy-seq
   (let [^Random rng (or rng (random/seed-rng seed))]
     (when-let [[[t _] & rest-prob]
                (->> prob-seq
                     (drop-while
                      (fn [[t prob]]
                        (or (zero? prob)
                            (>= (random/rand* rng) prob))))
                     not-empty)]
       (let [;; for additional spiciness, set the actual time of activity
             ;; (or at least that passed to the statement generator) to a time
             ;; somewhere between t and t+1m
             t-actual (long (+ t (random/rand-int* rng 60000)))

             ;; t -> t-actual -> <statement generator> -> timestamp, duration
             ;; the sequence should resume after timestamp + duration.

             ;; TODO: The statement gen function will return a statement
             ;; with duration ms in the meta.
             statement (statement/generate-statement
                        (merge (first reg-seq)
                               {:input input
                                :iri-map iri-map
                                :activities activities
                                :actor actor
                                :alignment alignment
                                :sim-t t-actual}))

             #_statement #_(with-meta
                         (assoc (first reg-seq)
                                :t (.toString (Instant/ofEpochMilli t-actual)))
                         {:end-ms (+ t-actual
                                     (long
                                      (random/rand-gauss
                                       rng 600000.0 0.5)))})
             ;; Get the length of time the statement took from the gen function.
             {:keys [end-ms]} (meta statement)]
         (cons statement
               (statement-seq
                input
                iri-map
                activities
                actor
                alignment
                {:prob-seq (drop-while
                            (comp
                             (partial >
                                      end-ms)
                             first)
                            rest-prob)
                 :reg-seq (rest reg-seq)
                 :seed seed
                 :rng rng})))))))

(s/def ::skeleton
  (s/map-of ::xapi/agent-id
            :skeleton/statement-seq))

(s/fdef build-skeleton
  :args (s/cat :input :com.yetanalytics.datasim/input)
  :ret ::skeleton)

(defn build-skeleton
  "Given simulation input, return a skeleton with statement
  sequences per actor from `start` of sim.

  Should be run once (in a single thread)
  Spooky."
  [{{actors :member} :personae
    {:keys [start from end
            timezone seed


            ]} :parameters
    profiles :profiles
    {alignments :alignment-map} :alignments
    :as input}]
  (let [^ZoneRegion zone (t/zone-id timezone)
        t-zero (.toEpochMilli (t/instant start))
        ;; If there's an end we need to set a ?sample-n for takes
        ?sample-n (when end
                    (let [t-end (.toEpochMilli (t/instant end))]
                      (- t-end t-zero)))
        ;; Useful time seqs
        {:keys [;; week-seq
                min-seq
                ;; t-seq
                ;; doy-seq
                ;; moh-seq
                ;; day-seq
                ;; sec-seq
                ;; dom-seq
                ;; hod-seq
                ;; hour-seq
                ;; dow-seq
                mod-seq
                day-night-seq]} (if ?sample-n
                                  (ts/time-seqs :t-zero t-zero
                                                :sample-n ?sample-n
                                                :zone zone)
                                  (ts/time-seqs :t-zero t-zero
                                                :zone zone))

        ;; Right now we are using common ARMA settings, this may change
        common-arma {:phi [0.5 0.2]
                     :theta []
                     :std 0.25
                     :c 0.0}
        ;; RNG for generating the rest of the seeds
        ^Random sim-rng (Random. seed)

        ;; Generate a seed for the group
        group-seed (.nextLong sim-rng)

        ;; Generate an ARMA seq for the group
        group-arma (ts/arma-seq (merge common-arma
                                       {:seed group-seed}))

        ;; We're going to make a probability 'mask' out of the group arma and
        ;; the day-night cycle. We'll also add the lunch our, when nothing
        ;; should start. All of this should probably be configurable later on.

        ;; The lunch hour seq is derived from what minute in the day it is
        lunch-hour-seq (map
                        (fn [x]
                          (if (<= 720 x 780)
                            1.0
                            -1.0))
                        mod-seq)

        ;; Compose the activity probability mask from the group-arma, day-night,
        ;; and lunch seqs
        mask (ts/op-seq max
                        [group-arma
                         day-night-seq
                         lunch-hour-seq])
        ;; activities used in the sim
        activities (activity/derive-cosmos input (.nextLong sim-rng))
        iri-map (apply p/profiles->map profiles)]
    ;; Now, for each actor we 'initialize' what is needed for the sim
    (into {}
          (for [[actor-id actor] (sort-by first (map (juxt xapiu/agent-id
                                                           identity)
                                                     actors))
                :let [;; seed specifically for the ARMA model
                      actor-arma-seed (.nextLong sim-rng)
                      ;; an arma seq
                      actor-arma (ts/arma-seq
                                  (merge common-arma
                                         {:seed actor-arma-seed}))
                      actor-prob (map vector
                                      min-seq
                                      (ts/op-seq (fn [a b]
                                                   (double
                                                    (maths/min-max 0.0 (/ (- a b) 2) 1.0)))
                                                 [actor-arma mask]))

                      actor-alignment (get alignments actor-id {})
                      actor-reg-seed (.nextLong sim-rng)

                      ;; infinite seq of maps containing registration uuid,
                      ;; statement template, and a seed for generation
                      actor-reg-seq (p/registration-seq
                                     profiles actor-alignment actor-reg-seed)

                      ;; additional seed for further gen
                      actor-seed (.nextLong sim-rng)]]
            [actor-id
             (statement-seq
              input
              iri-map
              activities
              actor
              actor-alignment
              {:seed actor-seed
               :prob-seq actor-prob
               :reg-seq actor-reg-seq})]))))

(s/fdef sim-seq
  :args (s/cat :input :com.yetanalytics.datasim/input)
  :ret :skeleton/statement-seq)

(defn sim-seq
  "Given input, build a skeleton and produce a seq of statements."
  [{{?max-statements :max
     ?from-stamp     :from} :parameters
    :as input}]
  (-> (build-skeleton input)
      ;; take the actor statement seqs
      vals
      (->> (su/seq-sort
            (comp :timestamp-ms
                  meta)))
      (cond->>
        ?from-stamp
        (drop-while
         (let [from-ms (t/to-millis-from-epoch ^String ?from-stamp)]
           (fn [s]
             (>= from-ms (-> s meta :timestamp-ms)))))
        ?max-statements
        (take ?max-statements))))


(comment
  (def i (input/from-location :input :json "dev-resources/input/simple.json"))


  (def skel
    (time (build-skeleton i)))

  (s/explain ::skeleton skel)

  (-> skel
      first
      second
      (->> (take 10))
      clojure.pprint/pprint
      )


  (s/explain ::xs/uuid "B73E0E16-386D-3D6D-8AE9-33B09C1C599E")

  (.toString (Instant/ofEpochMilli
   1574078559219))

  )
