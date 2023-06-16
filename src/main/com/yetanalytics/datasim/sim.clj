(ns com.yetanalytics.datasim.sim
  "Given input, compose a simulation model"
  (:require
   [clojure.spec.alpha :as s]
   [clojure.core.async :as a]
   [clojure.core.async.impl.protocols :as ap]
   [java-time :as t]
   [xapi-schema.spec :as xs]
   [com.yetanalytics.datasim.input :as input]
   [com.yetanalytics.datasim.timeseries :as ts]
   [com.yetanalytics.datasim.xapi :as xapi]
   [com.yetanalytics.datasim.xapi.profile :as p]
   [com.yetanalytics.datasim.xapi.activity :as activity]
   [com.yetanalytics.datasim.xapi.statement :as statement]
   [com.yetanalytics.datasim.util.xapi :as xapiu]
   [com.yetanalytics.datasim.util.maths :as maths]
   [com.yetanalytics.datasim.util.sequence :as su]
   [com.yetanalytics.datasim.util.async :as au]
   [com.yetanalytics.datasim.random :as random]
   [com.yetanalytics.pan.objects.template :as template])
  (:import [java.time ZoneRegion]
           [java.util Random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def min-ms 60000) ; The amount of milliseconds in one minute

(defn- actual-start-time
  "Set the actual time of the generated Statement to a time somwhere between
   `time-ms` and `time-ms + 1 min`, rather than always at `time-ms`.
   
   This is for additional spiciness :)"
  [time-ms rng]
  (long (+ time-ms (random/rand-int* rng min-ms))))

(defn- drop-time-probs
  "Given `prob-seq` consisting of `[time-ms prob]` pairs, drop the first couple
   of pairs; `prob` is the probability that on a given pair the dropping
   stops and `[[time-ms prob] & rest]` is returned."
  [prob-seq rng]
  (->> prob-seq
       (drop-while
        (fn [[_time-ms prob]]
          (or
           ;; micro-optimization - don't bother with rng if `prob` is 0
           (zero? prob)
           ;; choose `minutes` with probability `prob`
           (>= (random/rand* rng) prob))))
       not-empty))

(defn- drop-past-time-probs
  "Drop all `[time prob]` pairs where `time` occurs before `end-ms`."
  [prob-seq end-ms]
  (drop-while
   (fn [[time-ms _prob]] (< time-ms end-ms))
   prob-seq))

(s/fdef statement-seq
  :args (s/cat
         :input :com.yetanalytics.datasim/input
         :type-iri-map ::p/type-iri-map
         :activities ::activity/cosmos
         :actor ::xs/agent
         :alignment :alignment-map/alignments
         :actor-map :skeleton/actor-map)
  :ret :skeleton/statement-seq)

(defn- statement-seq
  "Return a lazy sequence of generated Statements; generation ends once
   `prob-seq` is exhausted."
  [input
   type-iri-map
   activities
   actor
   alignment
   {:keys [prob-seq reg-seq seed]}]
  (let [time-rng       (random/seed-rng seed)
        input-map-base {:input        input
                        :type-iri-map type-iri-map
                        :activities   activities
                        :actor        actor
                        :alignment    alignment}
        ;; time-ms -> start-ms -> <statement generator> -> end-ms
        ;; the sequence should resume after end-ms
        statement-seq*
        (fn statement-seq* [prob-seq reg-seq]
          (lazy-seq
           (when-some [[[time-ms _] & rest-prob-seq]
                       (drop-time-probs prob-seq time-rng)]
             (let [start-ms  (actual-start-time time-ms time-rng)
                   input-map (merge input-map-base
                                    (first reg-seq)
                                    {:sim-t start-ms})
                   statement (statement/generate-statement input-map)
                   end-ms    (:end-ms (meta statement))]
               (cons statement
                     (statement-seq*
                      (drop-past-time-probs rest-prob-seq end-ms)
                      (rest reg-seq)))))))]
    (statement-seq* prob-seq reg-seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skeleton
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- update-alignment
  [{existing-count  :count
    existing-weight :weight}
   {new-weight   :weight
    obj-override :objectOverride}]
  (let [count  (inc existing-count)
        weight (-> (* existing-count existing-weight)
                   (+ new-weight)
                   (/ count))]
    {:weight          weight
     :count           count
     :object-override obj-override}))

(defn- get-actor-alignments
  "Return `alignments` as a map from the component IDs to their alignment
   data, i.e. a map of `:weight`, `:count`, and `:object-override`. Only
   alignments that contain `actor-id`, `group-id`, or `role` will be
   included in the returned map."
  [alignments actor-id group-id role]
  (let [actor-alignment-ids (set [actor-id group-id role])]
    (reduce (fn [alignment-map {component-iri :component :as alignment}]
              (update alignment-map
                      component-iri
                      (fnil update-alignment {:weight 0.0 :count 0})
                      alignment))
            {}
            (for [{alignment-maps :alignments
                   alignment-id   :id} alignments
                  :when (actor-alignment-ids alignment-id)
                  alignment alignment-maps]
              alignment))))

(comment

  (:mod-seq
   (ts/time-seqs :t-zero (.toEpochMilli (java.time.Instant/now))))

  (let [group-arma
        (ts/arma-seq {:phi [0.5 0.2]
                      :theta []
                      :std 0.25
                      :c 0.0
                      :seed 100})
        actor-arma
        (ts/arma-seq {:phi [0.5 0.2]
                      :theta []
                      :std 0.25
                      :c 0.0
                      :seed 222})
        {:keys [day-night-seq mod-seq]}
        (ts/time-seqs :t-zero (.toEpochMilli (java.time.Instant/now)))
        lunch-hour-seq
        (map (fn [x]
               (if (<= 720 x 780)
                 1.0
                 -1.0))
             mod-seq)
        activity-prob-mask
        (ts/op-seq max
                   [group-arma
                    day-night-seq
                    lunch-hour-seq])]
    (ts/op-seq
     (fn [arma-n mask-n]
       (let [avg-n (-> (- arma-n mask-n) (/ 2))]
         (double (maths/min-max 0.0 avg-n 1.0))))
     [actor-arma activity-prob-mask])))

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
  [{:keys [profiles personae-array parameters alignments]
    :as   input}]
  (let [;; Input parameters and alignments
        {:keys [start end timezone seed] ?from-stamp :from} parameters
        {alignments :alignment-vector} alignments
        ;; Set timezone and time
        ^ZoneRegion zone (t/zone-id timezone)
        t-zero (.toEpochMilli (t/instant start))
        ;; Get actors and map actor IDs to (identified) group IDs
        actor-id-to-group-id-m (reduce
                                (fn [m {actors :member :as personae}]
                                  (let [group-id (xapiu/agent-id personae)]
                                    (reduce
                                     (fn [m' actor]
                                       (assoc m' (xapiu/agent-id actor) group-id))
                                     m
                                     actors)))
                                {}
                                personae-array)
        actors (apply concat (map :member personae-array))
        ;; If there's an end we need to set a ?sample-n for takes
        ?sample-n (when end
                    (let [t-end (.toEpochMilli (t/instant end))]
                      (- t-end t-zero)))
        ;; Useful time seqs
        {:keys [min-seq
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
        type-iri-map (-> (p/profiles->type-iri-map profiles)
                         ;; Select which primary patterns to generate from
                         (p/select-primary-patterns parameters))]
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
                                      (ts/op-seq
                                       (fn [a b]
                                         (double
                                          (maths/min-max 0.0 (/ (- a b) 2) 1.0)))
                                       [actor-arma mask]))
                      actor-alignment (get-actor-alignments
                                       alignments
                                       actor-id
                                       (get actor-id-to-group-id-m actor-id)
                                       (:role actor))
                      actor-reg-seed (.nextLong sim-rng)

                      ;; infinite seq of maps containing registration uuid,
                      ;; statement template, and a seed for generation
                      actor-reg-seq (p/registration-seq
                                     type-iri-map actor-alignment actor-reg-seed)

                      ;; additional seed for further gen
                      actor-seed (.nextLong sim-rng)

                      ;; Dissoc :role since it is not an xAPI property
                      actor-xapi (dissoc actor :role)]]
            [actor-id
             (cond->> (statement-seq
                       input
                       type-iri-map
                       activities
                       actor-xapi
                       actor-alignment
                       {:seed actor-seed
                        :prob-seq actor-prob
                        :reg-seq actor-reg-seq})
               ?from-stamp
               (drop-while
                (let [from-ms (t/to-millis-from-epoch ^String ?from-stamp)]
                  (fn [s]
                    (>= from-ms (-> s meta :timestamp-ms))))))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Sequence Simulation (Sync)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::select-agents
  (s/every ::xapi/agent-id))

(s/fdef sim-seq
  :args (s/cat :input :com.yetanalytics.datasim/input
               :options (s/keys*
                         :opt-un [::select-agents]))
  :ret :skeleton/statement-seq)

(defn sim-seq
  "Given input, build a skeleton and produce a seq of statements."
  [{{?max-statements :max} :parameters
    :as input}
   & {:keys [select-agents]}]
  (-> (build-skeleton input)
      (cond->
        select-agents
        (select-keys select-agents))
      ;; take the actor statement seqs
      vals
      (->> (su/seq-sort
            (comp :timestamp-ms
                  meta)))
      (cond->>
        ?max-statements
        (take ?max-statements))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Sequence Simulation (Async)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- chan?
  [x]
  (satisfies? ap/Channel x))

;; since we divide input.parameters.max by the number of agents in the sim when
;; generating in parallel, we can add a bit to each to get over max, or a lot to
;; account for an imbalance in activity at the tail of the sim

(s/def ::pad-chan-max
  pos-int?)

(s/fdef sim-chans
  :args (s/cat :input :com.yetanalytics.datasim/input
               :options (s/keys*
                         :opt-un [::select-agents
                                  ::pad-chan-max]))
  :ret (s/map-of ::xapi/agent-id
                 chan?))

(defn sim-chans
  "Given input, build a skeleton and produce a map of agent channels.

  Uses the core.async thread pool for concurrency.

  Note that input.parameters.max is implemented via division and may
  have unexpected results. input.parameters.end is preferable"
  [{{?max-statements :max} :parameters
    :as input}
   & {:keys [select-agents
             pad-chan-max]
      :or {pad-chan-max 1}}]
  (let [skeleton (cond-> (build-skeleton input)
                   select-agents
                   (select-keys select-agents))
        ?take-n (when ?max-statements
                  (+ pad-chan-max
                     (quot ?max-statements
                           (count skeleton))))]

    (reduce-kv
     (fn [m k v]
       (let [agent-seq v]
         (assoc m k (cond->> (a/to-chan! v)
                      ?take-n
                      (a/take ?take-n)))))
     (empty skeleton)
     skeleton)))

(s/def ::sort boolean?)
(s/def ::buffer-size pos-int?)

(s/fdef sim-chan
  :args (s/cat :input :com.yetanalytics.datasim/input
               :options (s/keys*
                         :opt-un [::select-agents
                                  ::pad-chan-max
                                  ::sort
                                  ::buffer-size]))
  :ret chan?)

(defn sim-chan
  "Merged output of `sim-chans` for parallel gen"
  [input
   & {:keys [sort
             buffer-size]
      :or {sort true
           buffer-size 100}
      :as kwargs}]
  (let [chan-map (apply sim-chans
                        input
                        (mapcat identity kwargs))]
    (if sort
      (au/sequence-messages
       (a/chan buffer-size)
       (fn [x y]
         (compare
          (-> x meta :timestamp-ms)
          (-> y meta :timestamp-ms)))
       (vals chan-map))
      (a/merge (vals chan-map)
               buffer-size))))


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

  (let [agent-chan
        (-> (sim-chans i)
            (get "mbox::mailto:alicefaux@example.org"))]
    (a/go-loop [cnt 0]
      (when-let [s (a/<! agent-chan)]
        (when (= 0
                 (mod cnt 10))
          (printf "\n%d statements\n\n" cnt)
          (println s))
        (recur (inc cnt)))
      ))

  (-> i :parameters clojure.pprint/pprint)

  (def ii
    (->
     i
     (assoc-in [:parameters :end] "2021-01-01T00:00:00.000000Z")))

  (time
   (-> ii sim-chan (->> (a/into [])) a/<!! count))

  (time
   (count (sim-seq ii)))

  )

(comment
  (get-actor-alignments

   [{:id         "mbox::mailto:bob@example.org"
     :type       "Agent"
     :alignments [{:component "https://example.org/activity/a"
                   :weight    0.5}
                  {:component "https://example.org/activity/c"
                   :weight    -0.2}]}]
   "mbox::mailto:bob@example.org"
   "trainee"
   "Lead Developer")

  (reduce
   (fn [m {actors :member :as personae}]
     (let [group-id (:name personae)]
       (reduce
        (fn [m' actor] (assoc m' (xapiu/agent-id actor) group-id))
        m
        actors)))
   {}
   [{:name "trainee"
     :objectType "Group"
     :member [{:name "Bob Fakename"
               :mbox "mailto:bob@example.org"
               :role "Lead Developer"}
              {:name "Alice Faux"
               :mbox "mailto:alice@example.org"
               :role "Lead Developer"}]}])

  (xapiu/agent-id {:name "Bob Fakename"
                   :mbox "mailto:bob@example.org"
                   :role "Lead Developer"}))
