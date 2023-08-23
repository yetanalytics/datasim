(ns com.yetanalytics.datasim.sim
  "Given input, compose a simulation model"
  (:require [clojure.spec.alpha :as s]
            [clojure.core.async :as a]
            [java-time.api      :as t]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.datasim                   :as-alias datasim]
            [com.yetanalytics.datasim.model             :as model]
            [com.yetanalytics.datasim.math.random       :as random]
            [com.yetanalytics.datasim.xapi.actor        :as actor]
            [com.yetanalytics.datasim.xapi.profile      :as p]
            [com.yetanalytics.datasim.xapi.registration :as reg]
            [com.yetanalytics.datasim.xapi.statement    :as statement]
            [com.yetanalytics.datasim.util.sequence     :as su]
            [com.yetanalytics.datasim.util.async        :as au])
  (:import [java.time ZoneRegion]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "skeleton" is a map of agent ids to maps with setup info for the agent.

;; A tuple of [timestamp probability]
(def prob-seq-moment-spec
  (s/tuple pos-int?
           (s/double-in
            :min 0.0
            :max 1.0
            :infinite? false
            :NaN? false)))

(s/def ::probability-seq
  (s/every prob-seq-moment-spec))

(s/def ::seed
  int?)

(s/def ::registration-seq
  (s/every ::p/registration-map))

;; Based on the probability of activity at a given minute, and an infinite seq
;; of profile walks, emit statements for one actor
(s/def :skeleton/statement-seq
  (s/every ::xs/statement :kind #(instance? clojure.lang.LazySeq %)))

(s/def ::skeleton
  (s/map-of ::actor/actor-ifi
            :skeleton/statement-seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def min-ms 60000.0) ; The amount of milliseconds in one minute

(s/fdef statement-seq
  :args (s/cat :inputs (s/keys :req-un [::statement/type-iri-map
                                        ::statement/activity-map
                                        ::statement/statement-base-map
                                        ::statement/parsed-rules-map
                                        ::statement/actor
                                        ::statement/alignments]
                               :opt-un [::statement/object-overrides])
               :probability-seq  ::probability-seq
               :registration-seq ::registration-seq
               :seed             ::seed)
  :ret :skeleton/statement-seq)

(defn- next-time
  "Generate a new millisecond time value that is added upon `prev-time`.
   The time difference is an exponentially-distributed random variable
   with `mean`; the `min` paramter also adds a fixed minimum
   time to the value, for a new mean `mean + min`. This ensures that the
   events occur as a Poisson random process. Note that this assumes the
   millisecond as the basic unit of time."
  [rng prev-time {:keys [mean min]
                  :or {mean min-ms
                       min  0}}]
  (let [rate  (/ 1.0 mean)
        delay (long (random/rand-exp rng rate))]
    (+ prev-time min delay)))

(defn- statement-seq
  "Generate a lazy sequence of xAPI Statements occuring as a Poisson
   process. Accepts a `registration-seq` where each entry includes the
   Statement Template and the temporal properties of each generated
   Statement. The sequence will either end at `?end-time` or, if `nil`,
   be infinite."
  [inputs registration-seq seed start-time ?end-time]
  (let [time-rng (random/seed-rng seed)
        end-cmp  (if (some? ?end-time)
                   (fn [sim-t] (< sim-t ?end-time))
                   (constantly true))
        statement-seq*
        (fn statement-seq* [sim-time registration-seq]
          (lazy-seq
           (let [reg-map    (first registration-seq)
                 time-delay (:time-delay reg-map)
                 sim-t      (next-time time-rng sim-time time-delay)
                 input-map  (merge inputs reg-map {:sim-t sim-t})]
             (if (end-cmp sim-t)
               (cons (statement/generate-statement input-map)
                     (statement-seq* sim-t (rest registration-seq)))
               '()))))]
    (statement-seq* start-time registration-seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skeleton
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data structure helpers

(defn- personaes->group-actor-id-map
  "Convert `personae-array` into a map from group IDs, which represent
   each personae in the array, to actor IDs, representing each group member."
  [personae-array]
  (reduce
   (fn [m {actors :member :as personae}]
     (let [group-id (actor/actor-ifi personae)]
       (reduce
        (fn [m* actor]
          (assoc m* (actor/actor-ifi actor) group-id))
        m
        actors)))
   {}
   personae-array))

;; Timestamp helpers

(defn- timezone->region ^ZoneRegion [tz]
  (t/zone-id tz))

(defn- timestamp->millis [ts]
  (.toEpochMilli (t/instant ts)))

(defn- drop-statements-from-time
  "Drop any `statements` whose `:timestamp-ms` metadata comes after
   `from-ms`."
  [from-ms statements]
  (drop-while
   (fn [statement]
     (>= from-ms (-> statement meta :timestamp-ms)))
   statements))

;; Time/probability sequence helpers

(s/fdef build-skeleton
  :args (s/cat :input ::datasim/input)
  :ret ::skeleton)

(defn build-skeleton
  "Given simulation input, return a skeleton with statement sequences per
   actor from `start` of sim. Should be run once (in a single thread).
  
   Spooky."
  [{:keys [profiles personae-array models parameters]}]
  (let [;; Input parameters
        {:keys [start end timezone seed] ?from-stamp :from} parameters
        ;; RNG for generating the rest of the seeds
        sim-rng     (random/seed-rng seed)
        ;; Set timezone region and timestamps
        zone-region (timezone->region timezone)
        t-start     (timestamp->millis start)
        ?t-from     (some-> ?from-stamp timestamp->millis)
        ?t-end      (some-> end timestamp->millis)
        ?sample-ms  (some-> ?t-end (- t-start))
        ;; Derive actor, activity, and profile object colls and maps
        actor-seq       (apply concat (map :member personae-array))
        actor-group-map (personaes->group-actor-id-map personae-array)
        ;; Derive profiles map
        activity-seed   (random/rand-unbound-int sim-rng)
        profiles-map    (p/profiles->profile-map profiles parameters activity-seed)
        ;; Derive model alignments + object overrides
        models-map      (model/models->map models)]
    ;; Now, for each actor we initialize what is needed for the sim
    (->> actor-seq
         (sort-by actor/actor-ifi)
         (reduce
          (fn [m actor]
            (let [;; Actor basics + alignment
                  actor-id        (actor/actor-ifi actor)
                  actor-role      (:role actor)
                  actor-group-id  (get actor-group-map actor-id)
                  actor-model-map (model/get-actor-model models-map
                                                         actor-id
                                                         actor-group-id
                                                         actor-role)
                  actor-alignment (:alignments actor-model-map)
                  ;; Actor registration seq
                  actor-reg-seed  (random/rand-unbound-int sim-rng)
                  actor-reg-seq   (reg/registration-seq profiles-map
                                                        actor-alignment
                                                        actor-reg-seed)
                  ;; Additional seed for further gen
                  actor-seed      (random/rand-unbound-int sim-rng)
                  ;; Dissoc `:role` since it is not an xAPI property
                  actor-xapi      (dissoc actor :role)
                  actor-xapi-map  {:actor actor-xapi}
                  ;; Statement seq
                  actor-input     (merge profiles-map
                                         actor-model-map
                                         actor-xapi-map)
                  actor-stmt-seq* (statement-seq actor-input
                                                 actor-reg-seq
                                                 actor-seed
                                                 t-start
                                                 ?t-end)
                  actor-stmt-seq  (cond->> actor-stmt-seq*
                                    ?t-from
                                    (drop-statements-from-time ?t-from))]
              (assoc m actor-id actor-stmt-seq)))
          {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Sequence Simulation (Sync)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::select-agents
  (s/every ::actor/actor-ifi))

(s/fdef sim-seq
  :args (s/cat :input :com.yetanalytics.datasim/input
               :options (s/keys*
                         :opt-un [::select-agents]))
  :ret :skeleton/statement-seq)

(defn sim-seq
  "Given input, build a skeleton and produce a seq of statements."
  [{{?max-statements :max} :parameters :as input}
   & {:keys [select-agents]}]
  (let [skeleton (cond-> (build-skeleton input)
                   select-agents
                   (select-keys select-agents))]
    (cond->> (->> skeleton vals (su/seq-sort (comp :timestamp-ms meta)))
      ?max-statements (take ?max-statements))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Sequence Simulation (Async)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; simulate multiple channels

(s/def ::pad-chan-max
  pos-int?)

(s/fdef sim-chans
  :args (s/cat :input :com.yetanalytics.datasim/input
               :options (s/keys*
                         :opt-un [::select-agents
                                  ::pad-chan-max]))
  :ret (s/map-of ::actor/actor-ifi au/chan?))

(defn sim-chans
  "Given input, build a skeleton and produce a map from agent IFIs to
   agent statement simulation channels.

   Uses the `core.async` thread pool for concurrency.

   Note that the `:max` parameter is used as a quotient and may
   have unexpected results if it is zero. The `:end` parameter is preferable.
   
   The `:max` parameter is divided by the number of agents in the simulation.
   Thus `pad-chan-max` is provided as a kwarg so we can add that amount to
   the length of each channel's statement seq - either a little bit to get over
   `:max`, or a lot to account for an imbalance in activity at the tail end
   of the simulation."
  [{{?max-statements :max} :parameters :as input}
   & {:keys [select-agents
             pad-chan-max]
      :or {pad-chan-max 1}}]
  (let [skeleton  (cond-> (build-skeleton input)
                    select-agents
                    (select-keys select-agents))
        ?take-n   (when ?max-statements ; TODO: Handle division by zero error
                    (->> (count skeleton)
                         (quot ?max-statements)
                         (+ pad-chan-max)))
        seq->chan (fn [agent-seq]
                    (cond->> (a/to-chan! agent-seq)
                      ?take-n
                      (a/take ?take-n)))]
    (update-vals skeleton seq->chan)))

;; simulate single channel

(defn- compare-timestamp-ms-meta
  [stmt-1 stmt-2]
  (compare
   (-> stmt-1 meta :timestamp-ms)
   (-> stmt-2 meta :timestamp-ms)))

(s/def ::sort boolean?)
(s/def ::buffer-size pos-int?)

(s/fdef sim-chan
  :args (s/cat :input :com.yetanalytics.datasim/input
               :options (s/keys*
                         :opt-un [::select-agents
                                  ::pad-chan-max
                                  ::sort
                                  ::buffer-size]))
  :ret au/chan?)

(defn sim-chan
  "Merged output of `sim-chans` for parallel generation."
  [input
   & {:keys [sort buffer-size]
      :or {sort true
           buffer-size 100}
      :as kwargs}]
  (let [chan-map (sim-chans input kwargs)
        chans    (vals chan-map)]
    (if sort
      (->> chans
           (au/sequence-messages (a/chan buffer-size)
                                 compare-timestamp-ms-meta))
      (-> chans
          (a/merge buffer-size)))))
