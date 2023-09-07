(ns com.yetanalytics.datasim.sim
  "Given input, compose a simulation model"
  (:require [clojure.spec.alpha :as s]
            [clojure.core.async :as a]
            [java-time.api      :as t]
            [com.yetanalytics.datasim                   :as-alias datasim]
            [com.yetanalytics.datasim.model             :as model]
            [com.yetanalytics.datasim.math.random       :as random]
            [com.yetanalytics.datasim.xapi.actor        :as actor]
            [com.yetanalytics.datasim.xapi.profile      :as p]
            [com.yetanalytics.datasim.xapi.statement    :as statement]
            [com.yetanalytics.datasim.util.sequence     :as su]
            [com.yetanalytics.datasim.util.async        :as au]
            [com.yetanalytics.datasim.model.temporal    :as temporal]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef statement-seq
  :args (s/cat :inputs     (s/keys :req-un [::statement/type-iri-map
                                            ::statement/activity-map
                                            ::statement/statement-base-map
                                            ::statement/parsed-rules-map
                                            ::statement/pattern-walk-fn
                                            ::statement/actor
                                            ::statement/alignments]
                                   :opt-un [::statement/object-overrides])
               :rng        ::random/rng
               :alignments ::model/alignments
               :start-time ::temporal/date-time
               :?end-time  ::temporal/date-time
               :?from-time ::temporal/date-time
               :zone-region string?)
  :ret :skeleton/statement-seq)

(defn- init-simulation-seq
  [pattern-walk-fn rng alignments]
  (for [registration (repeatedly (partial random/rand-uuid rng))]
    [registration (pattern-walk-fn alignments rng)]))

(defn- time-simulation-seq*
  [rng {prev-timestamp     :timestamp
        prev-timestamp-gen :timestamp-gen
        registration-seq   :registration-seq}]
  (let [;; Destructuring
        [registration & rest-regs]     registration-seq
        [registration-id template-seq] registration
        [template & rest-templates]    template-seq
        {:keys [period bounds retry?]} (meta template)
        ;; New
        timestamp (temporal/add-period prev-timestamp rng period)]
    (if (temporal/bounded-time? bounds timestamp)
      ;; Timestamp is in bound; valid statement gen
      (let [time-since-last   (t/duration prev-timestamp-gen timestamp)
            registration-seq* (cond->> rest-regs
                                (not-empty rest-templates)
                                (cons [registration-id rest-templates]))]
        {:result           {:timestamp       timestamp
                            :time-since-last time-since-last
                            :template        template
                            :registration    registration-id}
         :timestamp        timestamp
         :timestamp-gen    timestamp
         :registration-seq registration-seq*})
      ;; Timestamp is not in bound; skip to next bounded time
      ;; (and either retry this template or skip to next registration)
      (when-some [timestamp (temporal/next-bounded-time bounds timestamp)]
        (let [registration-seq* (cond->> rest-regs
                                  (and retry? (not-empty rest-templates))
                                  (cons [registration-id rest-templates]))]
          {:timestamp        timestamp
           :timestamp-gen    prev-timestamp-gen
           :registration-seq registration-seq*})))))

(defn- time-simulation-seq
  [rng start-time registration-seq]
  (->> (iterate (partial time-simulation-seq* rng)
                {:timestamp        start-time
                 :timestamp-gen    start-time
                 :registration-seq registration-seq})
       (take-while some?)
       (keep :result)))

(defn- drop-simulation-seq
  [?end-time simulation-seq]
  (let [before-end?
        (if (some? ?end-time)
          (fn [{:keys [timestamp]}]
            (t/before? timestamp ?end-time))
          (constantly true))]
    (take-while before-end? simulation-seq)))

(defn- seed-simulation-seq
  [rng simulation-seq]
  (map #(assoc % :seed (random/rand-unbound-int rng))
       simulation-seq))

(defn- from-simulation-seq
  [?from-time simulation-seq]
  (let [before-from?
        (if (some? ?from-time)
          (fn [{:keys [timestamp]}]
            ;; Also excludes timestamps that equal from-time
            (not (t/after? timestamp ?from-time)))
          (constantly false))]
    (drop-while before-from? simulation-seq)))

(defn- gens-simulation-seq
  [input simulation-seq]
  (map #(statement/generate-statement (merge input %))
       simulation-seq))

(defn simulation-seq
  "Generate a lazy sequence of xAPI Statements occuring as a Poisson
   process. The sequence will either end at `?end-time` or, if `nil`,
   be infinite."
  [{:keys [pattern-walk-fn] :as input} seed alignments start-time ?end-time ?from-time zone-region]
  (let [sim-rng  (random/seed-rng seed)
        temp-rng (random/rand-unbound-int sim-rng)
        time-rng (random/rand-unbound-int sim-rng)
        stmt-rng (random/rand-unbound-int sim-rng)]
    (->> (init-simulation-seq pattern-walk-fn temp-rng alignments)
         (time-simulation-seq time-rng start-time)
         (drop-simulation-seq ?end-time)
         (seed-simulation-seq stmt-rng)
         (from-simulation-seq ?from-time)
         (gens-simulation-seq (assoc input :timezone zone-region)))))

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

(s/fdef build-skeleton
  :args (s/cat :input ::datasim/input)
  :ret ::skeleton)

(defn build-skeleton
  "Given simulation input, return a skeleton with statement sequences per
   actor from `start` of sim. Should be run once (in a single thread).
  
   Spooky."
  [{:keys [profiles personae-array models parameters]}]
  (let [;; Input parameters
        {:keys [start end from timezone seed]} parameters
        ;; RNG for generating the rest of the seeds
        sim-rng     (random/seed-rng seed)
        ;; Set timezone region and timestamps
        zone-region (t/zone-id timezone)
        start-time  (-> start t/instant (t/local-date-time zone-region))
        ?end-time   (some-> end t/instant (t/local-date-time zone-region))
        ?from-time  (some-> from t/instant (t/local-date-time zone-region))
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
                  ;; Additional seed for further gen
                  actor-seed      (random/rand-unbound-int sim-rng)
                  ;; Dissoc `:role` since it is not an xAPI property
                  actor-xapi      (dissoc actor :role)
                  actor-xapi-map  {:actor actor-xapi}
                  ;; Statement seq
                  actor-input     (merge profiles-map
                                         actor-model-map
                                         actor-xapi-map)
                  actor-stmt-seq  (simulation-seq actor-input
                                                  actor-seed
                                                  actor-alignment
                                                  start-time
                                                  ?end-time
                                                  ?from-time
                                                  zone-region)]
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
    (cond->> (->> skeleton vals (su/seq-sort (comp :time-ms meta)))
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

(defn- compare-time-ms-meta
  [stmt-1 stmt-2]
  (compare
   (-> stmt-1 meta :time-ms)
   (-> stmt-2 meta :time-ms)))

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
                                 compare-time-ms-meta))
      (-> chans
          (a/merge buffer-size)))))
