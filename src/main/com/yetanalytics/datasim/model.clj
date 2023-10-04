(ns com.yetanalytics.datasim.model
  (:require [clojure.spec.alpha :as s]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.datasim.input.model            :as model]
            [com.yetanalytics.datasim.input.model.alignments :as model.alignments]
            [com.yetanalytics.datasim.util.random            :as random]
            [com.yetanalytics.datasim.model.weights          :as-alias weights]
            [com.yetanalytics.datasim.model.pattern          :as-alias pattern]
            [com.yetanalytics.datasim.model.alignment        :as-alias alignment]
            [com.yetanalytics.datasim.model.alignment.period :as-alias alignment.period]
            [com.yetanalytics.datasim.model.object-override  :as-alias obj-override]
            [com.yetanalytics.datasim.model.bounds           :as bounds]
            [com.yetanalytics.datasim.model.periods          :as periods]
            [com.yetanalytics.datasim.xapi.actor             :as actor]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::objects
  (s/coll-of ::model.alignments/object))

(s/def ::weights/verbs
  (s/map-of ::model.alignments/id ::random/weight))

(s/def ::weights/activities
  (s/map-of ::model.alignments/id ::random/weight))

(s/def ::weights/activity-types
  (s/map-of ::model.alignments/id ::random/weight))

(s/def ::weights/object-overrides
  (s/map-of ::model.alignments/object ::random/weight))

(s/def ::weights
  (s/keys :opt-un [::weights/verbs
                   ::weights/activities
                   ::weights/activity-types
                   ::weights/object-overrides]))

(s/def ::pattern/weights
  (s/map-of ::model.alignments/id ::random/weight))

(s/def ::pattern/bounds
  ::bounds/bounds)

(s/def ::pattern/bound-retries
  (s/every ::xs/iri :kind set?))

(s/def ::pattern/period
  ::periods/period)

(s/def ::pattern/repeat-max
  pos-int?)

(s/def ::pattern
  (s/keys :opt-un [::pattern/weights
                   ::pattern/bounds
                   ::pattern/bound-retries
                   ::pattern/period
                   ::pattern/repeat-max]))

(s/def ::patterns
  (s/every ::pattern))

(s/def ::alignments
  (s/keys :req-un [::weights
                   ::objects
                   ::patterns]))

(s/def ::default-model (s/nilable ::alignments))
(s/def ::agent-models (s/map-of ::actor/actor-ifi ::alignments))
(s/def ::group-models (s/map-of ::actor/actor-ifi ::alignments))
(s/def ::role-models (s/map-of string? ::alignments))

(def model-map-spec
  (s/keys :req-un [::default-model
                   ::agent-models
                   ::group-models
                   ::role-models]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- reduce-weights
  ([alignments]
   (reduce-weights alignments :id))
  ([alignments id-keyword]
   (reduce (fn [acc m]
             (let [id (get m id-keyword)]
               (if-some [weight (:weight m)]
                 (assoc acc id weight)
                 acc)))
           {}
           alignments)))

(defn- reduce-patterns
  [patterns]
  (reduce
   (fn [acc {:keys [id weights repeatMax bounds boundRestarts periods]}]
     (let [m (cond-> {}
               weights       (assoc :weights (reduce-weights weights))
               bounds        (assoc :bounds (bounds/convert-bounds bounds))
               boundRestarts (assoc :bound-restarts (set boundRestarts))
               periods       (assoc :periods (periods/convert-periods periods))
               repeatMax     (assoc :repeat-max repeatMax))]
       (assoc acc id m)))
   {}
   patterns))

(defn- mapify-alignments
  [{:keys [verbs activities activityTypes patterns templates objectOverrides]}]
  {:weights  {:verbs            (reduce-weights verbs)
              :activities       (reduce-weights activities)
              :activity-types   (reduce-weights activityTypes)
              :object-overrides (reduce-weights objectOverrides :object)}
   :objects  (mapv :object objectOverrides)
   :patterns (merge (reduce-patterns patterns)
                    (reduce-patterns templates))})

(s/fdef models->map
  :args (s/cat :models ::model/models)
  :ret model-map-spec)

(defn models->map
  "Given `models`, return a map of maps from agent, group, and role IDs to
   models, as well as the singular `:default-model`."
  [models]
  (let [init-map       {:default-model nil
                        :agent-models  {}
                        :group-models  {}
                        :role-models   {}}
        persona-type-m {"Agent" :agent-models
                        "Group" :group-models
                        "Role"  :role-models}]
    (reduce
     (fn [acc {:keys [personae] :as model}]
       (let [model* (mapify-alignments model)]
         (if (some? personae)
           (reduce
            (fn [acc* {persona-id   :id
                       persona-type :type}]
              (let [persona-kw (get persona-type-m persona-type)]
                (assoc-in acc* [persona-kw persona-id] model*)))
            acc
            personae)
           (assoc acc :default-model model*))))
     init-map
     models)))

(s/fdef get-actor-model
  :args (s/cat :model-map model-map-spec
               :agent-id ::actor/actor-ifi
               :group-id ::actor/actor-ifi
               :role-id  (s/and string? not-empty))
  :ret ::alignments)

(defn get-actor-model
  "Get the appropriate model associated with the actor described by
   the various IDs, with `agent-id`, `group-id`, and `role-id` going
   from greatest to least precedence."
  [{:keys [default-model agent-models group-models role-models]}
   agent-id
   group-id
   role-id]
  (or (get agent-models agent-id)
      (get group-models group-id)
      (get role-models role-id)
      default-model))
