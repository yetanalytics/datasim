(ns com.yetanalytics.datasim.model
  (:require [clojure.spec.alpha :as s]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.datasim.input.model           :as model]
            [com.yetanalytics.datasim.math.random           :as random]
            [com.yetanalytics.datasim.model.alignment       :as-alias alignment]
            [com.yetanalytics.datasim.model.object-override :as-alias obj-override]
            [com.yetanalytics.datasim.xapi.actor            :as actor]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::alignment/weights
  (s/map-of ::xs/iri ::random/weight))

;; TODO: Temporal properties
(s/def ::alignments
  (s/keys :opt-un [::alignment/weights]))

(s/def ::obj-override/weights
  (s/map-of :statement/object ::random/weight))

(s/def ::obj-override/objects
  (s/coll-of :statement/object :kind vector? :min-count 1))

(s/def ::object-overrides
  (s/keys :req-un [::obj-override/objects]
          :opt-un [::obj-override/weights]))

(s/def ::model
  (s/keys :opt-un [::alignments
                   ::object-overrides]))

(s/def ::default-model (s/nilable ::model))
(s/def ::agent-models (s/map-of ::actor/actor-ifi ::model))
(s/def ::group-models (s/map-of ::actor/actor-ifi ::model))
(s/def ::role-models (s/map-of string? ::model))

(def model-map-spec
  (s/keys :req-un [::default-model
                   ::agent-models
                   ::group-models
                   ::role-models]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Temporal properties
(defn- mapify-alignments
  [alignments]
  {:weights (reduce (fn [m {:keys [id weight]}] (assoc m id weight))
                    {}
                    alignments)})

(defn- mapify-object-overrides
  [object-overrides]
  {:weights (reduce (fn [m {:keys [weight object]}] (assoc m object weight))
                    {}
                    object-overrides)
   :objects (map :object object-overrides)})

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
     (fn [acc {:keys [personae alignments objectOverrides]}]
       (let [model* (cond-> {}
                      (not-empty alignments)
                      (assoc :alignments
                             (mapify-alignments alignments))
                      (not-empty objectOverrides)
                      (assoc :object-overrides
                             (mapify-object-overrides objectOverrides)))]
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
  :ret ::model)

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
