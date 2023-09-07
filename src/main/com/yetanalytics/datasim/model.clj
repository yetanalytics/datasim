(ns com.yetanalytics.datasim.model
  (:require [clojure.spec.alpha :as s]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.datasim.input.model            :as model]
            [com.yetanalytics.datasim.math.random            :as random]
            [com.yetanalytics.datasim.model.alignment        :as-alias alignment]
            [com.yetanalytics.datasim.model.alignment.period :as-alias alignment.period]
            [com.yetanalytics.datasim.model.object-override  :as-alias obj-override]
            [com.yetanalytics.datasim.model.temporal         :as temporal]
            [com.yetanalytics.datasim.xapi.actor             :as actor]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::alignment/weights
  (s/map-of ::xs/iri ::random/weight))

(s/def ::alignment.period/min int?)
(s/def ::alignment.period/mean pos-int?)

(s/def ::alignment/periods
  (s/map-of ::xs/iri (s/keys :req-un [::alignment.period/min
                                      ::alignment.period/mean])))

(s/def ::alignments
  (s/keys :opt-un [::alignment/weights
                   ::alignment/periods]))

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

(defn- mapify-alignments
  [alignments]
  {:weights (reduce (fn [acc {:keys [id weight]}]
                      (if (some? weight)
                        (assoc acc id weight)
                        acc))
                    {}
                    alignments)
   :bounds  (reduce (fn [acc {:keys [id bounds]}]
                      (assoc acc id (temporal/convert-bounds bounds)))
                    {}
                    alignments)
   :periods (reduce (fn [acc {:keys [id period]}]
                      (assoc acc id (temporal/convert-period period)))
                    {}
                    alignments)})

(defn- mapify-object-overrides
  [object-overrides]
  {:weights (reduce (fn [m {:keys [weight object]}]
                      (assoc m object weight))
                    {}
                    object-overrides)
   :objects (map :object object-overrides)})

(s/fdef models->map
  :args (s/cat :models ::model/models)
  :ret model-map-spec)

(defn models->map
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
  [{:keys [default-model agent-models group-models role-models]}
   agent-id
   group-id
   role-id]
  ;; TODO: Figure out personae precedence
  (or (get agent-models agent-id)
      (get group-models group-id)
      (get role-models role-id)
      default-model))
