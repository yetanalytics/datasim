(ns com.yetanalytics.datasim.xapi.activity
  (:require [clojure.spec.alpha :as s]
            [clojure.string     :as cs]
            [clojure.walk       :as w]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.datasim.math.random    :as random]
            [com.yetanalytics.datasim.xapi.statement :as stmt]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::activity-map
  (s/map-of ::xs/iri (s/map-of ::xs/iri ::xs/activity)))

(s/def ::min-per-type
  pos-int?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Template Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- template-property-activity-types
  "Derive Activity Type IDs from the Template's Determining Properties"
  [{:keys [objectActivityType
           contextCategoryActivityType
           contextGroupingActivityType
           contextParentActivityType
           contextOtherActivityType]}]
  (->> (concat [objectActivityType]
               contextCategoryActivityType
               contextGroupingActivityType
               contextParentActivityType
               contextOtherActivityType)
       (filter some?)))

(defn- rules->activity-type-ids
  [parsed-rules]
  (->> parsed-rules
       (map (fn [{:keys [spec valueset]}]
              (case spec
                :activity/definition
                (keep (fn [x] (when (map? x) (get x "type"))) valueset)
                :definition/type
                (seq valueset)
                ;; Else - even Activities and other values that can contain
                ;; Activity Types IRIs should not be included, since there
                ;; the Activity ID will also be included
                nil)))
       (mapcat identity)))

(defn- template-rule-activity-types
  "Derive Activity Type IDs from the Template's Rules"
  [template]
  (->> template
       stmt/template->parsed-rules
       rules->activity-type-ids))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assoc Activity Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- profile->statement-activity
  [activity]
  (-> activity
      (select-keys [:id :definition])
      (update-in [:definition] dissoc :_context)
      w/stringify-keys))

(defn- assoc-activity [activity-map activity]
  (let [{activity-id :id {activity-type-id :type} :definition} activity]
    (assoc-in activity-map
              [activity-id activity-type-id]
              (profile->statement-activity activity))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assoc Activity Type Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- activity-type->id
  [rng activity-type]
  (let [;; make a possibly familiar little tag if we can
        tag    (or (->> (cs/split activity-type #"/")
                        peek
                        (re-matches #"[a-zA-Z0-9]*"))
                   "activity")
        serial (random/rand-int* rng Integer/MAX_VALUE)]
    (format "https://example.org/%s/%d" tag serial)))

(defn- assoc-activity-type-id [rng min-per-type activity-map activity-type-id]
  (let [id->activity (fn [activity-id]
                       {"id"         activity-id
                        "definition" {"type" activity-type-id}})
        gen-activity (fn []
                       (->> activity-type-id
                            (activity-type->id rng)
                            id->activity))
        type-count   (count (get activity-map activity-type-id))
        activity-num (- min-per-type type-count)
        activities   (repeatedly activity-num gen-activity)]
    (reduce (fn [m* {activity-id "id" :as activity}]
              (assoc-in m*
                        [activity-type-id activity-id]
                        activity))
            activity-map
            activities)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Putting it all together
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef create-activity-map
  :args (s/cat :type-iri-map map? ; TODO: Better spec
               :seed int?
               :kwargs (s/keys* :opt-un [::min-per-type]))
  :ret ::activity-map)

(defn create-activity-map
  "Given a `type-iri-map` and `seed`, derive map from Activity Types to
   Activity IDs to the Activity objects. Finds Activities and Activity Type
   Concepts from `type-iri-map`, as well as additional Activity Types from
   Statement Templates; the latter is because unlike other values, Activity
   Types are commonly defined in Templates as Determining Properties or via
   rules.
   
   The `seed` is used generate additional Activities from Activity Types
   that are not specified as Concepts, while `min-per-type` specifies the
   minimum number of Activities that should be generated per Activity Type."
  [type-iri-map seed & {:keys [min-per-type] :or {min-per-type 1}}]
  (let [rng                 (random/seed-rng seed)
        concept-activities  (->> "Activity" (get type-iri-map) vals)
        concept-type-ids    (->> "ActivityType" (get type-iri-map) keys)
        statement-templates (->> "StatementTemplate" (get type-iri-map) vals)
        temp-prop-type-ids  (mapcat template-property-activity-types
                                    statement-templates)
        temp-rule-type-ids  (mapcat template-rule-activity-types
                                    statement-templates)
        activity-type-ids   (concat concept-type-ids
                                    temp-prop-type-ids
                                    temp-rule-type-ids)
        assoc-act-type-id   (partial assoc-activity-type-id rng min-per-type)
        reduce-act-type-ids (partial reduce assoc-act-type-id)
        reduce-activities   (partial reduce assoc-activity)]
    (-> {}
        (reduce-activities concept-activities)
        (reduce-act-type-ids activity-type-ids))))
