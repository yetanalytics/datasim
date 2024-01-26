(ns com.yetanalytics.datasim.xapi.profile.activity
  "Creation of `activity-map` for Profile compilation."
  (:require [clojure.spec.alpha :as s]
            [clojure.string     :as cs]
            [clojure.walk       :as w]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.pan.objects.concepts.activity-type :as pan-at]
            [com.yetanalytics.pan.objects.concepts.activity      :as pan-a]
            [com.yetanalytics.datasim.util.random           :as random]
            [com.yetanalytics.datasim.xapi.profile          :as-alias profile]
            [com.yetanalytics.datasim.xapi.profile.template :as template]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::activity-map
  (s/map-of (s/nilable ::pan-at/id) ; not all activities have activity types
            (s/map-of ::pan-a/id ::xs/activity)))

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

;; It's somewhat inefficient to parse rules in two different places during
;; profile compilation, but it creates somewhat cleaner code.
(defn- template-rule-activity-types
  "Derive Activity Type IDs from the Template's Rules"
  [template]
  (->> template
       template/template->parsed-rules
       rules->activity-type-ids))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assoc Activity Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- profile->statement-activity
  [{:keys [id activityDefinition]}]
  (cond-> {"id" id}
    activityDefinition
    (assoc "definition"
           (-> activityDefinition (dissoc :_context) w/stringify-keys))))

(defn- assoc-activity
  "Associate `activity` to `activity-map`."
  [activity-map activity]
  (let [{activity-id :id
         {activity-type-id :type} :activityDefinition}
        activity]
    (assoc-in activity-map
              [activity-type-id activity-id]
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
        serial (random/rand-int rng Integer/MAX_VALUE)]
    (format "https://example.org/%s/%d" tag serial)))

(defn- assoc-activity-type-id
  "Create a new Activity from `activity-type-id` and associate it with
   `activity-map`. Use `rng` to generate a new Activity ID, while `min-per-type`
   limits the max number of Activities generated for `activity-type-id`."
  [rng min-per-type activity-map activity-type-id]
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
  :args (s/cat :type-iri-map ::profile/type-iri-map
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
