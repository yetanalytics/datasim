(ns com.yetanalytics.datasim.xapi.activity
  (:require [clojure.spec.alpha :as s]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.input :as input]
            [com.yetanalytics.datasim.iri :as iri]
            [com.yetanalytics.datasim.random :as random]
            [clojure.walk :as w]
            [clojure.set :as cset]
            [clojure.string :as cs]
            [com.yetanalytics.datasim.xapi.statement :as stmt]))

(s/def :cosmos/activity-type
  iri/iri-spec)

(s/def :cosmos.activity-type/activity-id
  iri/iri-spec)

;; the total cosmos of activities in the sim.
(s/def ::cosmos
  (s/map-of
   :cosmos/activity-type
   (s/map-of
    :cosmos.activity-type/activity-id
    ::xs/activity)))

(s/def :derive-cosmos/seed
  int?)

;; minimum possible activities per type
;; TODO: take a map so this can be customized from params
(s/def :derive-cosmos.options/min-per-type
  pos-int?)

(s/def ::activity-map
  (s/map-of iri/iri-spec (s/map-of iri/iri-spec ::xs/activity)))

(s/fdef derive-cosmos
  :args (s/cat :input :com.yetanalytics.datasim/input
               :seed :derive-cosmos/seed
               :options (s/keys* :opt-un [:derive-cosmos.options/min-per-type]))
  :ret ::cosmos)

(defn derive-cosmos
  "Given a datasim input and a seed, derive a cosmos of activities. Takes a seed
  to generate additional activities not specified"
  [{:keys [profiles]
    :as input}
   seed
   & {:keys [min-per-type]
      :or {min-per-type 1}}]
  (let [concepts (mapcat :concepts profiles)
        ;; get all activity types used across profiles
        all-activity-types
        (into #{}
              (concat
               ;; template mentions of activity type
               (mapcat
                (fn [{:keys [objectActivityType
                             rules]}]
                  (cond->> (for [{:keys [location
                                         any
                                         all
                                         ]} rules
                                 :when (= location
                                          "$.object.definition.type")
                                 iri (concat any all)]
                             iri)
                    objectActivityType
                    (cons objectActivityType)))
                (mapcat
                 :templates
                 profiles))
               ;; Activity types from concepts
               (map :id
                    (filter (comp (partial = "ActivityType")
                                  :type)
                            concepts))))

        cosmos-supplied
        (reduce
         (fn [m {:keys [id]
                 {activity-type :type
                  :as definition} :activityDefinition}]
           (assoc-in m
                     [activity-type id]
                     {"id" id
                      "definition"
                      (w/stringify-keys
                       (dissoc definition
                               :_context))}))
         {}
         (filter (comp (partial = "Activity")
                       :type)
                 concepts))

        #_uncovered-types #_(cset/difference
                         all-activity-types
                         (set (keys cosmos-supplied)))
        #__ #_(assert (empty? uncovered-types)
                  (format "Uncovered activity types: %s"
                          uncovered-types))
        ;; delay an RNG, as we might not need it
        rng-d (delay (random/seed-rng seed))]
    (reduce
     (fn [cosmos type-iri]
       (let [type-count (count (get cosmos type-iri))]
         (if (<= min-per-type type-count)
           cosmos
           (update cosmos
                   type-iri
                   (fnil into {})
                   (for [_ (range (- min-per-type
                                     type-count))
                         :let [;; make a possibly familiar little tag if we can
                               tag (or (re-matches
                                        #"[a-zA-Z0-9]*"
                                        (last (cs/split type-iri #"/")))
                                       "activity")
                               serial (random/rand-int* @rng-d Integer/MAX_VALUE)
                               activity-id (format "https://example.org/%s/%d"
                                                   tag serial)]]
                     [activity-id
                      {"id" activity-id
                       "definition" {"type" type-iri}}])))))
     cosmos-supplied
     all-activity-types)))

;; NEW ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn- template-rule-activity-types
  "Derive Activity Type IDs from the Template's Rules"
  [template]
  (->> template
       stmt/template->parsed-rules
       rules->activity-type-ids))

(s/def ::min-per-type pos-int?)

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
