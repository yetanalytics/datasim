(ns com.yetanalytics.datasim.xapi.activity
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as cs]
            [clojure.walk :as w]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.iri :as iri]
            [com.yetanalytics.datasim.random :as random]))

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
