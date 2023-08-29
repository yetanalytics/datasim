(ns com.yetanalytics.datasim.model
  (:require [clojure.spec.alpha :as s]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.datasim.input.model            :as model]
            [com.yetanalytics.datasim.input.model.alignments :refer [day-of-week-map month-of-year-map]]
            [com.yetanalytics.datasim.math.random            :as random]
            [com.yetanalytics.datasim.model.alignment        :as-alias alignment]
            [com.yetanalytics.datasim.model.alignment.period :as-alias alignment.period]
            [com.yetanalytics.datasim.model.object-override  :as-alias obj-override]
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

(def ms-per-second
  1000)

(def ms-per-minute
  60000)

(def ms-per-hour
  3600000)

(def ms-per-day
  86400000)

(def ms-per-week
  604800000)

(defn- convert-time
  "Convert time `t` into milliseconds based on the time `unit`. Coerces
   any doubles into integers."
  [t unit]
  (long (case unit
          :millisecond t
          :second (* t ms-per-second)
          :minute (* t ms-per-minute)
          :hour   (* t ms-per-hour)
          :day    (* t ms-per-day)
          :week   (* t ms-per-week))))

(defn- convert-time-period
  [{:keys [min mean unit]}]
  (let [unit* (or (some-> unit keyword) :minute)
        mean* (or (some-> mean (convert-time unit*)) ms-per-minute)
        min*  (or (some-> min (convert-time unit*)) 0)]
    {:min  min*
     :mean mean*}))

(defn- convert-time-bound-unit
  [unit v]
  (let [convert-dow   (fn [d]
                        (if (string? d) (get day-of-week-map d) d))
        convert-month (fn [m]
                        (if (string? m) (get month-of-year-map m) m))]
    (case unit
      :daysOfWeek
      (if (coll? v)
        (->> v (mapv convert-dow))
        (->> v convert-dow (repeat 2) vec))
      :months
      (if (coll? v)
        (->> v (mapv convert-month))
        (->> v convert-month (repeat 2) vec))
      (if (coll? v)
        v
        [v v]))))

(defn- time-bound-unit-camel->kebab
  [unit]
  (case unit
    :daysOfWeek :days-of-week
    :daysOfMonth :days-of-month
    unit))

(defn- convert-time-bounds
  [bounds]
  (mapv (fn [bound]
          (reduce-kv (fn [bound* unit v]
                       (assoc bound*
                              (time-bound-unit-camel->kebab unit)
                              (mapv (partial convert-time-bound-unit unit) v)))
                     {}
                     bound))
        bounds))

(defn- mapify-alignments
  [alignments]
  {:weights (reduce (fn [acc {:keys [id weight]}]
                      (if (some? weight)
                        (assoc acc id weight)
                        acc))
                    {}
                    alignments)
   :bounds  (reduce (fn [acc {:keys [id bounds]}]
                      (assoc acc id (convert-time-bounds bounds)))
                    {}
                    alignments)
   :periods (reduce (fn [acc {:keys [id period]}]
                      (assoc acc id (convert-time-period period)))
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
