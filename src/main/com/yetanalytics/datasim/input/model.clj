(ns com.yetanalytics.datasim.input.model
  (:require [clojure.set                 :as cset]
            [clojure.spec.alpha          :as s]
            [clojure.spec.gen.alpha      :as sgen]
            [clojure.walk                :as w]
            [com.yetanalytics.pan.axioms :as ax]
            [com.yetanalytics.datasim.xapi.actor :as actor]
            [com.yetanalytics.datasim.input.model.agent :as-alias agent]
            [com.yetanalytics.datasim.input.model.group :as-alias group]
            [com.yetanalytics.datasim.input.model.role  :as-alias role]
            [com.yetanalytics.datasim.input.model.delay :as-alias delay]
            [com.yetanalytics.datasim.input.model.guard :as-alias guard]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personae
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti personae-spec :type)

(s/def ::agent/id ::actor/actor-ifi)
(s/def ::agent/type #{"Agent"})

(defmethod personae-spec "Actor" [_]
  (s/keys :req-un [::agent/id ::agent/type]))

(s/def ::group/id ::actor/actor-ifi)
(s/def ::group/type #{"Group"})

(defmethod personae-spec "Group" [_]
  (s/keys :req-un [::group/id ::group/type]))

(s/def ::role/id (s/and string? not-empty))
(s/def ::role/type #{"Role"})

(defmethod personae-spec "Role" [_]
  (s/keys :req-un [::role/id ::role/type]))

(s/def ::personae
  (s/every (s/multi-spec personae-spec :type)
           :kind vector?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Component/Object Override Weight
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::weight
  (s/double-in :min -1.0 :max 1.0 :infinite? false :NaN? false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Component Time Guard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- interval? [[start end]]
  (< start end))

(defmacro guard-spec [scalar-spec]
  `(s/every (s/or :scalar   ~scalar-spec
                  :interval (s/and (s/tuple ~scalar-spec ~scalar-spec)
                                   interval?))
            :kind vector?))

(def day-of-week-map
  {"Sunday"    0
   "Monday"    1
   "Tuesday"   2
   "Wednesday" 3
   "Thursday"  4
   "Friday"    5
   "Saturday"  6})

(def month-of-year-map
  {"January"   0
   "February"  1
   "March"     2
   "April"     3
   "May"       4
   "June"      5
   "July"      6
   "August"    7
   "September" 8
   "October"   9
   "November"  10
   "December"  11})

(def second-spec (s/int-in 0 60))

(def minute-spec (s/int-in 0 60))

(def hour-spec (s/int-in 0 24))

(def day-of-week-spec* (s/int-in 0 7))

(def day-of-week-spec
  (s/and
   (s/or :integer day-of-week-spec*
         :string  (s/and (s/conformer day-of-week-map
                                      (cset/map-invert day-of-week-map))
                         day-of-week-spec*))
   ;; Remove s/or tags
   (s/conformer second)))

(def day-of-month-spec (s/int-in 0 31))

(def month-of-year-spec* (s/int-in 0 12))

(def month-of-year-spec
  (s/and
   (s/or :integer month-of-year-spec*
         :string  (s/and (s/conformer month-of-year-map
                                      (cset/map-invert month-of-year-map))
                         month-of-year-spec*))
   ;; Remove s/or tags
   (s/conformer second)))

(def year-spec pos-int?)

(s/def ::guard/second (guard-spec second-spec))

(s/def ::guard/minute (guard-spec minute-spec))

(s/def ::guard/hour (guard-spec hour-spec))

(s/def ::guard/day-of-week (guard-spec day-of-week-spec))

(s/def ::guard/day-of-month (guard-spec day-of-month-spec))

(s/def ::guard/month (guard-spec month-of-year-spec))

(s/def ::guard/year (guard-spec year-spec))

(s/def ::guards
  (s/keys :opt-un [::guard/second
                   ::guard/minute
                   ::guard/hour
                   ::guard/day-of-week
                   ::guard/day-of-month
                   ::guard/month
                   ::guard/year]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Component Delay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::delay/mean (s/double-in :min 0 :infinite? false :NaN? false))

(s/def ::delay/sd (s/double-in :min 0 :infinite? false :NaN? false))

(s/def ::delay/unit #{"ms" "second" "minute" "hour" "day" "week" "month"})

(s/def ::delay
  (s/keys :req-un [::delay/mean
                   ::delay/unit]
          :opt-un [::delay/sd]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Component Properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::id ::ax/iri)

(def component-property-spec
  (s/keys :req-un [::id]
          :opt-un [::weight
                   ::guards
                   ::delay]))

(s/def ::componentProperties
  (s/every component-property-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object Override
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Due to limitations of keywords, we cannot have IRI keys, limiting extensions
(defn- no-iri-keys?
  "Returns false if there exists a key made from an IRI, e.g.
   (name :https://foo.org) => \"/foo.org\""
  [obj]
  (cond
    (map? obj)
    (if-not (->> obj vals (map no-iri-keys?) (some false?))
      (->> obj keys (some (partial re-matches #".*/.*")) not)
      false)
    (vector? obj)
    (every? no-iri-keys? obj)
    :else
    true))

(s/def ::object
  (s/with-gen (s/and (s/conformer w/stringify-keys w/keywordize-keys)
                     no-iri-keys?
                     :statement/object) ; from xapi-schema
    (fn [] ; TODO: More comprehensive gen
      (sgen/return
       {"id" "http://example.com/object-override"
        "objectType" "Activity"}))))

(def object-override-spec
  (s/keys :req-un [::object]
          :opt-un [::weight]))

(s/def ::objectOverrides
  (s/every object-override-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::id (s/nilable string?))

(def model-map-spec
  (s/keys :opt-un [::personae
                   ::componentProperties
                   ::objectOverrides]))

(defn- distinct-model-ids? [model-maps]
  (let [ids (map :id model-maps)]
    (= (-> ids count)
       (-> ids distinct count))))

(s/def ::model
  (s/and (s/every model-map-spec)
         distinct-model-ids?))
