(ns com.yetanalytics.datasim.input.temporal
  (:require [clojure.set                 :as cset]
            [clojure.spec.alpha          :as s]
            [com.yetanalytics.pan.axioms :as ax]
            [com.yetanalytics.datasim.input.temporal.delay :as-alias delay]
            [com.yetanalytics.datasim.input.temporal.guard :as-alias guard]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interval
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
  (s/or :integer day-of-week-spec*
        :string  (s/and (s/conformer day-of-week-map
                                     (cset/map-invert day-of-week-map))
                        day-of-week-spec*)))

(def day-of-month-spec (s/int-in 0 31))

(def month-of-year-spec* (s/int-in 0 12))

(def month-of-year-spec
  (s/or :integer month-of-year-spec*
        :string  (s/and (s/conformer month-of-year-map
                                     (cset/map-invert month-of-year-map))
                        month-of-year-spec*)))

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
;; Delay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::delay/mean (s/double-in :min 0 :infinite? false :NaN? false))

(s/def ::delay/sd (s/double-in :min 0 :infinite? false :NaN? false))

(s/def ::delay/unit #{"ms" "second" "minute" "hour" "day" "week" "month"})

(s/def ::delay
  (s/keys :req-un [::delay/mean
                   ::delay/unit]
          :opt-un [::delay/sd]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Temporal Spec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::id ::ax/iri)

(s/def ::temporal
  (s/keys :req-un [::id]
          :opt-un [::guards
                   ::delay]))
