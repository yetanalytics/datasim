(ns com.yetanalytics.datasim.input.model.alignments
  (:require [clojure.set        :as cset]
            [clojure.spec.alpha :as s]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.datasim.input.model.alignments.bound :as-alias bound]
            [com.yetanalytics.datasim.input.model.alignments.delay :as-alias delay]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Weight
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See also: ::object-override/weight

(s/def ::weight
  (s/double-in :min 0.0 :max 1.0 :infinite? false :NaN? false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time Bounds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- interval? [[start end]]
  (< start end))

(defmacro bound-spec [scalar-spec]
  `(s/every (s/or :scalar   ~scalar-spec
                  :interval (s/and (s/tuple ~scalar-spec ~scalar-spec)
                                   interval?))
            :kind vector?
            :min-count 1
            :gen-max 3))

(defmacro named-time-spec [index-spec name-index-map]
  `(s/and
    (s/or :integer ~index-spec
          :string  (s/and (set (keys ~name-index-map))
                          (s/conformer ~name-index-map
                                       (cset/map-invert ~name-index-map))
                          ~index-spec))
    ;; Remove s/or tags
    (s/conformer second)))

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

(def day-of-week-spec (named-time-spec day-of-week-spec* day-of-week-map))

(def day-of-month-spec (s/int-in 0 31))

(def month-of-year-spec* (s/int-in 0 12))

(def month-of-year-spec (named-time-spec month-of-year-spec* month-of-year-map))

(def year-spec pos-int?)

(s/def ::bound/second (bound-spec second-spec))

(s/def ::bound/minute (bound-spec minute-spec))

(s/def ::bound/hour (bound-spec hour-spec))

(s/def ::bound/day-of-week (bound-spec day-of-week-spec))

(s/def ::bound/day-of-month (bound-spec day-of-month-spec))

(s/def ::bound/month (bound-spec month-of-year-spec))

(s/def ::bound/year (bound-spec year-spec))

(s/def ::timeBounds
  (s/every (s/keys :opt-un [::bound/second
                            ::bound/minute
                            ::bound/hour
                            ::bound/day-of-week
                            ::bound/day-of-month
                            ::bound/month
                            ::bound/year])
           :kind vector?
           :min-count 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time Delay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private double-spec
  (s/double-in :min 0 :infinite? false :NaN? false))

(s/def ::delay/min double-spec)

(s/def ::delay/mean double-spec)

(s/def ::delay/max double-spec)

(s/def ::delay/sd double-spec)

(s/def ::delay/unit #{"ms" "second" "minute" "hour" "day" "week" "month"})

(defn- ordered-delay-values?
  [{:keys [min mean max]}]
  (cond
    (and min mean max) (<= min mean max)
    (and min mean)     (<= min mean)
    (and mean max)     (<= mean max)
    (and min max)      (<= min max)
    mean  true ; cannot have only min or only max
    :else false))

(s/def ::timeDelay
  (s/and (s/keys :req-un [::delay/unit]
                 :opt-un [::delay/min
                          ::delay/mean
                          ::delay/max
                          ::delay/sd])
         ordered-delay-values?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alignment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Use pan.axioms/iri once that has its own generator
(s/def ::id ::xs/iri)

(def alignment-spec
  (s/keys :req-un [::id]
          :opt-un [::weight
                   ::timeBounds
                   ::timeDelay]))

(def alignments-spec
  (s/every alignment-spec :kind vector? :min-count 1))
