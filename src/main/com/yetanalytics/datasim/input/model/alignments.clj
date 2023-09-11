(ns com.yetanalytics.datasim.input.model.alignments
  (:require [clojure.set        :as cset]
            [clojure.spec.alpha :as s]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.datasim.math.random :as random]
            [com.yetanalytics.datasim.input.model.alignments.bounds :as-alias bounds]
            [com.yetanalytics.datasim.input.model.alignments.period :as-alias period]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Weight
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See also: ::object-overrides/weight

(s/def ::weight ::random/weight)

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
  {"January"   1
   "February"  2
   "March"     3
   "April"     4
   "May"       5
   "June"      6
   "July"      7
   "August"    8
   "September" 9
   "October"   10
   "November"  11
   "December"  12})

(s/def ::bounds/seconds
  (bound-spec (s/int-in 0 60)))

(s/def ::bounds/minutes
  (bound-spec (s/int-in 0 60)))

(s/def ::bounds/hours
  (bound-spec (s/int-in 0 24)))

(s/def ::bounds/daysOfWeek
  (bound-spec (named-time-spec (s/int-in 0 7) day-of-week-map)))

(s/def ::bounds/daysOfMonth
  (bound-spec (s/int-in 1 32)))

(s/def ::bounds/months
  (bound-spec (named-time-spec (s/int-in 1 13) month-of-year-map)))

(s/def ::bounds/years
  (bound-spec pos-int?))

(s/def ::bounds
  (s/every (s/keys :opt-un [::bounds/seconds
                            ::bounds/minutes
                            ::bounds/hours
                            ::bounds/daysOfWeek
                            ::bounds/daysOfMonth
                            ::bounds/months
                            ::bounds/years])
           :kind vector?
           :min-count 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time Period
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::period/min
  (s/and number? pos?))

(s/def ::period/mean
  (s/and number? pos? (comp not zero?)))

(s/def ::period/unit
  #{"millis" "seconds" "minutes" "hours" "days" "weeks"})

(s/def ::period
  (s/keys :opt-un [::period/min
                   ::period/mean
                   ::period/unit]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Max Repeat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::repeat-max
  pos-int?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alignment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Use pan.axioms/iri once that has its own generator
(s/def ::id ::xs/iri)

(def alignment-spec
  (s/keys :req-un [::id]
          :opt-un [::weight
                   ::bounds
                   ::period
                   ::repeat-max]))

(def alignments-spec
  (s/every alignment-spec :kind vector? :min-count 1))
