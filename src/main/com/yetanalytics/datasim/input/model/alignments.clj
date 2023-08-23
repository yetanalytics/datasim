(ns com.yetanalytics.datasim.input.model.alignments
  (:require [clojure.set        :as cset]
            [clojure.spec.alpha :as s]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.datasim.math.random :as random]
            [com.yetanalytics.datasim.input.model.alignments.bound :as-alias bound]
            [com.yetanalytics.datasim.input.model.alignments.delay :as-alias delay]))

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

(s/def ::bound/second
  (bound-spec (s/int-in 0 60)))

(s/def ::bound/minute
  (bound-spec (s/int-in 0 60)))

(s/def ::bound/hour
  (bound-spec (s/int-in 0 24)))

(s/def ::bound/day-of-week
  (bound-spec (named-time-spec (s/int-in 0 7) day-of-week-map)))

(s/def ::bound/day-of-month
  (bound-spec (s/int-in 0 31)))

(s/def ::bound/month
  (bound-spec (named-time-spec (s/int-in 0 12) month-of-year-map)))

(s/def ::bound/year
  (bound-spec pos-int?))

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

(s/def ::delay/unit
  #{"millisecond" "second" "minute" "hour" "day" "week"})

(s/def ::timeDelay
  (s/keys :opt-un [::delay/min
                   ::delay/mean
                   ::delay/unit]))

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
