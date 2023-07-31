(ns com.yetanalytics.datasim.input.temporal
  (:require [clojure.spec.alpha          :as s]
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

(s/def ::guard/second (guard-spec (s/int-in 0 60)))

(s/def ::guard/minute (guard-spec (s/int-in 0 60)))

(s/def ::guard/hour (guard-spec (s/int-in 0 24)))

(s/def ::guard/day-of-week (guard-spec (s/int-in 0 7)))

(s/def ::guard/day-of-month (guard-spec (s/int-in 0 31)))

(s/def ::guard/month (guard-spec (s/int-in 0 12)))

(s/def ::guard/year (guard-spec nat-int?))

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
