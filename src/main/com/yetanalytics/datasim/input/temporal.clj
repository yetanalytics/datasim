(ns com.yetanalytics.datasim.input.temporal
  (:require
   [clojure.spec.alpha :as s]
   ;; For specs
   [com.yetanalytics.datasim.input.temporal.delay    :as-alias delay]
   [com.yetanalytics.datasim.input.temporal.interval :as-alias interval]
   [com.yetanalytics.datasim.input.temporal.interval.second       :as-alias second]
   [com.yetanalytics.datasim.input.temporal.interval.minute       :as-alias minute]
   [com.yetanalytics.datasim.input.temporal.interval.hour         :as-alias hour]
   [com.yetanalytics.datasim.input.temporal.interval.day-of-week  :as-alias dow]
   [com.yetanalytics.datasim.input.temporal.interval.day-of-month :as-alias dom]
   [com.yetanalytics.datasim.input.temporal.interval.month        :as-alias month]
   [com.yetanalytics.datasim.input.temporal.interval.year         :as-alias year]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- range? [{:keys [start end]}]
  (or (nil? start)
      (nil? end)
      (< start end)))

(defmacro interval [scalar-spec range-spec]
  `(s/or :scalar ~scalar-spec
         :range  ~range-spec
         :steps  (s/every (s/or :scalar ~scalar-spec
                                :range  ~range-spec)
                          :kind vector?)))

;; Second

(def second-scalar (s/int-in 0 60))

(s/def ::second/start second-scalar)
(s/def ::second/end second-scalar)

(def second-range
  (s/and (s/keys :opt-un [::second/start ::second/end])
         range?))

(s/def ::interval/second (interval second-scalar second-range))

;; Minute

(def minute-scalar (s/int-in 0 60))

(s/def ::minute/start minute-scalar)
(s/def ::minute/end minute-scalar)

(def minute-range
  (s/and (s/keys :opt-un [::minute/start ::minute/end])
         range?))

(s/def ::interval/minute (interval minute-scalar minute-range))

;; Hour

(def hour-scalar (s/int-in 0 24))

(s/def ::hour/start hour-scalar)
(s/def ::hour/end hour-scalar)

(def hour-range
  (s/and (s/keys :opt-un [::hour/start ::hour/end])
         range?))

(s/def ::interval/hour (interval hour-scalar hour-range))

;; Day of Week

(def dow-scalar (s/int-in 0 7))

(s/def ::dow/start dow-scalar)
(s/def ::dow/end dow-scalar)

(def dow-range
  (s/and (s/keys :opt-un [::dow/start ::dow/end])
         range?))

(s/def ::interval/day-of-week (interval dow-scalar dow-range))

;; Day of Month

(def dom-scalar (s/int-in 0 31))

(s/def ::dom/start dom-scalar)
(s/def ::dom/end dom-scalar)

(def dom-range
  (s/and (s/keys :opt-un [::dom/start ::dom/end])
         range?))

(s/def ::interval/day-of-month (interval dom-scalar dom-range))

;; Month

(def month-scalar (s/int-in 0 12))

(s/def ::month/start month-scalar)
(s/def ::month/end month-scalar)

(def month-range
  (s/and (s/keys :opt-un [::month/start ::month/end])
         range?))

(s/def ::interval/month (interval month-scalar month-range))

;; Year

(def year-scalar nat-int?)

(s/def ::year/start year-scalar)
(s/def ::year/end year-scalar)

(def year-range
  (s/and (s/keys :opt-un [::year/start ::year/end])
         range?))

(s/def ::interval/year (interval year-scalar year-range))

(s/def ::interval
  (s/keys :opt-un [::interval/second
                   ::interval/minute
                   ::interval/hour
                   ::interval/day-of-week
                   ::interval/day-of-month
                   ::interval/month
                   ::interval/year]))

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

(s/def ::temporal
  (s/keys :req-un [::interval
                   ::delay]))
