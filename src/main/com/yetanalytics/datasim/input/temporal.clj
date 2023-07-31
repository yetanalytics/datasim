(ns com.yetanalytics.datasim.input.temporal
  (:require
   [clojure.spec.alpha          :as s]
   [com.yetanalytics.pan.axioms :as ax]
   [com.yetanalytics.datasim.input.temporal.delay :as-alias delay]
   [com.yetanalytics.datasim.input.temporal.guard :as-alias guard]
   [com.yetanalytics.datasim.input.temporal.guard.second       :as-alias second]
   [com.yetanalytics.datasim.input.temporal.guard.minute       :as-alias minute]
   [com.yetanalytics.datasim.input.temporal.guard.hour         :as-alias hour]
   [com.yetanalytics.datasim.input.temporal.guard.day-of-week  :as-alias dow]
   [com.yetanalytics.datasim.input.temporal.guard.day-of-month :as-alias dom]
   [com.yetanalytics.datasim.input.temporal.guard.month        :as-alias month]
   [com.yetanalytics.datasim.input.temporal.guard.year         :as-alias year]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- interval? [{:keys [start end]}]
  (or (nil? start)
      (nil? end)
      (< start end)))

(defmacro guard-spec [scalar-spec interval-spec]
  `(s/or :scalar   ~scalar-spec
         :interval ~interval-spec
         :steps    (s/every (s/or :scalar  ~scalar-spec
                                  :interval ~interval-spec)
                            :kind vector?)))

;; Second

(def second-scalar (s/int-in 0 60))

(s/def ::second/start second-scalar)
(s/def ::second/end second-scalar)

(def second-interval
  (s/and (s/keys :opt-un [::second/start ::second/end])
         interval?))

(s/def ::guard/second (guard-spec second-scalar second-interval))

;; Minute

(def minute-scalar (s/int-in 0 60))

(s/def ::minute/start minute-scalar)
(s/def ::minute/end minute-scalar)

(def minute-interval
  (s/and (s/keys :opt-un [::minute/start ::minute/end])
         interval?))

(s/def ::guard/minute (guard-spec minute-scalar minute-interval))

;; Hour

(def hour-scalar (s/int-in 0 24))

(s/def ::hour/start hour-scalar)
(s/def ::hour/end hour-scalar)

(def hour-interval
  (s/and (s/keys :opt-un [::hour/start ::hour/end])
         interval?))

(s/def ::guard/hour (guard-spec hour-scalar hour-interval))

;; Day of Week

(def dow-scalar (s/int-in 0 7))

(s/def ::dow/start dow-scalar)
(s/def ::dow/end dow-scalar)

(def dow-interval
  (s/and (s/keys :opt-un [::dow/start ::dow/end])
         interval?))

(s/def ::guard/day-of-week (guard-spec dow-scalar dow-interval))

;; Day of Month

(def dom-scalar (s/int-in 0 31))

(s/def ::dom/start dom-scalar)
(s/def ::dom/end dom-scalar)

(def dom-interval
  (s/and (s/keys :opt-un [::dom/start ::dom/end])
         interval?))

(s/def ::guard/day-of-month (guard-spec dom-scalar dom-interval))

;; Month

(def month-scalar (s/int-in 0 12))

(s/def ::month/start month-scalar)
(s/def ::month/end month-scalar)

(def month-interval
  (s/and (s/keys :opt-un [::month/start ::month/end])
         interval?))

(s/def ::guard/month (guard-spec month-scalar month-interval))

;; Year

(def year-scalar nat-int?)

(s/def ::year/start year-scalar)
(s/def ::year/end year-scalar)

(def year-interval
  (s/and (s/keys :opt-un [::year/start ::year/end])
         interval?))

(s/def ::guard/year (guard-spec year-scalar year-interval))

(s/def ::guard
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
          :opt-un [::guard
                   ::delay]))
