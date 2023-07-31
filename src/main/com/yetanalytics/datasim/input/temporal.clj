(ns com.yetanalytics.datasim.input.temporal
  (:require [clojure.spec.alpha :as s]
            ;; For specs
            [com.yetanalytics.datasim.input.temporal.interval :as-alias interval]
            [com.yetanalytics.datasim.input.temporal.delay    :as-alias delay]))

(defmacro interval [max]
  (let [scalar-spec   `(s/int-in 0 ~max)
        interval-spec `(s/and (s/tuple (s/int-in 0 ~max) (s/int-in 0 ~max))
                              #(< (first %) (second %)))]
    `(s/or :scalar   ~scalar-spec
           :interval ~interval-spec
           :steps    (s/every (s/or :scalar   ~scalar-spec
                                    :interval ~interval-spec)
                              :kind vector?))))

(s/def ::interval/second (interval 60))

(s/def ::interval/minute (interval 60))

(s/def ::interval/hour (interval 24))

(s/def ::interval/day-of-week (interval 7))

(s/def ::interval/day-of-month (interval 31))

(s/def ::interval/month (interval 12))

(s/def ::interval/year (interval 9999))

(s/def ::interval
  (s/keys :opt-un [::interval/second
                   ::interval/minute
                   ::interval/hour
                   ::interval/day-of-week
                   ::interval/day-of-month
                   ::interval/month
                   ::interval/year]))

(s/def ::delay/mean (s/double-in :min 0 :infinite? false :NaN? false))

(s/def ::delay/sd (s/double-in :min 0 :infinite? false :NaN? false))

(s/def ::delay/unit #{"ms" "second" "minute" "hour" "day" "week" "month"})

(s/def ::delay
  (s/keys :req-un [::delay/mean
                   ::delay/unit]
          :opt-un [::delay/sd]))

(s/def ::temporal
  (s/keys :req-un [::interval
                   ::delay]))
