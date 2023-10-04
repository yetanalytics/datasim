(ns com.yetanalytics.datasim.model.periods
  (:require [clojure.spec.alpha :as s]
            [java-time.api      :as t]
            [com.yetanalytics.datasim.input.model.alignments :as align]
            [com.yetanalytics.datasim.util.random            :as random]
            [com.yetanalytics.datasim.model.bounds           :as bounds]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::min nat-int?)

(s/def ::mean pos-int?)

(s/def ::fixed pos-int?)

(s/def ::period
  (s/or :variable (s/keys :req-un [::min ::mean]
                          :opt-un [::bounds/bounds])
        :fixed (s/keys :req-un [::fixed]
                       :opt-un [::bounds/bounds])))

(s/def ::periods
  (s/every ::period))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- convert-time
  "Convert time `t` into milliseconds based on the time `unit`. Coerces
   any doubles into integers."
  [t unit]
  (long (case unit
          :millis  t
          :seconds (* t ms-per-second)
          :minutes (* t ms-per-minute)
          :hours   (* t ms-per-hour)
          :days    (* t ms-per-day)
          :weeks   (* t ms-per-week))))

(defn- convert-period
  [{:keys [min mean fixed unit bounds]}]
  (let [unit*   (or (some-> unit keyword) :minutes)
        mean*   (or (some-> mean (convert-time unit*)) ms-per-minute)
        min*    (or (some-> min (convert-time unit*)) 0)
        fixed*  (some-> fixed (convert-time unit*))
        bounds* (some-> bounds bounds/convert-bounds)]
    (cond-> {}
      fixed*       (assoc :fixed fixed*)
      (not fixed*) (assoc :min min*
                          :mean mean*)
      bounds*      (assoc :bounds bounds*))))

(s/fdef convert-periods
  :args (s/cat :periods ::align/periods)
  :ret ::periods)

(defn convert-periods
  [periods]
  (mapv convert-period periods))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- generate-period
  [rng {:keys [mean min fixed]}]
  (or fixed
      (let [rate   (/ 1.0 mean)
            t-diff (long (random/rand-exp rng rate))]
        (+ min t-diff))))

(defn- add-period
  [date-time rng period]
  (t/plus date-time (t/millis (generate-period rng period))))

(s/fdef add-periods
  :args (s/cat :date-time t/local-date-time?
               :rng       ::rng
               :periods   ::periods)
  :ret t/local-date-time?)

(defn add-periods
  "Add a random amount of milliseonds to `date-time` based on the first map of
   valid parameter map in `periods`. A valid map either has no time bounds or
   bounds that satisfy `date-time`. The millis amount is an exponentially
   distributed random variable with `period` parameters `:mean` and `:min`.
   The generated sequence that uses these periodic date-times will thus occur
   as a Poisson random process."
  [date-time rng periods]
  (let [periods    (or periods
                       [{:min  0
                         :mean ms-per-minute}])
        some-bound (fn [{:keys [bounds] :as period}]
                     (when (bounds/bounded-time? bounds date-time) period))]
    (if-some [period (some some-bound periods)]
      (add-period date-time rng period)
      (throw (ex-info "Timestamp does not satisfy any period `bounds`; no default period present."
                      {:type      ::outside-period-bound
                       :periods   periods
                       :date-time date-time})))))
