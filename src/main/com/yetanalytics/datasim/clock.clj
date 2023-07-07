(ns com.yetanalytics.datasim.clock
  (:require [java-time.api :as t])
  (:import [java.time Clock Instant Period]))

(defn sim-instant
  "Given a numeric t value, return an instant in time."
  [t & {:keys [t-zero ;; the instant corresponding to T0
               step ;; :nanos, :micros, seconds, minutes, hours, days, weeks, months, years
               as ;; :inst, :long
               ]
        :or {t-zero Instant/EPOCH
             step :minutes
             as :inst}}]
  (cond-> (t/plus t-zero
                  ((case step
                     :nanos t/nanos
                     :micros t/micros
                     :seconds t/seconds
                     :minutes t/minutes
                     :hours t/hours
                     :days t/days
                     :weeks t/weeks
                     :months t/months
                     :years t/years)
                   t))
    (= as :long)
    t/to-millis-from-epoch))
