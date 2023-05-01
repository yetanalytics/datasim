(ns com.yetanalytics.datasim.clock
  (:require [java-time :as t])
  (:import [java.time Instant]))

(defn sim-instant
  "Given a numeric `t` value, return an instant in time. Accepts these kwargs:
   
   | Kwarg    | Description
   | ---      | ---
   | `t-zero` | The instant corresponding to T0.
   | `step`   | One of `:nanos`, `:micros`, `:seconds`, `:minutes`, `:hours`, `:days`, `:weeks`, `:months`, or `:years`
   | `as`     | One of `:inst` (return as java.time.Instant instance) or `:long` (return the number of ms as a long)."
  [t & {:keys [t-zero step as]
        :or {t-zero Instant/EPOCH
             step   :minutes
             as     :inst}}]
  (let [t->step (case step
                  :nanos t/nanos
                  :micros t/micros
                  :seconds t/seconds
                  :minutes t/minutes
                  :hours t/hours
                  :days t/days
                  :weeks t/weeks
                  :months t/months
                  :years t/years)]
    (cond-> (t/plus t-zero (t->step t))
      (= as :long)
      t/to-millis-from-epoch)))
