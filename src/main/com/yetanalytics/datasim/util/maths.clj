(ns com.yetanalytics.datasim.util.maths
  "Common math utilities")

(defn min-max
  "Given a minimum `min-n`, maximum `max-n` and a number `n`, bound it."
  [min-n n max-n]
  (-> n
      (max min-n)
      (min max-n)))
