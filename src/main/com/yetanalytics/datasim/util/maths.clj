(ns com.yetanalytics.datasim.util.maths
  "Common math utilities")

(defn min-max
  "given a minimum, maximum and a number, bound it."
  [min-n n max-n]
  (-> n
      (max min-n)
      (min max-n)))

(defn bound-probability
  "Bound `n` to between `0.0` and `1.0` to create a valid probability."
  [n]
  (min-max 0.0 n 1.0))
