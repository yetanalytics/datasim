(ns com.yetanalytics.datasim.util.maths
  "Common math utilities")

(defn min-max
  "given a minimum, maximum and a number, bound it."
  [min-n n max-n]
  (-> n
      (max min-n)
      (min max-n)))
