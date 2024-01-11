(ns bench
  (:require [criterium.core :as c]
            [com.yetanalytics.datasim.sim :as sim]
            [com.yetanalytics.datasim.input :as input]))

(defn statements-per-second
  "How many statements per second will datasim generate?"
  [input]
  (let [sim-seq (sim/sim-seq input)
        t-start (System/currentTimeMillis)
        t-end (+ t-start 1000)]
    (loop [[_ & rest-s] sim-seq
           s-count 0]
      (if (> t-end (System/currentTimeMillis))
        (recur rest-s (inc s-count))
        s-count))))

(comment
  (def input
    {:profiles [(input/from-location :profile :json "dev-resources/bench/calibration.jsonld")]
     :personae-array [(input/from-location :personae :json "dev-resources/bench/actors.json")]
     :models (input/from-location :models :json "dev-resources/bench/models.json")
     :parameters (input/from-location :parameters :json "dev-resources/bench/params.json")})

  (c/with-progress-reporting
    (c/quick-bench (do (doall (take 1000 (sim/sim-seq input)))
                       nil)))

  ;; Evaluation count : 6 in 6 samples of 1 calls.
  ;; Execution time mean : 201.061588 ms
  ;; Execution time std-deviation : 6.698801 ms
  ;; Execution time lower quantile : 196.857258 ms ( 2.5%)
  ;; Execution time upper quantile : 212.503809 ms (97.5%)
  ;; Overhead used : 7.430081 ns

  (statements-per-second input) ;; => 4938

  (quot (reduce + (repeatedly 60 #(statements-per-second input)))
        60) ;; => 4462

  )
