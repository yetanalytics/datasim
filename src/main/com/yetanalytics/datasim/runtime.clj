(ns com.yetanalytics.datasim.runtime
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.sim :as sim]
            [com.yetanalytics.datasim.input :as input]
            [cheshire.core :as json]))

;; Simple, single-thread impl for now
;; Gets the statement seqs and combines them, writes to out
(s/fdef run-sim!
  :args (s/cat :input :com.yetanalytics.datasim/input)
  :ret nil?)

(defn run-sim! [input & rest-args]
  (doseq [s (apply sim/sim-seq input rest-args)]
    (json/generate-stream s *out*)
    (.write *out* "\n")
    (flush)))


(comment
  (def i
    (update (input/from-location :input :json "dev-resources/input/simple.json")
            :parameters
            merge
            {:start "2019-11-18T11:38:39.219768Z",
             :end nil #_"2019-11-19T11:38:39.219768Z"
             :timezone "America/New_York",
             :seed 42,
             :from "2019-11-18T11:38:39.219768Z"}))


  (-> (sim/sim-seq i)
      (nth 100)
      time)


  (run-sim! i)




  )
