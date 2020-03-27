(ns bench
  (:require [criterium.core :as c]
            [com.yetanalytics.datasim.sim :as sim]
            [com.yetanalytics.datasim.input :as input]
            [com.yetanalytics.datasim.util.sequence :as su]
            [clojure.data.json :as json]))


(def input
  (input/map->Input
   {:profiles [(input/from-location :profile :json "dev-resources/bench/calibration.jsonld")]
    :personae (input/from-location :personae :json "dev-resources/bench/actors.json")
    :alignments (input/from-location :alignments :json "dev-resources/bench/alignments.json")
    :parameters (input/from-location :parameters :json "dev-resources/bench/params.json")}))

(comment

  (c/with-progress-reporting
    (c/quick-bench (do (-> (sim/build-skeleton input)
                           vals
                           (->> (su/seq-sort
                                 (comp :timestamp-ms
                                       meta))
                                (take 1000)
                                (into [])))
                       nil)))

  )
