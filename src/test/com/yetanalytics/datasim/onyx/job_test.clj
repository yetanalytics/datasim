(ns com.yetanalytics.datasim.onyx.job-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.onyx.job :refer :all]))

(deftest mom-partition-test
  (let [{:keys [workflow
                lifecycles
                catalog]
         :as job-config} (config
                          {:input-json (slurp "dev-resources/input/mom.json")
                           :gen-concurrency 4
                           :gen-batch-size 3000
                           :out-concurrency 4
                           :out-batch-size 3000})

        inputs (keep :com.yetanalytics.datasim.onyx.seq/input-json lifecycles)
        parts (keep :com.yetanalytics.datasim.onyx.seq/select-agents
                    lifecycles)]
    (testing "Partitions 64 actors into 4 partitons of 16 agents each"
      (is (= 4 (count workflow)))
      (is (= 4 (count inputs)))
      (is (apply = 16 (map count parts))))))


(comment

  ;; to gracefully print, useful for making more tests
  (clojure.pprint/pprint
   (-> (config
        {:input-json (slurp "dev-resources/input/mom.json")
         :gen-concurrency 4
         :gen-batch-size 3000
         :out-concurrency 4
         :out-batch-size 3000})
       (update :lifecycles
               (fn [ls]
                 (mapv (fn [lc]
                        (if (:com.yetanalytics.datasim.onyx.seq/input-json lc)
                          (assoc lc :com.yetanalytics.datasim.onyx.seq/input-json "<json>")
                          lc))
                      ls)))))
  )
