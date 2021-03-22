(ns com.yetanalytics.datasim.onyx.job-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.onyx.job :refer :all]))

(deftest mom-partition-test
  (let [{:keys [workflow
                lifecycles
                catalog]
         :as job-config} (config
                          {:input-json (slurp "dev-resources/input/mom.json")
                           :concurrency 4
                           :batch-size 10
                           :lrs {:endpoint "http://localhost:8000/xapi"
                                 :batch-size 1000
                                 :username "foo"
                                 :password "bar"}})

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
         :concurrency 4
         :batch-size 10
         :lrs {:endpoint "http://localhost:8000/xapi"
               :batch-size 1000
               :username "foo"
               :password "bar"}})
       (update :lifecycles
               (fn [ls]
                 (mapv (fn [lc]
                        (if (:com.yetanalytics.datasim.onyx.seq/input-json lc)
                          (assoc lc :com.yetanalytics.datasim.onyx.seq/input-json "<json>")
                          lc))
                      ls)))))
  )
