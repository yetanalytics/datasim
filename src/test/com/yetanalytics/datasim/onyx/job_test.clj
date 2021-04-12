(ns com.yetanalytics.datasim.onyx.job-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.onyx.job :refer :all]))

(deftest mom-partition-test
  (let [{:keys [workflow
                lifecycles
                catalog]
         :as job-config} (config
                          {:input-loc "https://raw.githubusercontent.com/yetanalytics/datasim/DS-102_insanely_large_dataset/dev-resources/input/mom64.json"
                           :gen-concurrency 4
                           :gen-batch-size 3000
                           :out-concurrency 4
                           :out-batch-size 3000})

        inputs (keep :com.yetanalytics.datasim.onyx.sim/input-loc lifecycles)
        parts (keep :com.yetanalytics.datasim.onyx.sim/select-agents
                    lifecycles)]
    (testing "Partitions 64 actors into 4 partitons of 16 agents each"
      (is (= 4 (count workflow)))
      (is (= 4 (count inputs)))
      (is (apply = 16 (map count parts))))))


(comment

  ;; to gracefully print, useful for making more tests
  (clojure.pprint/pprint
   (config
    {:input-loc "https://raw.githubusercontent.com/yetanalytics/datasim/DS-102_insanely_large_dataset/dev-resources/input/mom64.json"
     :gen-batch-size 1000
     :gen-concurrency 4
     :out-batch-size 1000}))

  (require 'onyx.api)

  (require '[com.yetanalytics.datasim.onyx.config :as config])

  (def peer-config (:peer-config (config/get-config)))

  (let [id (java.util.UUID/randomUUID)
        {:as config} (config
                      {:input-loc "https://raw.githubusercontent.com/yetanalytics/datasim/DS-102_insanely_large_dataset/dev-resources/input/mom64.json"
                       :override-max 1000

                       :gen-concurrency 8
                       :gen-batch-size 250

                       })]
    (onyx.api/submit-job
     (assoc-in peer-config [ :onyx/tenancy-id] "local")
     config))

  )

#_(deftest config-test
  (testing "produces a valid job config from input"
    (is (= (-> (config
                {:input-json (slurp "dev-resources/input/simple.json")
                 :lrs {:endpoint "http://localhost:8000/xapi"
                       :batch-size 25
                       :username "foo"
                       :password "bar"}})
               ;; dissoc long icky json
               (update :lifecycles
                       (fn [ls]
                         (map
                          #(assoc % :com.yetanalytics.datasim.onyx.seq/input-json "<json>")
                          ls))))
           {:workflow [[:in-0 :out] [:in-1 :out] [:in-2 :out]],
            :lifecycles
            [{:lifecycle/task :in-0,
              :lifecycle/calls :com.yetanalytics.datasim.onyx.seq/in-calls,
              :com.yetanalytics.datasim.onyx.seq/input-json "<json>",
              :com.yetanalytics.datasim.onyx.seq/lrs
              {:endpoint "http://localhost:8000/xapi",
               :batch-size 25,
               :username "foo",
               :password "bar"},
              :com.yetanalytics.datasim.onyx.seq/strip-ids? false,
              :com.yetanalytics.datasim.onyx.seq/remove-refs? false}
             {:lifecycle/task :in-0,
              :lifecycle/calls :onyx.plugin.seq/reader-calls,
              :com.yetanalytics.datasim.onyx.seq/input-json "<json>"}
             {:lifecycle/task :in-1,
              :lifecycle/calls :com.yetanalytics.datasim.onyx.seq/in-calls,
              :com.yetanalytics.datasim.onyx.seq/input-json "<json>",
              :com.yetanalytics.datasim.onyx.seq/lrs
              {:endpoint "http://localhost:8000/xapi",
               :batch-size 25,
               :username "foo",
               :password "bar"},
              :com.yetanalytics.datasim.onyx.seq/strip-ids? false,
              :com.yetanalytics.datasim.onyx.seq/remove-refs? false}
             {:lifecycle/task :in-1,
              :lifecycle/calls :onyx.plugin.seq/reader-calls,
              :com.yetanalytics.datasim.onyx.seq/input-json "<json>"}
             {:lifecycle/task :in-2,
              :lifecycle/calls :com.yetanalytics.datasim.onyx.seq/in-calls,
              :com.yetanalytics.datasim.onyx.seq/input-json "<json>",
              :com.yetanalytics.datasim.onyx.seq/lrs
              {:endpoint "http://localhost:8000/xapi",
               :batch-size 25,
               :username "foo",
               :password "bar"},
              :com.yetanalytics.datasim.onyx.seq/strip-ids? false,
              :com.yetanalytics.datasim.onyx.seq/remove-refs? false}
             {:lifecycle/task :in-2,
              :lifecycle/calls :onyx.plugin.seq/reader-calls,
              :com.yetanalytics.datasim.onyx.seq/input-json "<json>"}],
            :catalog
            [{:onyx/name :in-0,
              :onyx/plugin :onyx.plugin.seq/input,
              :onyx/type :input,
              :onyx/medium :seq,
              :seq/checkpoint? false,
              :onyx/batch-size 10,
              :onyx/max-peers 1,
              :onyx/doc "Reads segments from seq for partition 0"}
             {:onyx/name :in-1,
              :onyx/plugin :onyx.plugin.seq/input,
              :onyx/type :input,
              :onyx/medium :seq,
              :seq/checkpoint? false,
              :onyx/batch-size 10,
              :onyx/max-peers 1,
              :onyx/doc "Reads segments from seq for partition 1"}
             {:onyx/name :in-2,
              :onyx/plugin :onyx.plugin.seq/input,
              :onyx/type :input,
              :onyx/medium :seq,
              :seq/checkpoint? false,
              :onyx/batch-size 10,
              :onyx/max-peers 1,
              :onyx/doc "Reads segments from seq for partition 2"}
             {:onyx/name :out,
              :onyx/plugin :onyx.plugin.http-output/output,
              :onyx/type :output,
              :onyx/medium :http,
              :http-output/success-fn
              :com.yetanalytics.datasim.onyx.http/post-success?,
              :http-output/retry-params
              {:base-sleep-ms 200,
               :max-sleep-ms 30000,
               :max-total-sleep-ms 3600000},
              :onyx/batch-size 10,
              :onyx/doc "POST statements to http endpoint"}],
            :task-scheduler :onyx.task-scheduler/balanced}))))
