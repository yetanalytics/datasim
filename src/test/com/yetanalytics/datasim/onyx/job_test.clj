(ns com.yetanalytics.datasim.onyx.job-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.onyx.job :refer :all]))

(deftest mom-partition-test
  (let [{:keys [workflow
                lifecycles
                catalog]
         :as job-config} (config
                          {:input-loc "dev-resources/input/mom64.json"
                           :gen-concurrency 4})

        inputs (keep :com.yetanalytics.datasim.onyx.sim/input-loc lifecycles)
        parts (keep :com.yetanalytics.datasim.onyx.sim/select-agents
                    lifecycles)]
    (testing "Partitions 64 actors into 4 partitons of 16 agents each"
      (is (= 4 (count workflow)))
      (is (= 4 (count inputs)))
      (is (apply = 16 (map count parts))))))

(deftest config-test
  (testing "produces a valid job config from input"
    (is (= (config
            {:input-loc "dev-resources/input/simple.json"
             :gen-concurrency 3})
           {:workflow [[:in-0 :out-0] [:in-1 :out-1] [:in-2 :out-2]],
            :lifecycles
            [{:lifecycle/task :out-0,
              :lifecycle/calls :com.yetanalytics.datasim.onyx.http/out-calls,
              :com.yetanalytics.datasim.onyx.http/lrs-request
              {:url "null/statements",
               :args
               {:headers
                {"X-Experience-API-Version" "1.0.3",
                 "Content-Type" "application/json"},
                :as :json}}}
             {:lifecycle/task :out-1,
              :lifecycle/calls :com.yetanalytics.datasim.onyx.http/out-calls,
              :com.yetanalytics.datasim.onyx.http/lrs-request
              {:url "null/statements",
               :args
               {:headers
                {"X-Experience-API-Version" "1.0.3",
                 "Content-Type" "application/json"},
                :as :json}}}
             {:lifecycle/task :out-2,
              :lifecycle/calls :com.yetanalytics.datasim.onyx.http/out-calls,
              :com.yetanalytics.datasim.onyx.http/lrs-request
              {:url "null/statements",
               :args
               {:headers
                {"X-Experience-API-Version" "1.0.3",
                 "Content-Type" "application/json"},
                :as :json}}}
             {:lifecycle/task :in-0,
              :lifecycle/calls :com.yetanalytics.datasim.onyx.sim/in-calls,
              :com.yetanalytics.datasim.onyx.sim/input-loc
              "dev-resources/input/simple.json",
              :com.yetanalytics.datasim.onyx.sim/strip-ids? false,
              :com.yetanalytics.datasim.onyx.sim/remove-refs? false,
              :com.yetanalytics.datasim.onyx.sim/select-agents
              #{"mbox::mailto:bobfake@example.org"},
              :com.yetanalytics.datasim.onyx.sim/batch-size 1}
             {:lifecycle/task :in-1,
              :lifecycle/calls :com.yetanalytics.datasim.onyx.sim/in-calls,
              :com.yetanalytics.datasim.onyx.sim/input-loc
              "dev-resources/input/simple.json",
              :com.yetanalytics.datasim.onyx.sim/strip-ids? false,
              :com.yetanalytics.datasim.onyx.sim/remove-refs? false,
              :com.yetanalytics.datasim.onyx.sim/select-agents
              #{"mbox::mailto:frederstaz@example.org"},
              :com.yetanalytics.datasim.onyx.sim/batch-size 1}
             {:lifecycle/task :in-2,
              :lifecycle/calls :com.yetanalytics.datasim.onyx.sim/in-calls,
              :com.yetanalytics.datasim.onyx.sim/input-loc
              "dev-resources/input/simple.json",
              :com.yetanalytics.datasim.onyx.sim/strip-ids? false,
              :com.yetanalytics.datasim.onyx.sim/remove-refs? false,
              :com.yetanalytics.datasim.onyx.sim/select-agents
              #{"mbox::mailto:alicefaux@example.org"},
              :com.yetanalytics.datasim.onyx.sim/batch-size 1}],
            :catalog
            [{:http-output/retry-params
              {:base-sleep-ms 500,
               :max-sleep-ms 30000,
               :max-total-sleep-ms 3600000},
              :onyx/plugin :onyx.plugin.http-output/output,
              :onyx/medium :http,
              :onyx/batch-timeout 50,
              :onyx/type :output,
              :onyx/name :out-0,
              :onyx/n-peers 1,
              :onyx/doc "POST statements to http endpoint",
              :http-output/success-fn
              :com.yetanalytics.datasim.onyx.http/post-success?,
              :onyx/batch-size 1}
             {:http-output/retry-params
              {:base-sleep-ms 500,
               :max-sleep-ms 30000,
               :max-total-sleep-ms 3600000},
              :onyx/plugin :onyx.plugin.http-output/output,
              :onyx/medium :http,
              :onyx/batch-timeout 50,
              :onyx/type :output,
              :onyx/name :out-1,
              :onyx/n-peers 1,
              :onyx/doc "POST statements to http endpoint",
              :http-output/success-fn
              :com.yetanalytics.datasim.onyx.http/post-success?,
              :onyx/batch-size 1}
             {:http-output/retry-params
              {:base-sleep-ms 500,
               :max-sleep-ms 30000,
               :max-total-sleep-ms 3600000},
              :onyx/plugin :onyx.plugin.http-output/output,
              :onyx/medium :http,
              :onyx/batch-timeout 50,
              :onyx/type :output,
              :onyx/name :out-2,
              :onyx/n-peers 1,
              :onyx/doc "POST statements to http endpoint",
              :http-output/success-fn
              :com.yetanalytics.datasim.onyx.http/post-success?,
              :onyx/batch-size 1}
             #:onyx{:name :in-0,
                    :plugin :com.yetanalytics.datasim.onyx.sim/plugin,
                    :type :input,
                    :medium :seq,
                    :batch-size 1,
                    :n-peers 1,
                    :doc "Reads segments from seq for partition :in-0"}
             #:onyx{:name :in-1,
                    :plugin :com.yetanalytics.datasim.onyx.sim/plugin,
                    :type :input,
                    :medium :seq,
                    :batch-size 1,
                    :n-peers 1,
                    :doc "Reads segments from seq for partition :in-1"}
             #:onyx{:name :in-2,
                    :plugin :com.yetanalytics.datasim.onyx.sim/plugin,
                    :type :input,
                    :medium :seq,
                    :batch-size 1,
                    :n-peers 1,
                    :doc "Reads segments from seq for partition :in-2"}],
            :task-scheduler :onyx.task-scheduler/semi-colocated}))))

(comment

  ;; to gracefully print, useful for making more tests
  (clojure.pprint/pprint
   (config
    {:input-loc "dev-resources/input/simple.json"
     :gen-concurrency 3}))

  )
