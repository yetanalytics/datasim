(ns com.yetanalytics.datasim.onyx.job
  (:require [com.yetanalytics.datasim.onyx.seq :as dseq]
            [com.yetanalytics.datasim.util.xapi :as xapiu]
            [com.yetanalytics.datasim.onyx.util :as u]
            [com.yetanalytics.datasim.onyx.http :as http]))

(defn config
  "Build a config for distributing generation and post of DATASIM simulations"
  [{:keys [batch-size
           partition-size ;; How many actors per generator?
           input-json
           lrs]
    :or {batch-size 10
         partition-size 1 ;; one actor per partition
         }}]

  (assert lrs "LRS must be provided")
  (assert input-json "Input JSON must be provided")

  (let [input (u/parse-input input-json)
        actor-ids (map xapiu/agent-id
                       (get-in input [:personae :member]))

        _ (assert (<= partition-size (count actor-ids))
                  "Partition size may not be higher than actor count")]
    (reduce
     (partial merge-with into)
     {:workflow []
      :lifecycles []
      :catalog []
      :task-scheduler :onyx.task-scheduler/balanced}
     (concat
      (map-indexed
       (fn [idx ids]
         (let [task-name (keyword (format "in-%d" idx))]
           {:workflow [[task-name :out]]
            :lifecycles [{:lifecycle/task task-name
                          :lifecycle/calls ::dseq/in-calls
                          ::dseq/input-json input-json
                          ::dseq/lrs lrs}
                         {:lifecycle/task task-name
                          :lifecycle/calls :onyx.plugin.seq/reader-calls}]
            :catalog [{:onyx/name task-name
                       :onyx/plugin :onyx.plugin.seq/input
                       :onyx/type :input
                       :onyx/medium :seq
                       :seq/checkpoint? false
                       :onyx/batch-size batch-size
                       :onyx/max-peers 1
                       :onyx/doc (format "Reads segments from seq for partition %d" idx)}]}))
       (partition-all partition-size actor-ids))
      [{:catalog [{:onyx/name :out
                   :onyx/plugin :onyx.plugin.http-output/output
                   :onyx/type :output
                   :onyx/medium :http
                   :http-output/success-fn ::http/post-success?
                   :http-output/retry-params {:base-sleep-ms 200
                                              :max-sleep-ms 30000
                                              :max-total-sleep-ms 3600000}
                   :onyx/batch-size batch-size
                   :onyx/doc "POST statements to http endpoint"}]}]))))
