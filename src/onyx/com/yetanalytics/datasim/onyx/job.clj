(ns com.yetanalytics.datasim.onyx.job
  (:require [com.yetanalytics.datasim.onyx.seq :as dseq]
            [com.yetanalytics.datasim.util.xapi :as xapiu]
            [com.yetanalytics.datasim.onyx.util :as u]
            [com.yetanalytics.datasim.onyx.http :as http]
            [cheshire.core :as json]))

(defn- override-max!
  [json-str mo]
  (json/generate-string
   (assoc-in (json/parse-string json-str) ["parameters" "max"] mo)))

(defn config
  "Build a config for distributing generation and post of DATASIM simulations"
  [{:keys [concurrency ;; how many simultaneous generators should run?
           batch-size ;; onyx batch size
           input-json
           lrs
           retry-params
           strip-ids?
           remove-refs?
           override-max]
    :or {concurrency 1 ;; everthing on one gen by default
         batch-size 10
         retry-params
         {:base-sleep-ms 200
          :max-sleep-ms 30000
          :max-total-sleep-ms 3600000}
         strip-ids? false
         remove-refs? false}}]

  (assert lrs "LRS must be provided")
  (assert input-json "Input JSON must be provided")

  (let [input-json (cond-> input-json
                     override-max (override-max! override-max))
        {{?max :max} :parameters ;; if there's a max param, get it for part-ing
         :as input} (u/parse-input input-json)
        actor-ids (map xapiu/agent-id
                       (get-in input [:personae :member]))
        _ (assert (<= concurrency (count actor-ids))
                  "Concurrency may not be higher than actor count")

        agent-parts (u/round-robin concurrency
                                   actor-ids)
        ?part-max (when ?max
                    (quot ?max (count agent-parts)))]
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
            :lifecycles [(cond-> {:lifecycle/task task-name
                                  :lifecycle/calls ::dseq/in-calls
                                  ::dseq/input-json input-json
                                  ::dseq/lrs lrs
                                  ::dseq/strip-ids? strip-ids?
                                  ::dseq/remove-refs? remove-refs?
                                  ::dseq/select-agents (set ids)}
                           ?part-max (assoc ::dseq/take-n ?part-max))
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
       agent-parts)
      [{:catalog [{:onyx/name :out
                   :onyx/plugin :onyx.plugin.http-output/output
                   :onyx/type :output
                   :onyx/medium :http
                   :http-output/success-fn ::http/post-success?
                   :http-output/retry-params retry-params
                   :onyx/batch-size batch-size
                   :onyx/doc "POST statements to http endpoint"}]}]))))

(defn config-2
  "Build a config for distributing generation and post of DATASIM simulations
  Attempt colocated pairs"
  [{:keys [concurrency ;; how many simultaneous generators should run?
           batch-size ;; onyx batch size
           input-json
           lrs
           retry-params
           strip-ids?
           remove-refs?
           override-max]
    :or {concurrency 1 ;; everthing on one gen by default
         batch-size 10
         retry-params
         {:base-sleep-ms 200
          :max-sleep-ms 30000
          :max-total-sleep-ms 3600000}
         strip-ids? false
         remove-refs? false}}]

  (assert lrs "LRS must be provided")
  (assert input-json "Input JSON must be provided")

  (let [input-json (cond-> input-json
                     override-max (override-max! override-max))
        {{?max :max} :parameters ;; if there's a max param, get it for part-ing
         :as input} (u/parse-input input-json)
        actor-ids (map xapiu/agent-id
                       (get-in input [:personae :member]))
        _ (assert (<= concurrency (count actor-ids))
                  "Concurrency may not be higher than actor count")

        agent-parts (u/round-robin concurrency
                                   actor-ids)
        ?part-max (when ?max
                    (quot ?max (count agent-parts)))]
    (reduce
     (partial merge-with into)
     {:workflow []
      :lifecycles []
      :catalog []
      :task-scheduler :onyx.task-scheduler/colocated
      }
     (map-indexed
       (fn [idx ids]
         (let [in-name (keyword (format "in-%d" idx))
               out-name (keyword (format "out-%d" idx))]
           {:workflow [[in-name out-name]]
            :lifecycles [(cond-> {:lifecycle/task in-name
                                  :lifecycle/calls ::dseq/in-calls
                                  ::dseq/input-json input-json
                                  ::dseq/lrs lrs
                                  ::dseq/strip-ids? strip-ids?
                                  ::dseq/remove-refs? remove-refs?
                                  ::dseq/select-agents (set ids)}
                           ?part-max (assoc ::dseq/take-n ?part-max))
                         {:lifecycle/task in-name
                          :lifecycle/calls :onyx.plugin.seq/reader-calls}]
            :catalog [{:onyx/name in-name
                       :onyx/plugin :onyx.plugin.seq/input
                       :onyx/type :input
                       :onyx/medium :seq
                       :seq/checkpoint? false
                       :onyx/batch-size batch-size
                       :onyx/n-peers 1
                       :onyx/doc (format "Reads segments from seq for partition %d" idx)}
                      {:onyx/name out-name
                       :onyx/plugin :onyx.plugin.http-output/output
                       :onyx/type :output
                       :onyx/medium :http
                       :http-output/success-fn ::http/post-success?
                       :http-output/retry-params retry-params
                       :onyx/batch-size batch-size
                       :onyx/n-peers 1
                       :onyx/doc "POST statements to http endpoint"}]}))
       agent-parts))))


(defn colo-configs
  "Return a collection of job configs given a DATASIM input
  Tasks use the colocated scheduler  "
  [{:keys [concurrency ;; how many simultaneous generators should run?
           batch-size ;; onyx batch size
           input-json
           lrs
           retry-params
           strip-ids?
           remove-refs?
           override-max]
    :or {concurrency 1 ;; everthing on one gen by default
         batch-size 10
         retry-params
         {:base-sleep-ms 200
          :max-sleep-ms 30000
          :max-total-sleep-ms 3600000}
         strip-ids? false
         remove-refs? false}}]

  (assert lrs "LRS must be provided")
  (assert input-json "Input JSON must be provided")

  (let [input-json (cond-> input-json
                     override-max (override-max! override-max))
        {{?max :max} :parameters ;; if there's a max param, get it for part-ing
         :as input} (u/parse-input input-json)
        actor-ids (map xapiu/agent-id
                       (get-in input [:personae :member]))
        _ (assert (<= concurrency (count actor-ids))
                  "Concurrency may not be higher than actor count")

        agent-parts (u/round-robin concurrency
                                   actor-ids)
        ?part-max (when ?max
                    (quot ?max (count agent-parts)))]
    (for [ids agent-parts]
      {:task-scheduler :onyx.task-scheduler/colocated
       :workflow [[:in :out]]
       :lifecycles [(cond-> {:lifecycle/task :in
                             :lifecycle/calls ::dseq/in-calls
                             ::dseq/input-json input-json
                             ::dseq/lrs lrs
                             ::dseq/strip-ids? strip-ids?
                             ::dseq/remove-refs? remove-refs?
                             ::dseq/select-agents (set ids)}
                      ?part-max (assoc ::dseq/take-n ?part-max))
                    {:lifecycle/task :in
                     :lifecycle/calls :onyx.plugin.seq/reader-calls}]
       :catalog [{:onyx/name :in
                  :onyx/plugin :onyx.plugin.seq/input
                  :onyx/type :input
                  :onyx/medium :seq
                  :seq/checkpoint? false
                  :onyx/batch-size batch-size
                  :onyx/n-peers 1
                  ;; :onyx/max-peers 1
                  :onyx/doc "Reads segments from seq for partition"}
                 {:onyx/name :out
                  :onyx/plugin :onyx.plugin.http-output/output
                  :onyx/type :output
                  :onyx/medium :http
                  :http-output/success-fn ::http/post-success?
                  :http-output/retry-params retry-params
                  :onyx/batch-size batch-size
                  :onyx/n-peers 1
                  ;; :onyx/max-peers 1
                  :onyx/doc "POST statements to http endpoint"}]})))
