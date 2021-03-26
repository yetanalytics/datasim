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

(defn- lrs-req
  [{:keys [endpoint
           x-api-key
           username
           password]
    :as lrs}]
  {:url (format "%s/statements" endpoint)
   :args
   (cond-> {:headers (cond-> {"X-Experience-API-Version" "1.0.3"
                              "Content-Type" "application/json"}
                       ;; Amazon API Gateway key support
                       x-api-key
                       (assoc "X-API-Key" x-api-key))
            :as :json}
     (and username password)
     (assoc :basic-auth [username password]))})

(defn config
  "Build a config for distributing generation and post of DATASIM simulations"
  [{:keys [gen-concurrency
           post-concurrency
           batch-size ;; onyx batch size
           input-json
           lrs
           retry-params
           strip-ids?
           remove-refs?
           override-max]
    :or {gen-concurrency 1
         post-concurrency 1
         batch-size 10
         retry-params
         {:base-sleep-ms 500
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
        _ (assert (<= gen-concurrency (count actor-ids))
                  "Gen concurrency may not be higher than actor count")

        agent-parts (u/round-robin gen-concurrency
                                   actor-ids)
        ?part-max (when ?max
                    (quot ?max (count agent-parts)))
        part-input-json
        (if ?part-max
          (override-max! input-json ?part-max)
          input-json)
        ;; form the basic LRS request
        lrs-request (lrs-req lrs)]
    (reduce
     (partial merge-with into)
     {:workflow []
      :lifecycles [{:lifecycle/task :out
                    :lifecycle/calls ::http/out-calls
                    ::http/lrs-request lrs-request}]
      :catalog [{:onyx/name :out
                 :onyx/plugin :onyx.plugin.http-output/output
                 :onyx/type :output
                 :onyx/medium :http
                 :http-output/success-fn ::http/post-success?
                 :http-output/retry-params retry-params
                 :onyx/batch-size batch-size
                 :onyx/n-peers post-concurrency
                 :onyx/doc "POST statements to http endpoint"}]
      :task-scheduler :onyx.task-scheduler/balanced
      }
     (map-indexed
       (fn [idx ids]
         (let [in-name (keyword (format "in-%d" idx))]
           {:workflow [[in-name :out]]
            :lifecycles [(cond-> {:lifecycle/task in-name
                                  :lifecycle/calls ::dseq/in-calls
                                  ::dseq/input-json part-input-json
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
                      ]}))
       agent-parts))))
