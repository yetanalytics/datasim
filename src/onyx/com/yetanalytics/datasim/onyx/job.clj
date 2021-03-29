(ns com.yetanalytics.datasim.onyx.job
  (:require [com.yetanalytics.datasim.onyx.seq :as dseq]
            [com.yetanalytics.datasim.util.xapi :as xapiu]
            [com.yetanalytics.datasim.onyx.util :as u]
            [com.yetanalytics.datasim.onyx.http :as http]
            [cheshire.core :as json]
            [taoensso.timbre :as log]))

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

;; TODO: reenable and merge LRS functionality after s3 is good
#_(defn config
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





(defn noop
  [seg]
  (println :segment-out seg))


;; TODO this works great up to a couple mil but then the comms overhead is too much
(defn config
  "Build a config for distributing generation and post of DATASIM simulations
  Target s3"
  [{:keys [;; SIM
           input-json ;; TODO: change to url or file on s3
           strip-ids?
           remove-refs?
           override-max
           ;; JOB
           gen-concurrency
           gen-batch-size
           out-ratio

           in-batch-size
           in-batch-timeout
           out-batch-size
           out-batch-timeout
           ;; S3
           s3-bucket
           s3-prefix
           s3-prefix-separator
           s3-encryption
           s3-max-concurrent-uploads


           ]
    :or {gen-concurrency 1
         gen-batch-size 1
         out-ratio 8
         in-batch-size 20
         in-batch-timeout 50
         out-batch-size 20
         out-batch-timeout 50
         strip-ids? false
         remove-refs? false}}]

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
                    (max 1 (quot ?max (count agent-parts))))
        part-input-json
        (if ?part-max
          (override-max! input-json ?part-max)
          input-json)

        in-names (map
                  #(keyword (format "in-%d" %))
                  (range (count agent-parts)))

        ;; The 8-1 ratio seems to be most performant
        out-task-count (max (quot (count agent-parts)
                                  out-ratio)
                            1) ;; but there should be at least one!
        out-names (map
                   #(keyword (format "out-%d" %))
                   (range out-task-count))]
    (reduce
     (partial merge-with into)
     {:workflow (into []
                      (map vector
                           in-names
                           (cycle out-names)))
      :lifecycles []
      :catalog []
      :task-scheduler :onyx.task-scheduler/balanced}
     (concat
      (map
       (fn [out-name]
         {:lifecycles [{:lifecycle/task out-name
                        :lifecycle/calls :onyx.plugin.s3-output/s3-output-calls}]
          :catalog [#_{:onyx/name out-name
                       :onyx/fn ::noop
                       :onyx/plugin :onyx.peer.function/function
                       :onyx/medium :function
                       :onyx/type :output
                       :onyx/n-peers 1
                       :onyx/batch-size out-batch-size
                       :onyx/batch-timeout out-batch-timeout}
                    {:onyx/name out-name
                     :onyx/plugin :onyx.plugin.s3-output/output
                     :s3/bucket s3-bucket
                     :s3/encryption s3-encryption
                     :s3/serializer-fn ::u/batch->smile
                     :s3/key-naming-fn :onyx.plugin.s3-output/default-naming-fn ;; TODO FIXX
                     :s3/prefix s3-prefix
                     :s3/prefix-separator s3-prefix-separator
                     :s3/serialize-per-element? false
                     :s3/max-concurrent-uploads s3-max-concurrent-uploads
                     :onyx/type :output
                     :onyx/medium :s3
                     :onyx/n-peers 1
                     :onyx/batch-size out-batch-size
                     :onyx/batch-timeout out-batch-timeout
                     :onyx/doc "Writes segments to s3 files, one file per batch"}
                    ]})
       out-names)
      (map
       (fn [in-name ids]
         {:lifecycles [(cond-> {:lifecycle/task in-name
                                :lifecycle/calls ::dseq/in-calls
                                ::dseq/input-json part-input-json
                                ::dseq/strip-ids? strip-ids?
                                ::dseq/remove-refs? remove-refs?
                                ::dseq/select-agents (set ids)
                                ::dseq/batch-size gen-batch-size}
                         ?part-max (assoc ::dseq/take-n ?part-max))
                       {:lifecycle/task in-name
                        :lifecycle/calls :onyx.plugin.seq/reader-calls}]
          :catalog [{:onyx/name in-name
                     :onyx/plugin :onyx.plugin.seq/input
                     :onyx/type :input
                     :onyx/medium :seq
                     :seq/checkpoint? false
                     :onyx/batch-size in-batch-size
                     :onyx/n-peers 1
                     :onyx/doc (format "Reads segments from seq for partition %s" in-name)}
                    ]})
       in-names
       agent-parts)))))


(defn colo-configs
  "Build one or more (gen-concurrency / out-ratio) job configs for distributing generation and post of DATASIM simulations
  Specified to run in a single peeer group
  Ensure that cluster has at least gen-concurrency/out-ratio peer groups (nodes)
  Ensure that each peer group has exactly out-ratio + 1 virtual peers"
  [{:keys [;; SIM
           input-json
           strip-ids?
           remove-refs?
           override-max
           ;; JOB
           gen-concurrency
           gen-batch-size
           out-ratio

           in-batch-size
           in-batch-timeout
           out-batch-size
           out-batch-timeout
           ;; S3
           s3-bucket
           s3-prefix
           s3-prefix-separator
           s3-encryption
           s3-max-concurrent-uploads


           ]
    :or {gen-concurrency 1
         gen-batch-size 1
         out-ratio 8
         in-batch-size 20
         in-batch-timeout 50
         out-batch-size 20
         out-batch-timeout 50
         strip-ids? false
         remove-refs? false}}]

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
                    (max 1 (quot ?max (count agent-parts))))
        part-input-json
        (if ?part-max
          (override-max! input-json ?part-max)
          input-json)

        out-task-count (max (quot (count agent-parts)
                                  out-ratio)
                            1) ;; but there should be at least one!
        #_#_#_#_
        out-names (map
                   #(keyword (format "out-%d" %))
                   (range out-task-count))

        in-names (map
                  #(keyword (format "in-%d" %))
                  (range (count agent-parts)))

        out-range (range out-task-count)]
    (for [[out-idx
           ins] (map vector
                     out-range
                     (partition-all
                      out-ratio
                      (map-indexed vector
                                   agent-parts)))
          :let [out-name (keyword (format "out-%d" out-idx))]]
      (reduce
       (partial merge-with into)
       {:lifecycles [{:lifecycle/task out-name
                      :lifecycle/calls :onyx.plugin.s3-output/s3-output-calls}]
        :catalog [#_{:onyx/name out-name
                     :onyx/fn ::noop
                     :onyx/plugin :onyx.peer.function/function
                     :onyx/medium :function
                     :onyx/type :output
                     :onyx/n-peers 1
                     :onyx/batch-size out-batch-size
                     :onyx/batch-timeout out-batch-timeout}
                  {:onyx/name out-name
                   :onyx/plugin :onyx.plugin.s3-output/output
                   :s3/bucket s3-bucket
                   :s3/encryption s3-encryption
                   :s3/serializer-fn ::u/batch->smile
                   :s3/key-naming-fn :onyx.plugin.s3-output/default-naming-fn ;; TODO FIXX
                   :s3/prefix s3-prefix
                   :s3/prefix-separator s3-prefix-separator
                   :s3/serialize-per-element? false
                   :s3/max-concurrent-uploads s3-max-concurrent-uploads
                   :onyx/type :output
                   :onyx/medium :s3
                   :onyx/n-peers 1 ;; will be ignored, but hey
                   :onyx/batch-size out-batch-size
                   :onyx/batch-timeout out-batch-timeout
                   :onyx/doc "Writes segments to s3 files, one file per batch"}
                  ]
        :task-scheduler :onyx.task-scheduler/colocated}
       (map
        (fn [[in-idx ids]]
          (let [in-name (keyword (format "in-%d" in-idx))]
            {:workflow [[in-name out-name]]
             :lifecycles [(cond-> {:lifecycle/task in-name
                                   :lifecycle/calls ::dseq/in-calls
                                   ::dseq/input-json part-input-json
                                   ::dseq/strip-ids? strip-ids?
                                   ::dseq/remove-refs? remove-refs?
                                   ::dseq/select-agents (set ids)
                                   ::dseq/batch-size gen-batch-size}
                            ?part-max (assoc ::dseq/take-n ?part-max))
                          {:lifecycle/task in-name
                           :lifecycle/calls :onyx.plugin.seq/reader-calls}]
             :catalog [{:onyx/name in-name
                        :onyx/plugin :onyx.plugin.seq/input
                        :onyx/type :input
                        :onyx/medium :seq
                        :seq/checkpoint? false
                        :onyx/batch-size in-batch-size
                        :onyx/n-peers 1
                        :onyx/doc (format "Reads segments from seq for partition %s" in-name)}
                       ]}))
        ins)))))
