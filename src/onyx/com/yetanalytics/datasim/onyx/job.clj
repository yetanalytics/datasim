(ns com.yetanalytics.datasim.onyx.job
  (:require [com.yetanalytics.datasim.onyx.sim :as dsim]
            [com.yetanalytics.datasim.input :as input]
            [com.yetanalytics.datasim.util.xapi :as xapiu]
            [com.yetanalytics.datasim.onyx.util :as u]
            [com.yetanalytics.datasim.onyx.http :as http]
            [cheshire.core :as json]
            [taoensso.timbre :as log]
            [clojure.string :as cs]))

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

(defn noop
  [seg]
  nil)

(defn output-naming-fn [{:keys [onyx.core/lifecycle-id
                                onyx.core/task
                                onyx.core/batch
                                onyx.core/job-id
                                onyx.core/tenancy-id]
                          :as event}]
  (let [range-start (-> batch first :range first)
        idx-start (-> batch first :chunk-idx)
        range-end (-> batch last :range peek)
        idx-end (-> batch last :chunk-idx)]
    (format
     "%s_%s_%s/%d-%d_%d-%d_%d"
     tenancy-id
     job-id
     (name task)
     idx-start
     idx-end
     range-start
     range-end
     (count (mapcat :statements batch)))))

(comment
  (output-naming-fn
   {
    :onyx.core/task :out-1
    :onyx.core/batch [{:range [0 0]
                       :chunk-idx 0
                       :statements [{}]}]
    :onyx.core/job-id (java.util.UUID/randomUUID)
    :onyx.core/tenancy-id "foo"}
   ) ;; => "foo_201ede1b-5696-496b-9cbf-983294bd5e82_out-1/0-0_0-0_1"

  )

;; TODO this works great up to a couple mil but then the comms overhead is too much
(defn config
  "Build a config for distributing generation and post of DATASIM simulations
  Target s3"
  [{:keys [;; SIM
           input-loc
           strip-ids?
           remove-refs?
           override-max
           ;; JOB
           gen-concurrency
           gen-batch-size
           out-ratio
           noop
           ;;percentage

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

           split-output

           ]
    :or {gen-concurrency 1
         gen-batch-size 1
         out-ratio 1
         ;; percentage 100
         in-batch-size 1
         in-batch-timeout 50
         out-batch-size 1
         out-batch-timeout 50
         strip-ids? false
         remove-refs? false
         split-output true}}]

  (assert input-loc "Input location must be provided")

  (let [{{?max :max} :parameters ;; if there's a max param, get it for part-ing
         :as input}
        (cond-> (input/from-location :input :json input-loc)
          override-max (u/override-max! override-max))
        actor-ids (map xapiu/agent-id
                       (get-in input [:personae :member]))
        _ (assert (<= gen-concurrency (count actor-ids))
                  "Gen concurrency may not be higher than actor count")

        agent-parts (u/round-robin gen-concurrency
                                   actor-ids)
        ?part-max (when ?max
                    (max 1 (quot ?max (count agent-parts))))
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
      :task-scheduler :onyx.task-scheduler/semi-colocated
      ;; :percentage percentage
      }
     (concat
      (map
       (fn [out-name]
         {:lifecycles (if noop
                        []
                        [{:lifecycle/task out-name
                          :lifecycle/calls :onyx.plugin.s3-output/s3-output-calls}])
          :catalog [(if noop
                      {:onyx/name out-name
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
                       :s3/serializer-fn ::u/batch->json
                       :s3/key-naming-fn (if split-output
                                           ::output-naming-fn
                                           :onyx.plugin.s3-output/default-naming-fn) ;; TODO FIXX
                       :s3/prefix s3-prefix
                       :s3/prefix-separator s3-prefix-separator
                       :s3/serialize-per-element? false
                       :s3/max-concurrent-uploads s3-max-concurrent-uploads
                       :onyx/type :output
                       :onyx/medium :s3
                       :onyx/n-peers 1
                       :onyx/batch-size out-batch-size
                       :onyx/batch-timeout out-batch-timeout
                       :onyx/doc "Writes segments to s3 files, one file per batch"})
                    ]})
       out-names)
      (map
       (fn [in-name ids]
         {:lifecycles [(cond-> {:lifecycle/task in-name
                                :lifecycle/calls ::dsim/in-calls
                                ::dsim/input-loc input-loc
                                ::dsim/strip-ids? strip-ids?
                                ::dsim/remove-refs? remove-refs?
                                ::dsim/select-agents (set ids)
                                ::dsim/batch-size gen-batch-size
                                }
                         ?part-max (assoc ::dsim/take-n ?part-max))]
          :catalog [{:onyx/name in-name
                     :onyx/plugin ::dsim/plugin
                     :onyx/type :input
                     :onyx/medium :seq
                     ;; :seq/checkpoint? false
                     :onyx/batch-size in-batch-size
                     :onyx/n-peers 1
                     :onyx/doc (format "Reads segments from seq for partition %s" in-name)}
                    ]})
       in-names
       agent-parts)))))
