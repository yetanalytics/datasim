(ns com.yetanalytics.datasim.onyx.job
  (:require [com.yetanalytics.datasim.onyx.sim :as dsim]
            [com.yetanalytics.datasim.input :as input]
            [com.yetanalytics.datasim.xapi.actor :as actor]
            [com.yetanalytics.datasim.onyx.util :as u]
            [com.yetanalytics.datasim.onyx.http :as http]
            [cheshire.core :as json]
            [taoensso.timbre :as log]
            [clojure.string :as cs])
  (:import [java.time Instant]))

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

(defn output-naming-fn [{:keys [onyx.core/lifecycle-id]
                         :as event}]
  (format
   "%s_%s.json"
   (.toString (Instant/now))
   lifecycle-id))

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
           colo
           in-batch-size
           in-batch-timeout
           out-batch-size
           out-batch-timeout
           out-mode

           ;; LRS
           lrs-retry-params
           lrs

           ;; S3
           s3-bucket
           s3-prefix
           s3-prefix-separator
           s3-encryption
           s3-max-concurrent-uploads
           ]
    :or {gen-concurrency 1
         gen-batch-size 1
         out-mode :lrs
         lrs-retry-params {:base-sleep-ms 500
                           :max-sleep-ms 30000
                           :max-total-sleep-ms 3600000}

         s3-bucket ""
         s3-prefix ""
         s3-prefix-separator "/"
         s3-encryption :none
         s3-max-concurrent-uploads 16

         out-ratio 1
         colo true
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
        actor-ids (-> input
                      :personae-array
                      (->> (mapcat :member)
                           (map actor/actor-ifi)
                           distinct))
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
      :task-scheduler (if colo
                        :onyx.task-scheduler/semi-colocated
                        :onyx.task-scheduler/balanced)
      }
     (concat
      (map
       (fn [out-name]
         (case out-mode
           :lrs
           {:catalog [{:onyx/name out-name
                       :onyx/plugin :onyx.plugin.http-output/output
                       :onyx/type :output
                       :onyx/medium :http
                       :http-output/success-fn ::http/post-success?
                       :http-output/retry-params lrs-retry-params
                       :onyx/batch-size out-batch-size
                       :onyx/batch-timeout out-batch-timeout
                       :onyx/n-peers 1
                       :onyx/doc "POST statements to http endpoint"}]
            :lifecycles [{:lifecycle/task out-name
                          :lifecycle/calls ::http/out-calls
                          ::http/lrs-request (lrs-req lrs)}]}
           :s3
           {:catalog [{:onyx/name out-name
                       :onyx/plugin :onyx.plugin.s3-output/output
                       :s3/bucket s3-bucket
                       :s3/encryption s3-encryption
                       :s3/serializer-fn ::u/batch->json
                       :s3/key-naming-fn ::output-naming-fn
                       :s3/prefix s3-prefix
                       :s3/prefix-separator s3-prefix-separator
                       :s3/serialize-per-element? false
                       :s3/max-concurrent-uploads s3-max-concurrent-uploads
                       :s3/multi-upload true
                       :s3/prefix-key :task-prefix
                       :s3/content-type "application/json"
                       :onyx/type :output
                       :onyx/medium :s3
                       :onyx/n-peers 1
                       :onyx/batch-size out-batch-size
                       :onyx/batch-timeout out-batch-timeout
                       :onyx/doc "Writes segments to s3 files, one file per batch"}]
            :lifecycles [{:lifecycle/task out-name
                          :lifecycle/calls :onyx.plugin.s3-output/s3-output-calls}]}
           :noop
           {:catalog [{:onyx/name out-name
                       :onyx/fn ::noop
                       :onyx/plugin :onyx.peer.function/function
                       :onyx/medium :function
                       :onyx/type :output
                       :onyx/n-peers 1
                       :onyx/batch-size out-batch-size
                       :onyx/batch-timeout out-batch-timeout}]
            :lifecycles []}))
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
