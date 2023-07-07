(ns com.yetanalytics.datasim.client
  "Simple xAPI LRS client fns"
  (:require [clojure.core.async :as a]
            [clojure.java.io    :as io]
            [cheshire.core      :as json]
            [org.httpkit.client :as http])
  (:import [java.util UUID]))

(def default-http-options
  {:headers {"X-Experience-Api-Version" "1.0.3"
             "Content-Type" "application/json"}})

(defn- id->uuid [^String id]
  (UUID/fromString id))

(defn- decode-body [body]
  (with-open [rdr (io/reader body)]
    (json/decode-stream rdr)))

(defn- format-url [endpoint]
  (format "%s/statements" endpoint))

(defn- post-options [http-options batch]
  (merge default-http-options
         http-options
         {:body (json/encode batch)
          :as :stream}))

;; `http/post` cannot be resolved since it's defined using `http/defreq`
#_{:clj-kondo/ignore [:unresolved-var]}
(defn- post-batch
  ([endpoint http-options batch]
   (http/post (format-url endpoint)
              (post-options http-options batch)))
  ([endpoint http-options batch callback-fn]
   (http/post (format-url endpoint)
              (post-options http-options batch)
              callback-fn)))

(defn post-statements
  "Given LRS options and a statement seq, send them to an LRS in synchronous
   batches. If an `emit-ids-fn` is given it will be called with posted statement
   IDs on success."
  [{:keys [endpoint
           batch-size
           http-options]
    :or {batch-size 25}}
   statement-seq
   & {:keys [emit-ids-fn]}]
  ;; TODO: Exponential backoff, etc
  (loop [batches (partition-all batch-size statement-seq)
         success 0
         fail    []]
    (if-let [batch (first batches)]
      (let [{:keys [status body] :as response}
            @(post-batch endpoint http-options batch)]
        (if (= 200 status)
          ;; Success!
          ;; FIXME: Shouldn't other codes like 204 be supported?
          (let [statement-ids (map id->uuid (decode-body body))]
            (when emit-ids-fn
              (emit-ids-fn statement-ids))
            (recur (rest batches)
                   (+ success (count statement-ids))
                   fail))
          ;; Failure
          (let [response* (cond-> response
                            body (assoc :body (decode-body body)))]
            {:success success
             :fail    (conj fail response*)})))
      ;; Batch finished POSTing
      {:success success
       :fail    fail})))

(defn post-statements-async
  "Given LRS options and a channel with statements, send them to an LRS in
   asynchronous batches.

   Returns a channel that will reciveve `[:success <list of statement ids>]`
   for each batch or `[:fail <failing request>]`. Will stop sending on failure."
  [{:keys [endpoint
           batch-size
           http-options]
    :or {batch-size 25}}
   statement-chan
   & {:keys [concurrency
             buffer-in
             buffer-out]
      :or {concurrency 4
           buffer-in   100 ; 10x default batch size
           buffer-out  100}}]
  (let [run?     (atom true)
        in-chan  (a/chan buffer-in (partition-all batch-size))
        out-chan (a/chan buffer-out) ; is this.. backpressure?
        callback (fn [port {:keys [status body error] :as ret}]
                   (if (or (not= 200 status) error)
                     ;; Error: Stop further processing
                     (do
                       (swap! run? not)
                       (a/put! port [:fail ret]))
                     ;; Success: Continue
                     (a/put! port
                             [:success (mapv id->uuid (decode-body body))]))
                   ;; Close the return channel
                   (a/close! port))
        async-fn (fn [batch port]
                   (let [callback (partial callback port)]
                     (if @run?
                       (post-batch endpoint http-options batch callback)
                       (a/close! port))))]
    (a/pipeline-async concurrency out-chan async-fn in-chan)
    ;; Pipe to in-chan
    (a/pipe statement-chan in-chan)
    ;; Return the out chan
    out-chan))
