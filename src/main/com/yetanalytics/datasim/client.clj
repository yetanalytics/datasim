(ns com.yetanalytics.datasim.client
  "Simple xAPI LRS client functions."
  (:require [clojure.core.async :as a]
            [clojure.java.io    :as io]
            [cheshire.core      :as json]
            [org.httpkit.client :as http]
            [com.yetanalytics.datasim.util.io :as dio]))

(defn post-error-message
  "Error message for when POSTing to an LRS fails."
  [status error]
  (format "POST Request FAILED with STATUS: %d, MESSAGE: %s"
          status
          (or (some-> error ex-message) "<none>")))

(def default-http-options
  {:headers {"X-Experience-Api-Version" "1.0.3"
             "Content-Type" "application/json"}})

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
  "Given LRS options and a `statement-seq`, send them to an LRS in synchronous
   batches. If `print-ids?` is `true`, returned statement IDs will be printed
   to stdout. `username` and `password` in the options map are the Basic Auth
   credentials of the LRS."
  [{:keys [endpoint
           batch-size
           username
           password]
    :or {batch-size 25}}
   statement-seq
   & {:keys [print-ids?]
      :or   {print-ids? true}}]
  ;; TODO: Exponential backoff, etc
  (let [http-options {:basic-auth [username password]}]
    (loop [batches (partition-all batch-size statement-seq)
           success 0
           fail    []]
      (if-let [batch (first batches)]
        (let [{:keys [status body] :as response}
              @(post-batch endpoint http-options batch)]
          (if (<= 200 status 299)
            ;; Success!
            (let [statement-ids (decode-body body)]
              (when print-ids?
                (dio/println-coll statement-ids))
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
         :fail    fail}))))

(defn post-statements-async
  "Given LRS options and a channel with statements, send them to an LRS in
   asynchronous batches. `username` and `password` in the options map are the
   Basic Auth credentials of the LRS.

   Returns a channel that will reciveve `[:success <list of statement ids>]`
   for each batch or `[:fail <failing request>]`. Will stop sending on failure."
  [{:keys [endpoint
           batch-size
           username
           password]
    :or {batch-size 25}}
   statement-chan
   & {:keys [concurrency
             buffer-in
             buffer-out]
      :or {concurrency 4
           buffer-in   100 ; 10x default batch size
           buffer-out  100}}]
  (let [http-opts {:basic-auth [username password]}
        run?      (atom true)
        in-chan   (a/chan buffer-in (partition-all batch-size))
        out-chan  (a/chan buffer-out) ; is this.. backpressure?
        callback  (fn [port {:keys [status body error] :as ret}]
                    (if (or (not= 200 status) error)
                      ;; Error: Stop further processing
                      (do
                        (swap! run? not)
                        (a/put! port [:fail ret]))
                      ;; Success: Continue
                      (a/put! port
                              [:success (decode-body body)]))
                    ;; Close the return channel
                    (a/close! port))
        async-fn (fn [batch port]
                   (let [callback (partial callback port)]
                     (if @run?
                       (post-batch endpoint http-opts batch callback)
                       (a/close! port))))]
    (a/pipeline-async concurrency out-chan async-fn in-chan)
    ;; Pipe to in-chan
    (a/pipe statement-chan in-chan)
    ;; Return the out chan
    out-chan))
