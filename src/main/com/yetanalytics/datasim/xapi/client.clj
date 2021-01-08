(ns com.yetanalytics.datasim.xapi.client
  "Simple xAPI LRS client fns"
  (:require
   [clojure.java.io :as io]
   [cheshire.core :as json]
   [org.httpkit.client :as http]
   [clojure.core.async :as a]))

(def default-http-options
  {:headers {"X-Experience-Api-Version" "1.0.3"
             "content-type" "application/json"}})

(defn post-statements
  "Given LRS options and a seq of statements, send them to an LRS in sync batches
  If an emit-ids-fn is given it will be called with posted statement ids on success."
  [{:keys [endpoint
           batch-size
           http-options]
    :or {batch-size 25}}
   statement-seq
   & {:keys [emit-ids-fn]}]
  ;; TODO: Exp backoff, etc
  (loop [batches (partition-all batch-size statement-seq)
         success 0
         fail []]
    (if-let [batch (first batches)]
      (let [ start (. System (currentTimeMillis))
            http-opts (merge default-http-options
                             http-options
                             {:body (json/encode batch)
                              :as :stream})
            {:keys [status body] :as response} @(http/post
                                                 (format "%s/statements" endpoint)
                                                 http-opts)]

        (if (= 200 status)
          (let [ids (map
                     (fn [^String id]
                       (java.util.UUID/fromString id))
                     (with-open [rdr (io/reader body)]
                       (json/decode-stream rdr)))]
            (when emit-ids-fn
              (emit-ids-fn ids))
            (recur (rest batches)
                   (+ success (count ids))
                   fail))
          {:success success
           :fail (conj fail
                       (cond-> response
                         body
                         (assoc :body
                                (with-open [rdr (io/reader body)]
                                  (json/decode-stream rdr)))))}))
      {:success success
       :fail fail})))

(defn post-statements-async
  "Given LRS options and a channel with statements, send them to an LRS in async
   batches

   Returns a channel that will reciveve [:success <list of statement ids>] for
   each batch or [:fail <failing request>]. Will stop sending on failure."
  [{:keys [endpoint
           batch-size
           http-options]
    :or {batch-size 25}}
   statement-chan
   & {:keys [concurrency
             buffer-in
             buffer-out]
      :or {concurrency 4
           buffer-in 100 ;; 10x default batch size
           buffer-out 100
           }}]
  (let [run? (atom true)
        in-chan (a/chan buffer-in
                        (partition-all batch-size))
        ;; is this.. backpressure?
        out-chan (a/chan buffer-out)]
    (a/pipeline-async
     concurrency
     out-chan
     (fn [batch p]
       (if @run?
         (http/post
          (format "%s/statements" endpoint)
          (merge default-http-options
                 http-options
                 {:body (json/encode batch)
                  :as :stream})
          (fn [{:keys [status headers body error] :as ret}]
            (if (or (not= 200 status)
                    error)
              (do
                ;; Stop further processing
                (swap! run? not)
                (a/put!
                 p
                 [:fail ret]))
              (a/put!
               p
               [:success (mapv
                          (fn [^String id]
                            (java.util.UUID/fromString id))
                          (with-open [rdr (io/reader body)]
                            (json/decode-stream rdr)))]))
            ;; Close the return channel
            (a/close! p)))
         (a/close! p)))
     in-chan)
    ;; Pipe to in-chan
    (a/pipe statement-chan in-chan)
    ;; Return the out chan
    out-chan))
