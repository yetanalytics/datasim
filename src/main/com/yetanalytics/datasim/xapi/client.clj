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
    :or {batch-size 10}}
   statement-seq
   & {:keys [emit-ids-fn]}]
  ;; TODO: Exp backoff, etc
  (loop [batches (partition-all batch-size statement-seq)
         success 0
         fail []]
    (if-let [batch (first batches)]
      (let [{:keys [status body] :as response} @(http/post
                                                 (format "%s/statements" endpoint)
                                                 (merge default-http-options
                                                        http-options
                                                        {:body (json/encode batch)
                                                         :as :stream}))]
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

(defn- post-chan
  [url options]
  (let [pchan (a/promise-chan)]
    (http/post url options
               (fn [ret] (a/put! pchan ret)))
    pchan))

(defn post-statements-async
  "Given LRS options and a seq of statements, send them to an LRS in async batches concurrent by agent
  If an emit-ids-fn is given it will be called with posted statement ids on success."
  [{:keys [endpoint
           batch-size
           http-options]
    :or {batch-size 10}}
   statement-chan
   & {:keys [emit-ids-fn]}]
  ;; TODO: Exp backoff, etc
  (a/go-loop [sent 0]
    (if-let [batch (not-empty (a/<! (a/into []
                                            (a/take batch-size
                                                    statement-chan))))]
      (let [{:keys [status headers body error] :as ret}
            (a/<! (post-chan
                   (format "%s/statements" endpoint)
                   (merge default-http-options
                          http-options
                          {:body (json/encode batch)
                           :as :stream})))]
        (if (and (= 200
                    status)
                 (not error))
          (let [ids (map
                     (fn [^String id]
                       (java.util.UUID/fromString id))
                     (with-open [rdr (io/reader body)]
                       (json/decode-stream rdr)))]
            (when emit-ids-fn
              (emit-ids-fn ids))
            (recur (+ sent (count batch))))
          ;; return error
          {:fail [ret]
           :success sent}))
      {:success sent})))
