(ns com.yetanalytics.datasim.xapi.client
  "Simple xAPI LRS client fns"
  (:require
   [clojure.java.io :as io]
   [cheshire.core :as json]
   [org.httpkit.client :as http]
   ))

(def default-http-options
  {:headers {"X-Experience-Api-Version" "1.0.3"
             "content-type" "application/json"}})

(defn post-statements
  "Given LRS options and a seq of statements, send them to an LRS in sync batches"
  [{:keys [endpoint
           batch-size
           http-options]
    :or {batch-size 10}}
   statement-seq]
  ;; TODO: Exp backoff, etc
  (loop [batches (partition-all batch-size statement-seq)
         success []
         fail []]
    (if-let [batch (first batches)]
      (let [{:keys [status body] :as response} @(http/post
                                            (format "%s/statements" endpoint)
                                            (merge default-http-options
                                                   http-options
                                                   {:body (json/encode batch)
                                                    :as :stream}))]
        (if (= 200 status)
          (recur (rest batches)
                 (into success
                       (map
                        (fn [^String id]
                          (java.util.UUID/fromString id))
                        (with-open [rdr (io/reader body)]
                         (json/decode-stream rdr))))
                 fail)
          {:success success
           :fail (conj fail response)}))
      {:success success
       :fail fail})))
