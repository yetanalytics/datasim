(ns com.yetanalytics.datasim.io
  (:require [clojure.java.io :as io]
            [com.yetanalytics.datasim.protocols :as p]
            [cheshire.core :as json])
  (:import [java.io IOException]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exceptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- throw-parse-error [location cause-exn]
  (throw (ex-info "Parse Error"
                  {:type     ::parse-error
                   :location location}
                  cause-exn)))

(defn- throw-unparse-error [location cause-exn]
  (throw (ex-info "Unparse Error"
                  {:type     ::unparse-error
                   :location location}
                  cause-exn)))

(defn- throw-io-error [location cause-exn]
  (throw (ex-info "I/O Error"
                  {:type     ::io-error
                   :location location}
                  cause-exn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IO Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-loc-json
  "Reads in a file from the given location and parses it."
  [record loc]
  (try (with-open [r (io/reader loc)]
         (try
           (p/read-body-fn
            record
            (doall (json/parse-stream r (partial p/read-key-fn record))))
           (catch Exception e
             (throw-parse-error loc e))))
       (catch IOException e
         (throw-io-error loc e))))

(defn read-loc-array
  "Reads from a stream but assumes an array with the members of the record type passed to it"
  [record loc]
  (try (with-open [r (io/reader loc)]
         (try
           (let [input-coll (json/parse-stream r (partial p/read-key-fn record))]
             (mapv (fn [input]
                     (p/read-body-fn record input))
                   input-coll))
           (catch Exception e
             (throw-parse-error loc e))))
       (catch IOException e
         (throw-io-error loc e))))

(defn- write-json!
  [record loc w]
  (try
    (json/generate-stream (p/write-body-fn record)
                          w
                          {:key-fn (partial p/write-key-fn record)})
    (catch Exception e
      (throw-unparse-error loc e))))

(defn write-loc-json
  "Write a record to a location"
  [record loc]
  (try (if (#{*out* *err*} loc)
         (let [w (io/writer loc)]
           (write-json! record loc w)
           (.write w "\n")
           (.flush w))
         (with-open [w (io/writer loc)]
           (write-json! record loc w)))
       (catch IOException e
         (throw-io-error loc e))))

(defn write-file-json
  "Write a record to a file"
  [record loc]
  (let [file (io/file loc)]
    (io/make-parents file)
    (write-loc-json record file)))
