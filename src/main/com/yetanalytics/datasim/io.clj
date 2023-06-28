(ns com.yetanalytics.datasim.io
  (:require [clojure.java.io :as io]
            [cheshire.core   :as json]
            [com.yetanalytics.datasim.protocols :as p])
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
;; JSON I/O Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-loc-json
  "Reads in a file from the given location `loc` and parses it. The `record`
   argument, which should implement `protocols/JSONRepresentable`, determines
   the exact functions used to read the file contents."
  [record loc]
  (let [key-fn    (partial p/read-key-fn record)
        read-body (partial p/read-body-fn record)]
    (try (with-open [r (io/reader loc)]
           (try (read-body (doall (json/parse-stream r key-fn)))
                (catch Exception e
                  (throw-parse-error loc e))))
         (catch IOException e
           (throw-io-error loc e)))))

(defn read-loc-array
  "Reads from a stream from the given location `loc` but assumes an array with
   the members of the `record` type passed to it. The The `record` argument,
   which should implement `protocols/JSONRepresentable`, determines
   the exact functions used to read the file contents."
  [record loc]
  (let [key-fn    (partial p/read-key-fn record)
        read-body (partial p/read-body-fn record)]
    (try (with-open [r (io/reader loc)]
           (try (mapv read-body (json/parse-stream r key-fn))
                (catch Exception e
                  (throw-parse-error loc e))))
         (catch IOException e
           (throw-io-error loc e)))))

(defn- write-json!
  [record loc writer]
  (let [key-fn (partial p/write-key-fn record)]
    (try (json/generate-stream (p/write-body-fn record)
                               writer
                               {:key-fn key-fn})
         (catch Exception e
           (throw-unparse-error loc e)))))

(defn write-loc-json
  "Write the contents of `record` to a location `loc`; `record` should
   implement `protocols/JSONRepresentable` and determines the exact functions
   used to read the file contents."
  [record loc]
  (try (if (#{*out* *err*} loc)
         ;; Write to stdout or stderr
         (let [w (io/writer loc)]
           (write-json! record loc w)
           (.write w "\n")
           (.flush w))
         ;; Write to a file
         (with-open [w (io/writer loc)]
           (write-json! record loc w)))
       (catch IOException e
         (throw-io-error loc e))))

(defn write-file-json
  "Write `record` to a file at location `loc`; `record` should implement
   `protocols/JSONRepresentable` and determines the exact functions
   used to read the file contents."
  [record loc]
  (let [file (io/file loc)]
    (io/make-parents file)
    (write-loc-json record file)))
