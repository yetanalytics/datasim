(ns com.yetanalytics.datasim.util.io
  (:require [clojure.java.io :as io]
            [clojure.string  :as cstr]
            [cheshire.core   :as json])
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
;; Keyword Key Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- keywordize-json-key
  [k]
  (keyword (cond-> k
             (= \@ (first k))
             (cstr/replace-first \@ \_))))

(defn- stringify-edn-key
  [k]
  (let [named-key (name k)]
    (cond-> named-key
      (= \_ (first named-key))
      (cstr/replace-first \_ \@))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON I/O Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-json-location
  "Reads in a file from the given `location` and parses it into EDN. If
   the data being read is an array or sequence, return all the data
   as a seq."
  [location]
  (try (with-open [r (io/reader location)]
         (try (doall (json/parse-stream r keywordize-json-key))
              (catch Exception e
                (throw-parse-error location e))))
       (catch IOException e
         (throw-io-error location e))))

(defn- write-json!
  [data location writer]
  (try (json/generate-stream data writer {:key-fn stringify-edn-key})
       (catch Exception e
         (throw-unparse-error location e))))

(defn write-json-location
  "Write the contents of `data` to the `location`, which can be `*out*`
   for stdout or `*err*` for stderr."
  [data location]
  (try (if (#{*out* *err*} location)
         ;; Write to stdout or stderr
         (let [w (io/writer location)]
           (write-json! data location w)
           (.write w "\n")
           (.flush w))
         ;; Write to a file
         (with-open [w (io/writer location)]
           (write-json! data location w)))
       (catch IOException e
         (throw-io-error location e))))

(defn write-json-file
  "Write the contents of `data` to the file `location`; a file will be
   created if it does not exist."
  [data location]
  (let [file (io/file location)]
    (io/make-parents file)
    (with-open [w (io/writer location)]
      (write-json! data location w))))
