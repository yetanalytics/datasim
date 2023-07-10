(ns com.yetanalytics.datasim.util.io
  (:require [clojure.java.io :as io]
            [clojure.string  :as cstr]
            [cheshire.core   :as json])
  (:import [java.io IOException]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exceptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- throw-parse-error [cause-exn]
  (throw (ex-info "Parse Error"
                  {:type ::parse-error}
                  cause-exn)))

(defn- throw-unparse-error [cause-exn]
  (throw (ex-info "Unparse Error"
                  {:type ::unparse-error}
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

;; TODO: Also have a `key-fn?` kwarg here?
(defn read-json-location
  "Reads in a file from the given `location` and parses it into EDN. If
   the data being read is an array or sequence, return all the data
   as a seq."
  [location]
  (try (with-open [r (io/reader location)]
         (try (doall (json/parse-stream r keywordize-json-key))
              (catch Exception e
                (throw-parse-error e))))
       (catch IOException e
         (throw-io-error location e))))

(defn write-json
  "Write the contents of `data` to `writer`, which can be returned by
  `clojure.java.io/writer`. Applies `stringify-edn-key` if `key-fn?` is
   `true`."
  [data writer & {:keys [key-fn?] :or {key-fn? true}}]
  (try (if key-fn?
         (json/generate-stream data writer {:key-fn stringify-edn-key})
         (json/generate-stream data writer))
       (catch Exception e
         (throw-unparse-error e))))

;; Avoid `with-open` to prevent stdout/stderr writers from being closed

(defn write-json-stdout
  "Write the contents of `data` to standard output. Applies `stringify-edn-key`
   if `key-fn?` is `true`."
  [data & {:keys [key-fn?] :or {key-fn? true}}]
  (let [w (io/writer *out*)]
    (write-json data w :key-fn? key-fn?)
    (.write w "\n")
    (.flush w)))

(defn write-json-stderr
  "Write the contents of `data` to standard error. Applies `stringify-edn-key`
   if `key-fn?` is `true`."
  [data & {:keys [key-fn?] :or {key-fn? true}}]
  (let [w (io/writer *err*)]
    (write-json data w :key-fn? key-fn?)
    (.write w "\n")
    (.flush w)))

;; Use `with-open` to ensure that the file writer gets closed

(defn write-json-file
  "Write the contents of `data` to the file `location`; a file will be
   created if it does not exist. Stringifies keyword keys. Applies
   `stringify-edn-key` if `key-fn?` is `true`."
  [data location & {:keys [key-fn?] :or {key-fn? true}}]
  (let [file (io/file location)]
    (io/make-parents file)
    (with-open [w (io/writer location)]
      (write-json data w :key-fn? key-fn?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String I/O Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn println-coll
  "Print each string in `str-coll` followed by a newline to stdout,
   flushing the output buffer after each line."
  [str-coll]
  (doseq [s str-coll]
    (println s)
    (flush)))

(defn println-err-coll
  "Print each string in `err-str-coll` followed by a newline to stderr,
   flushing the output buffer after each line."
  [err-str-coll]
  (binding [*out* *err*]
    (println-coll err-str-coll)))
