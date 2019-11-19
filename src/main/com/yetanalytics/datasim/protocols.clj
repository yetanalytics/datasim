(ns com.yetanalytics.datasim.protocols
  "Protocols for use across the application, we enumerate them here in one NS so
   we don't get into circ. dependency errors.")

(defprotocol FromInput
  "Things that come from the user and must be validated."
  (validate [this]
    "Validate the input, which must be read in first. Returns the output of `s/explain-data`"))

(defprotocol Serializable
  "Things that are serializable/deserializable"
  (deserialize [this r]
    "Get it from a reader.")
  (serialize [this w]
    "Write it to a writer"))

(defprotocol JSONRepresentable
  "Things that can be represented as JSON"
  (read-key-fn [this k]
    "Wrapped in a partial, is a key-fn suitable for http://clojure.github.io/data.json/#clojure.data.json/read")
  (read-value-fn [this k v]
    "Wrapped in a partial, is a value-fn suitable for http://clojure.github.io/data.json/#clojure.data.json/read")
  (read-body-fn [this json-result]
    "Return a new record with the given JSON incorporated")
  (write-key-fn [this k]
    "Wrapped in a partial, is a key-fn suitable for http://clojure.github.io/data.json/#clojure.data.json/write")
  (write-value-fn [this k v]
    "Wrapped in a partial, is a value-fn suitable for http://clojure.github.io/data.json/#clojure.data.json/write")
  (write-body-fn [this]
    "Return the part of the record used to generate json.")
  )
