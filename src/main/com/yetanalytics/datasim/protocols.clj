(ns com.yetanalytics.datasim.protocols
  "Protocols for use across the application, we enumerate them here in one NS so
   we don't get into circ. dependency errors.")

(defprotocol FromInput
  "Things that come from the user and must be validated."
  (validate [this]
    "Validate the input, which must be read in first. Returns nil if the input
     is valid, or a vector of error maps each containing:
       :id - Generated unique ID for error
       :path - Path of error in input
       :text - Error message/text"))

(defprotocol JSONRepresentable
  "Things that can be represented as JSON"
  (read-key-fn [this k]
    "Wrapped in a partial, will coerce k to internal representation.")
  (read-body-fn [this json-result]
    "Return a new record with the given JSON incorporated")
  (write-key-fn [this k]
    "Wrapped in a partial, will coerce k to json representation")
  (write-body-fn [this]
    "Return the part of the record used to generate json."))
