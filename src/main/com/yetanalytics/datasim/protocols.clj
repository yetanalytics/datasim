(ns com.yetanalytics.datasim.protocols
  "Protocols for use across the application,")

;; We enumerate them here in one namespace so that we don't get into circular
;; dependency errors.

(defprotocol FromInput
  "Inputs that come from CLI a file, or other source and must bevalidated."
  (validate [input]
    "Validate the `input`, which must be read in first. Returns `nil` if the
     `input` is valid, or a vector of error maps each containing:
     
     - `:id` - Generated unique ID for error
     - `:path` - Path of error in input
     - `:text` - Error message/text"))

(defprotocol JSONRepresentable
  "Things that can be represented as JSON."
  (read-key-fn [this k]
    "Wrapped in a partial, will coerce key `k` to internal representation.")
  (read-body-fn [this json-result]
    "Return a new record with the given JSON incorporated")
  (write-key-fn [this k]
    "Wrapped in a partial, will coerce key `k` to JSON representation")
  (write-body-fn [this]
    "Return the part of the record used to generate JSON."))
