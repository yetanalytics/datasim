(ns com.yetanalytics.datasim.protocols
  "Protocols for use across the application, we enumerate them here in one NS so
   we don't get into circ. dependency errors.")

(defprotocol FromInput
  (validate [this]
    "Validate the input, which must be read in first. Returns the output of `s/explain-data`"))

(defprotocol Serializable
  "Things that are serializable/deserializable"
  (deserialize [this r]
    "Get it from a reader.")
  (serialize [this w]
    "Write it to a writer"))
