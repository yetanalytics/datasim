(ns com.yetanalytics.datasim.protocols
  "Protocols for use across the application, we enumerate them here in one NS so
   we don't get into circ. dependency errors.")

(defprotocol FromInput
  (read-in [this location]
    "Return this with the data read and parsed, or throw.")
  (validate [this]
    "Validate the input, which must be read in first. Returns the output of `s/explain-data`"))
