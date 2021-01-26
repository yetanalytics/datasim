(ns com.yetanalytics.datasim.util
  "General utilities"
  (:require [clojure.spec.alpha :as s]))

(s/fdef remove-nil-vals
  :args (s/cat :associative
               map?)
  :ret map?
  :fn (fn [{map-out :ret}]
        (every? some? (vals map-out))))

(defn remove-nil-vals
  "Remove nil values from an associative structure. Does not recurse.
  Suitable for coercing a record with nil vals to a map."
  [m]
  (reduce-kv
   (fn [m' k v]
     (if (nil? v)
       m'
       (assoc m' k v)))
   {}
   m))
