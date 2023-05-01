(ns com.yetanalytics.datasim.util
  "General utilities"
  (:require [clojure.spec.alpha :as s]))

(s/fdef remove-nil-vals
  :args (s/cat :m map?)
  :ret  (s/and map?
               #(every? some? (vals %))))

(defn remove-nil-vals
  "Remove `nil` values from the associative structure `m`, e.g. a map or
   a record. Does not recurse. Suitable for coercing a record with `nil` vals
   to a map."
  [m]
  (reduce-kv
   (fn [m' k v]
     (if (nil? v)
       m'
       (assoc m' k v)))
   {}
   m))
