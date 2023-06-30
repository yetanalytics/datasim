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

(defn dynamic-or
  "Similar to `s/or`, but accepts a coll of `[:key pred]` pairs. Unlike
   `s/or`, this accepts a dynamic number of such pairs (hence why it
   is a function instead of a macro). This will only work properly if
   `preds` are keywords or predicate functions."
  [key-pred-pairs]
  (let [keys  (mapv first key-pred-pairs)
        preds (mapv second key-pred-pairs)]
    ;; Yes, spec says not to use `or-spec-impl`, but we need to create
    ;; `s/or` specs at runtime and it is much easier to bypass the macro
    ;; instead of mixing compile-time and run-time code.
    (s/or-spec-impl keys preds preds nil)))
