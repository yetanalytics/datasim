(ns com.yetanalytics.datasim.onyx.util
  (:require [com.yetanalytics.datasim.input :as input]
            [cheshire.core :as json]
            [byte-streams :as bs]))


(defn round-robin
  "Given a number of partitions/buckets and a sequence, return a set of sets
  dividing up xs"
  [num-parts
   xs]
  (set
   (remove empty?
           (reduce
            (fn [vs [x idx]]
              (update vs idx conj x))
            (into []
                  (repeat num-parts #{}))
            (map
             vector
             xs
             (cycle (range num-parts)))))))

(defn batch->smile
  "Convert a batch of segments to SMILE data"
  ^bytes [segments]
  (json/generate-smile
   (mapcat :statements
           segments)))

(defn batch->json
  "Convert a batch of segments to JSON bytes"
  ^bytes [segments]
  (bs/to-byte-array
   (json/generate-string
    (mapcat :statements
            segments))))

(defn override-max!
  [input mo]
  (assoc-in input [:parameters :max] mo))
