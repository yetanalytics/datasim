(ns com.yetanalytics.datasim.onyx.util
  (:require [com.yetanalytics.datasim.input :as input]
            [cheshire.core :as json])
  (:import [java.io ByteArrayInputStream]))

(defn parse-input
  "Return a valid, realized input or throw"
  [^String input-json]
  (input/validate-throw
   (input/from-location
    :input :json
    (ByteArrayInputStream.
     (.getBytes ^String input-json "UTF-8")))))

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
  [segments]
  (json/generate-smile
   (mapcat :statements
           segments)))

(comment

  (batch->smile [{:statements []}])
  )
