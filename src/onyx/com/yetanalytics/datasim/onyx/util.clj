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
