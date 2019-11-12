(ns com.yetanalytics.datasim.input
  "Comprehensive specification of input"
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.pan.objects.profile :as ps]
            [com.yetanalytics.datasim.profile :as profile]
            ))

(s/def ::profiles
  (s/every ::ps/profile :min-count 1
           :into []))

(s/def :com.yetanalytics.datasim/input
  ;; "Comprehensive input spec"
  (s/keys :req-un [::profiles]))

(defrecord Input [profiles]
  p/FromInput
  ;; TODO: impliment read-in for comprehensive spec
  (validate [this]
    (s/explain-data :com.yetanalytics.datasim/input this)))

(defmulti from-location
  "Instantiate a new input object of type-k from location"
  (fn [type-k _]
    type-k))

(defmethod from-location :profile
  [_ location]
  (p/read-in (profile/map->Profile {})
             location))

(defn validate
  "Validate input using the FromInput protocol. Throw an exception if the input
   isn't valid."
  [input]
  (if-let [spec-error (p/validate input)]
    (throw (ex-info (pr-str spec-error)
                    {:type ::invalid-input
                     :input input
                     :spec-error spec-error}))
    input))
