(ns com.yetanalytics.datasim.input
  "Comprehensive specification of input"
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.pan.objects.profile :as ps]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.profile :as profile]
            [com.yetanalytics.datasim.personae :as personae]
            [com.yetanalytics.datasim.io :as dio]))

(s/def ::profiles
  (s/every ::ps/profile :min-count 1
           :into []))

(s/def ::personae
  ::personae/group)

(s/def :com.yetanalytics.datasim/input
  ;; "Comprehensive input spec"
  (s/keys :req-un [::profiles
                   ::personae]))

(defrecord Input [profiles]
  p/FromInput
  (validate [this]
    (s/explain-data :com.yetanalytics.datasim/input this)))

(defmulti from-location
  "Instantiate a new input object of type-k from location"
  (fn [type-k _]
    type-k))

(defmethod from-location :profile
  [_ location]
  (dio/read-loc (profile/map->Profile {}) location))

(defmethod from-location :personae
  [_ location]
  (dio/read-loc (personae/map->Personae {}) location))

(defn validate
  "Validate input using the FromInput protocol. Does no handling on result"
  [input]
  (p/validate input))

(defn validate-throw
  "Validate input using the FromInput protocol. Throw an exception if the input
   isn't valid."
  [input]
  (if-let [spec-error (p/validate input)]
    (throw (ex-info (pr-str spec-error)
                    {:type ::invalid-input
                     :input input
                     :spec-error spec-error}))
    input))
