(ns com.yetanalytics.datasim.input
  "Comprehensive specification of input"
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.pan.objects.profile :as ps]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.input.profile :as profile]
            [com.yetanalytics.datasim.input.personae :as personae]
            [com.yetanalytics.datasim.input.alignments :as alignments]
            [com.yetanalytics.datasim.input.parameters :as params]
            [com.yetanalytics.datasim.io :as dio]))

(s/def ::profiles
  (s/every ::ps/profile :min-count 1
           :into []))

(s/def ::personae
  ::personae/personae)

(s/def ::alignments
  ::alignments/alignments)

(s/def ::parameters
  ::params/parameters)

(s/def :com.yetanalytics.datasim/input
  ;; "Comprehensive input spec"
  (s/keys :req-un [::profiles
                   ::personae
                   ::alignments
                   ::parameters]))

(defrecord Input [profiles]
  p/FromInput
  (validate [this]
    (s/explain-data :com.yetanalytics.datasim/input this)))

(defmulti from-location
  "Instantiate a new input object of type-k from location in the given format"
  (fn [type-k fmt-k _]
    [type-k fmt-k]))

(defmethod from-location [:profile :json]
  [_ _ location]
  (dio/read-loc-json (profile/map->Profile {}) location))

(defmethod from-location [:personae :json]
  [_ _ location]
  (dio/read-loc-json (personae/map->Personae {}) location))

(defmethod from-location [:alignments :json]
  [_ _ location]
  (dio/read-loc-json (alignments/map->Alignments {}) location))

(defmethod from-location [:parameters :json]
  [_ _ location]
  (dio/read-loc-json (params/map->Parameters {}) location))

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
