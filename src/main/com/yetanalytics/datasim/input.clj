(ns com.yetanalytics.datasim.input
  "Comprehensive specification of input"
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim :as-alias datasim]
            [com.yetanalytics.datasim.input.profile    :as profile]
            [com.yetanalytics.datasim.input.personae   :as personae]
            [com.yetanalytics.datasim.input.model      :as models]
            [com.yetanalytics.datasim.input.parameters :as params]
            [com.yetanalytics.datasim.util.io          :as dio]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::profiles
  ::profile/profiles)

;; This is our system:
;;   persona: a single Agent who is a member of a Group
;;   personae: a Group that contains one or more Agent, i.e. persona
;;   personae-array: an array of one or more Groups

(s/def ::personae-array
  ::personae/personae-array)

(s/def ::models
  ::models/models)

(s/def ::parameters
  ::params/parameters)

(s/def ::datasim/input
  (s/keys :req-un [::profiles
                   ::personae-array
                   ::models
                   ::parameters]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We need this function here since it's the only validation function that
;; makes use of two different parts of the input spec

(defn validate-pattern-filters
  [{{:keys [gen-profiles gen-patterns]} :parameters
    profiles :profiles}]
  (concat (profile/validate-profile-filters profiles gen-profiles)
          (profile/validate-pattern-filters profiles gen-patterns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input I/O
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- throw-unknown-key [type-k]
  (throw (ex-info (format "Unknown key %s" type-k)
                  {:type ::unknown-key
                   :key  type-k})))

(defn- throw-unknown-format [format-k]
  (throw (ex-info (format "Unknown format %s" format-k)
                  {:type   ::unknown-format
                   :format format-k})))

;; Read Input

(defmulti from-location
  (fn [type-k format-k _] [type-k format-k]))

(defmethod from-location :default [type-k format-k _]
  (if (#{:json} format-k)
    (throw-unknown-key type-k)
    (throw-unknown-format format-k)))

(defmethod from-location [:input :json] [_ _ location]
  (-> (dio/read-json-location location)
      (update :parameters params/apply-defaults)))

(defmethod from-location [:profile :json] [_ _ location]
  (dio/read-json-location location))

(defmethod from-location [:profiles :json] [_ _ location]
  (->> (dio/read-json-location location)
       vec))

(defmethod from-location [:personae :json] [_ _ location]
  (dio/read-json-location location))

(defmethod from-location [:personae-array :json] [_ _ location]
  (->> (dio/read-json-location location)
       vec))

(defmethod from-location [:models :json] [_ _ location]
  (->> (dio/read-json-location location)
       vec))

(defmethod from-location [:parameters :json] [_ _ location]
  (->> (dio/read-json-location location)
       params/apply-defaults))

;; Write to file

(defmulti to-file (fn [_ format-k _] format-k))

(defmethod to-file :default [_ format-k _]
  (throw-unknown-format format-k))

(defmethod to-file :json [data _ location]
  (dio/write-json-file data location))

;; Write to stdout

(defmulti to-out (fn [_ fmt-k] fmt-k))

(defmethod to-out :default [_ format-k _]
  (throw-unknown-format format-k))

(defmethod to-out :json [data _]
  (dio/write-json-stdout data))

;; Write to stderr
;; TODO: Currently unused

(defmulti to-err (fn [_ fmt-k] fmt-k))

(defmethod to-err :default [_ format-k _]
  (throw-unknown-format format-k))

(defmethod to-err :json [data _]
  (dio/write-json-stderr data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input Validation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti validate
  "Validate input using the FromInput protocol. Does no handling on result.
   Returns a coll of maps on failure, `nil` on success."
  (fn [type-k _] type-k))

(defmethod validate :default [type-k _]
  (throw-unknown-key type-k))

(defmethod validate :input
  [_ {:keys [profiles personae-array models parameters] :as input}]
  (-> (concat (profile/validate-profiles profiles)
              (personae/validate-personae-array personae-array)
              (models/validate-models models)
              (params/validate-parameters parameters)
              (validate-pattern-filters input))
      vec
      not-empty))

(defmethod validate :profile [_ profile]
  (profile/validate-profile profile))

(defmethod validate :personae [_ personae]
  (personae/validate-personae personae))

(defmethod validate :models [_ models]
  (models/validate-models models))

(defmethod validate :parameters [_ parameters]
  (params/validate-parameters parameters))

(defn validate-throw
  "Validate input using the FromInput protocol. Throw an exception if the input
   isn't valid."
  [type-k input]
  (if-let [errors (not-empty (validate type-k input))]
    (throw (ex-info (format "Validation Errors on %s" (name type-k))
                    {:type   ::invalid-input
                     :key    type-k
                     :input  input
                     :errors errors}))
    input))
