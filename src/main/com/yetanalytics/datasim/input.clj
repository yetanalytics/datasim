(ns com.yetanalytics.datasim.input
  "Comprehensive specification of input"
  (:require [clojure.spec.alpha :as s]
            [clojure.walk       :as w]
            [com.yetanalytics.datasim.protocols        :as p]
            [com.yetanalytics.datasim.input.profile    :as profile]
            [com.yetanalytics.datasim.input.personae   :as personae]
            [com.yetanalytics.datasim.input.alignments :as alignments]
            [com.yetanalytics.datasim.input.parameters :as params]
            [com.yetanalytics.datasim.io               :as dio]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is our system:
;;   persona: a single Agent who is a member of a Group
;;   personae: a Group that contains one or more Agent, i.e. persona
;;   personae-array: an array of one or more Groups

(s/def ::personae-array
  ::personae/personae-array)

;; TODO: `::alignments` is the name of two separate things: the top-level
;; alignments input vector, and the inner vector associated with each actor.
;; Find separate names for each.
(s/def ::alignments
  ::alignments/alignments-input)

(s/def ::parameters
  ::params/parameters)

(s/def :com.yetanalytics.datasim/input
  (s/keys :req-un [::profiles ; TODO: This spec isn't defined???
                   ::personae-array
                   ::alignments
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
;; Input Sub-Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def subobject-constructors
  {:profile        profile/map->Profile
   :personae       personae/map->Personae
   :alignments     alignments/map->Alignments
   :parameters     params/map->Parameters
   ;; Hack for array-valued inputs
   :profiles       profile/map->Profile
   :personae-array personae/map->Personae})

(defn realize-subobjects
  "Make subobjects from JSON into records"
  [json-result]
  (reduce-kv
   (fn [m k v]
     (if-let [constructor (get subobject-constructors k)]
       (let [rec     (constructor {})
             key-fn  (partial p/read-key-fn rec)
             body-fn (cond->> (partial p/read-body-fn rec)
                       (#{:profiles :personae-array} k)
                       (partial mapv))]
         (assoc m k (body-fn
                     (w/postwalk
                      ;; here we can be sure every prop is a key
                      ;; since we force it with read-key-fn on input
                      (fn [node] (cond-> node (keyword? node) key-fn))
                      v))))
       (throw (ex-info (format "Unknown key %s" k)
                       {:type ::unknown-key
                        :key k
                        :json json-result}))))
   {}
   json-result))

(defn unrealize-subobjects
  "Make subobjects ready for writing to JSON"
  [input]
  (reduce-kv
   (fn [m k v]
     (if-let [constructor (get subobject-constructors k)]
       (let [rec     (constructor {})
             key-fn  (partial p/write-key-fn rec)
             body-fn (cond->> p/write-body-fn
                       (#{:profiles :personae-array} k)
                       (partial mapv))]
         (assoc m k (w/postwalk
                     ;; Here we can't know it's a key unless we find it in a map
                     (fn [node]
                       (cond->> node
                         (map? node)
                         (reduce-kv
                          (fn [m' k' v'] (assoc m' (key-fn k') v')) {})))
                     (body-fn v))))
       (throw (ex-info (format "Unknown key %s" k)
                       {:type ::unknown-key
                        :key k
                        :input input}))))
   {}
   input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input Record
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Input [profiles
                  personae-array
                  alignments
                  parameters]
  p/FromInput
  (validate [input]
    (-> (concat (profile/validate-profiles profiles)
                (personae/validate-personae-array personae-array)
                (alignments/validate-alignments alignments)
                (params/validate-parameters parameters)
                (validate-pattern-filters input))
        vec
        not-empty))

  p/JSONRepresentable
  (read-key-fn [_ k]
    (keyword k))
  (read-body-fn [_ json-result]
    (map->Input (realize-subobjects json-result)))
  (write-key-fn [_ k]
    (name k))
  (write-body-fn [input]
    (unrealize-subobjects input)))

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
      (update :profiles (partial mapv profile/map->Profile))
      (update :personae-array (partial mapv personae/map->Personae))
      (update :alignments vec)
      (update :alignments (partial assoc {} :alignment-vector))
      (update :alignments alignments/map->Alignments)
      (update :parameters params/add-defaults)
      (update :parameters params/map->Parameters)
      map->Input))

(defmethod from-location [:profile :json] [_ _ location]
  (->> (dio/read-json-location location)
       profile/map->Profile))

(defmethod from-location [:profiles :json] [_ _ location]
  (->> (dio/read-json-location location)
       (mapv profile/map->Profile)))

(defmethod from-location [:personae :json] [_ _ location]
  (->> (dio/read-json-location location)
       personae/map->Personae))

(defmethod from-location [:personae-array :json] [_ _ location]
  (->> (dio/read-json-location location)
       (mapv personae/map->Personae)))

(defmethod from-location [:alignments :json] [_ _ location]
  (->> (dio/read-json-location location)
       vec
       (assoc {} :alignment-vector)
       alignments/map->Alignments))

(defmethod from-location [:parameters :json] [_ _ location]
  (->> (dio/read-json-location location)
       params/add-defaults
       params/map->Parameters))

;; Write to file

(defmulti to-file (fn [_ format-k _] format-k))

(defmethod to-file :default [_ format-k _]
  (throw-unknown-format format-k))

(defmethod to-file :json [data _ location]
  (dio/write-json-file (p/write-body-fn data) location))

;; Write to stdout

(defmulti to-out (fn [_ fmt-k] fmt-k))

(defmethod to-out :default [_ format-k _]
  (throw-unknown-format format-k))

(defmethod to-out :json [data _]
  (dio/write-json-location (p/write-body-fn data) *out*))

;; Write to stderr

(defmulti to-err (fn [_ fmt-k] fmt-k))

(defmethod to-err :default [_ format-k _]
  (throw-unknown-format format-k))

(defmethod to-err :json [data _]
  (dio/write-json-location (p/write-body-fn data) *err*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input Validation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn validate
  "Validate input using the FromInput protocol. Does no handling on result.
   Returns a coll of maps on failure, `nil` on success."
  [input]
  (p/validate input))

(defn validate-throw
  "Validate input using the FromInput protocol. Throw an exception if the input
   isn't valid."
  [input]
  (if-let [errors (not-empty (p/validate input))]
    (throw (ex-info "Validation Errors"
                    {:type ::invalid-input
                     :input input
                     :errors errors}))
    input))
