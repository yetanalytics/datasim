(ns com.yetanalytics.datasim.input
  "Comprehensive specification of input"
  (:require [clojure.spec.alpha   :as s]
            [clojure.string       :as cs]
            [clojure.walk         :as w]
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

(defn validate-pattern-filters
  [{{:keys [gen-profiles
            gen-patterns]} :parameters
    :keys                  [profiles]}]
  (let [profile-idset (into #{}
                            (map :id profiles))
        pattern-idset (into #{}
                            (keep (fn [{:keys [id primary]}]
                                    (when primary id))
                                  (mapcat :patterns profiles)))]
    (concat
     (for [[idx profile-id] (map-indexed vector gen-profiles)
           :when            (not (contains? profile-idset profile-id))]
       {:id   (str "parameters-gen-profiles-" idx)
        :path [:parameters :gen-profiles idx]
        :text (format "Profile ID %s is not one of provided profiles: %s"
                      profile-id
                      (cs/join \, profile-idset))})
     (for [[idx pattern-id] (map-indexed vector gen-patterns)
           :when            (not (contains? pattern-idset pattern-id))]
       {:id   (str "parameters-gen-patterns-" idx)
        :path [:parameters :gen-patterns idx]
        :text
        (format
         "Pattern ID %s is not among primary patterns in provided profiles: %s"
         pattern-id
         (cs/join \, pattern-idset))}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input Sub-Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def subobject-constructors
  {:profile profile/map->Profile
   :personae personae/map->Personae
   :alignments alignments/map->Alignments
   :parameters params/map->Parameters
   ;; Hack for array-valued inputs
   :profiles profile/map->Profile
   :personae-array personae/map->Personae})

(defn realize-subobjects
  "Make subobjects from JSON into records"
  [json-result]
  (reduce-kv
   (fn [m k v]
     (if-let [constructor (get subobject-constructors k)]
       (let [rec (constructor {})
             key-fn
             (partial
              p/read-key-fn
              rec)
             body-fn
             (cond->> (partial
                       p/read-body-fn
                       rec)
               ;; for profiles and personae-array, it's a vector
               (#{:profiles :personae-array} k)
               (partial mapv))]
         (assoc m
                k
                (body-fn
                 (w/postwalk
                  (fn [node]
                    ;; here we can be sure every prop is a key
                    ;; since we force it with read-key-fn on input
                    (if (keyword? node)
                      (key-fn node)
                      node))
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
       (let [rec (constructor {})
             key-fn
             (partial
              p/write-key-fn
              rec)
             body-fn
             (cond->> p/write-body-fn
               ;; for profiles and persoane-array, it's a vector
               (#{:profiles :personae-array} k)
               (partial mapv))]
         (assoc m
                k
                (w/postwalk
                 (fn [node]
                   ;; Here we can't know it's a key unless we find it in a map
                   (if (map? node)
                     (reduce-kv
                      (fn [m' k' v']
                        (assoc m' (key-fn k') v'))
                      {}
                      node)
                     node))
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

(defn from-location
  [type-k fmt-k location]
  (if-let [constructor (if (= type-k :input)
                         map->Input
                         (get subobject-constructors type-k))]
    (case fmt-k
      ;; currently only JSON
      :json (if (#{:profiles :personae-array} type-k)
              (dio/read-loc-array (constructor {}) location)
              (dio/read-loc-json (constructor {}) location)))
    (throw (ex-info (format "Unknown key %s" type-k)
                    {:type ::unknown-key
                     :key type-k}))))

(defn to-file
  [record fmt-k location]
  (case fmt-k
    ;; currently only JSON
    :json (dio/write-file-json record location)))

(defn to-out
  [record fmt-k]
  (case fmt-k
    ;; currently only JSON
    :json (dio/write-loc-json record *out*)))

(defn to-err
  [record fmt-k]
  (case fmt-k
    ;; currently only JSON
    :json (dio/write-loc-json record *err*)))

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
