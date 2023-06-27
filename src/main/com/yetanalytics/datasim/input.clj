(ns com.yetanalytics.datasim.input
  "Comprehensive specification of input"
  (:require [clojure.spec.alpha   :as s]
            [clojure.string       :as cs]
            [clojure.walk         :as w]
            [com.yetanalytics.pan :as pan]
            [com.yetanalytics.datasim.protocols        :as p]
            [com.yetanalytics.datasim.input.profile    :as profile]
            [com.yetanalytics.datasim.input.personae   :as personae]
            [com.yetanalytics.datasim.input.alignments :as alignments]
            [com.yetanalytics.datasim.input.parameters :as params]
            [com.yetanalytics.datasim.io               :as dio]
            [com.yetanalytics.datasim.util.xapi        :as xapiu]
            [com.yetanalytics.datasim.util.errors      :as errs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is our system:
;;   persona: a single Agent who is a member of a Group
;;   personae: a Group that contains one or more Agent, i.e. persona
;;   personae-array: an array of one or more Groups

(defn- distinct-member-ids?
  [personaes]
  (let [member-ids (->> personaes
                        (map :member)
                        (apply concat)
                        (map xapiu/agent-id))]
    (= (-> member-ids count)
       (-> member-ids distinct count))))

(s/def ::personae-array
  (s/and
   (s/every ::personae/personae :min-count 1 :into [])
   distinct-member-ids?))

(s/def ::alignments
  ::alignments/alignments-input)

(s/def ::parameters
  ::params/parameters)

(s/def :com.yetanalytics.datasim/input
  ;; "Comprehensive input spec"
  (s/keys :req-un [::profiles
                   ::personae-array
                   ::alignments
                   ::parameters]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn validate-profiles
  [profile-coll]
  (if (vector? profile-coll)
    (let [prof-errs (pan/validate-profile-coll profile-coll
                                               :syntax? true
                                               :pattern-rels? true
                                               :result :type-path-string)]
      (errs/type-path-string-ms->map-coll (map :id profile-coll)
                                          prof-errs))
    ;; TODO: Something more solid/less hacky, particularly in the Pan lib itself
    [{:path [::profiles]
      :text "Profiles must be a vector!"
      :id   [::profiles]}]))

(defn validate-personae-array
  [personae-array]
  (when-some [ed (s/explain-data ::personae-array personae-array)]
    (errs/explain-to-map-coll ::personae-array ed)))

(defn validate-alignments
  [alignments]
  (when-some [ed (s/explain-data ::alignments alignments)]
    (errs/explain-to-map-coll ::alignments ed)))

(defn validate-parameters
  [parameters]
  (when-some [ed (s/explain-data ::parameters parameters)]
    (errs/explain-to-map-coll ::parameters ed)))

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
  (validate [this]
    (-> (concat (validate-profiles (:profiles this))
                (validate-personae-array (:personae-array this))
                (validate-alignments (:alignments this))
                (validate-parameters (:parameters this))
                (validate-pattern-filters this))
        vec
        not-empty))

  p/JSONRepresentable
  (read-key-fn [_ k]
    (keyword nil k))
  (read-body-fn [_ json-result]
    (map->Input
     (realize-subobjects json-result)))
  (write-key-fn [_ k]
    (name k))
  (write-body-fn [this]
    (unrealize-subobjects this)))

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
