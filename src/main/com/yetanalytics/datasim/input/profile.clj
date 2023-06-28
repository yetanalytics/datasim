(ns com.yetanalytics.datasim.input.profile
  "Profile input parsing."
  (:require [clojure.string       :as cstr]
            [com.yetanalytics.pan :as pan]
            [com.yetanalytics.datasim.protocols   :as p]
            [com.yetanalytics.datasim.util.errors :as errs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn validate-profile
  [{:keys [id] :as profile}]
  (some->> (pan/validate-profile profile
                                 :syntax? true
                                 :result :type-path-string)
           (errs/type-path-string-m->map-coll id)))

(defn validate-profiles
  [profiles]
  (if (vector? profiles)
    (let [prof-errs (pan/validate-profile-coll profiles
                                               :syntax? true
                                               :pattern-rels? true
                                               :result :type-path-string)]
      (errs/type-path-string-ms->map-coll (map :id profiles)
                                          prof-errs))
    ;; TODO: Something more solid/less hacky, particularly in the Pan lib itself
    [{:path [::profiles]
      :text "Profiles must be a vector!"
      :id   [::profiles]}]))

;; Helpers to `input/validate-pattern-filters`

(defn- validate-pattern-filters-emsg
  [pattern-id pattern-id-set]
  (format
   "Pattern ID %s is not among primary patterns in provided profiles: %s"
   pattern-id
   (cstr/join \, pattern-id-set)))

(defn- validate-profile-fitlers-emsg
  [profile-id profile-id-set]
  (format "Profile ID %s is not one of provided profiles: %s"
          profile-id
          (cstr/join \, profile-id-set)))

(defn validate-pattern-filters
  [profiles gen-patterns]
  (let [pattern-id-set (->> profiles
                            (mapcat :patterns)
                            (keep (fn [{:keys [id primary]}] (when primary id)))
                            (into #{}))]
    (for [[idx pattern-id] (map-indexed vector gen-patterns)
          :when            (not (contains? pattern-id-set pattern-id))]
      {:id   (str "parameters-gen-patterns-" idx)
       :path [:parameters :gen-patterns idx]
       :text (validate-pattern-filters-emsg pattern-id pattern-id-set)})))

(defn validate-profile-filters
  [profiles gen-profiles]
  (let [profile-id-set (->> profiles (map :id) (into #{}))]
    (for [[idx profile-id] (map-indexed vector gen-profiles)
          :when            (not (contains? profile-id-set profile-id))]
      {:id   (str "parameters-gen-profiles-" idx)
       :path [:parameters :gen-profiles idx]
       :text (validate-profile-fitlers-emsg profile-id profile-id-set)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Record
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: Do not include `seeAlso`, `concepts`, `templates`, and `patterns`,
;; which are all optional properties.
;; The record constructor will populate these properties with `nil` values if
;; they're missing, causing the Profile to fail validation. (The vanilla Profile
;; `defrecord` serves as the supertype for all possible Profiles anyways.)

;; NOTE: Do not include the type property, as it would conflict with `core.type`
;; and be annoying. It is static anyhow.

(defrecord Profile [id
                    _context
                    conformsTo
                    prefLabel
                    definition
                    versions
                    author]
  p/FromInput
  (validate [profile]
    (validate-profile profile))

  p/JSONRepresentable
  (read-key-fn [_ k]
    (let [key-name (name k)]
      (keyword (if (= "@context" key-name) "_context" key-name))))
  (read-body-fn [_ json-result]
    (map->Profile json-result))
  (write-key-fn [_ k]
    (let [key-name (name k)]
      (if (= key-name "_context") "@context" key-name)))
  (write-body-fn [this]
    this))
