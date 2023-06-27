(ns com.yetanalytics.datasim.input.profile
  "Profile input parsing."
  (:require [com.yetanalytics.pan                 :as pan]
            [com.yetanalytics.datasim.protocols   :as p]
            [com.yetanalytics.datasim.util.errors :as errs]))

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
  (validate [this]
    (let [prof-errs (pan/validate-profile this
                                          :syntax? true
                                          :result :type-path-string)]
      (errs/type-path-string-m->map-coll id prof-errs)))

  p/JSONRepresentable
  (read-key-fn [_ k]
    (let [kn (name k)]
      (keyword nil (if (= "@context" kn) "_context" kn))))
  (read-body-fn [_ json-result]
    (map->Profile json-result))
  (write-key-fn [_ k]
    (let [nn (name k)]
      (if (= nn "_context") "@context" nn)))
  (write-body-fn [this]
    this))
