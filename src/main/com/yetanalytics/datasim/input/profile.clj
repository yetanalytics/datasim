(ns com.yetanalytics.datasim.input.profile
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.protocols :as p]
            [clojure.string :as cs]
            [com.yetanalytics.pan :as pan]
            [com.yetanalytics.pan.objects.profile :as profile]
            [clojure.walk :as w]
            [com.yetanalytics.datasim.util :as u])
  (:import [java.io Reader Writer]))

;; NOTE: Do not include optional args seeAlso, concepts, templates, and patterns
;; The record constructor will populate these properties with nils if they're
;; missing, causing the Profile to fail validation. (The vanilla Profile
;; defrecord serves as the supertype for all possible Profiles anyways.)

;; NOTE: Do not include the type property, as it would conflict and be annoying.
;; It is static anyhow.

(defrecord Profile [id
                    _context
                    conformsTo
                    prefLabel
                    definition
                    versions
                    author]
  p/FromInput
  (validate [this]
    (when-some [err-map (pan/validate-profile this
                                              :syntax? true
                                              :pattern-rels? true)]
      err-map))

  p/JSONRepresentable
  (read-key-fn [_this k]
    (let [kn (name k)]
      (keyword nil (if (= "@context" kn) "_context" kn))))
  (read-body-fn [_this json-result]
    (map->Profile json-result))
  (write-key-fn [_this k]
    (let [nn (name k)]
      (if (= nn "_context") "@context" nn)))
  (write-body-fn [this]
    this))
