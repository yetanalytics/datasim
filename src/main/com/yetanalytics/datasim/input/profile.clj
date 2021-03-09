(ns com.yetanalytics.datasim.input.profile
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.protocols :as p]
            [clojure.string :as cs]
            [com.yetanalytics.pan.objects.profile :as profile]
            [clojure.walk :as w]
            [com.yetanalytics.datasim.util :as u])
  (:import [java.io Reader Writer]))

;; NOTE: Do not include optional args seeAlso, concepts, templates, and patterns
;; The record constructor will populate these properties with nils, causing
;; the Profile to fail validation.

(defrecord Profile [id
                    ;; type ;; that would conflict and be annoying, it's static anyhow
                    _context
                    conformsTo
                    prefLabel
                    definition
                    versions
                    author]
  p/FromInput
  (validate [this]
    (s/explain-data ::profile/profile this))

  p/JSONRepresentable
  (read-key-fn [this k]
    (let [kn (name k)]
      (keyword nil
               (if (= "@context" kn)
                 "_context"
                 kn))))
  (read-body-fn [this json-result]
    (map->Profile json-result))
  (write-key-fn [this k]
    (let [nn (name k)]
      (if (= nn "_context")
        "@context"
        nn)))
  (write-body-fn [this]
    this))
