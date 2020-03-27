(ns com.yetanalytics.datasim.input.profile
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.protocols :as p]
            [clojure.string :as cs]
            [com.yetanalytics.pan.objects.profile :as profile]
            [com.yetanalytics.pan.objects.pattern :as pat]
            [clojure.data.json :as json]
            [clojure.walk :as w])
  (:import [java.io Reader Writer]))

;; Spec overrides for Kelivin's lib:

(s/def ::pat/optional
  ::pat/id)

(s/def ::pat/oneOrMore
  ::pat/id)
(s/def ::pat/zeroOrMore
  ::pat/id)

(defrecord Profile [id
                    ;; type ;; that would conflict and be annoying, it's static anyhow
                    _context
                    conformsTo
                    prefLabel
                    definition
                    seeAlso
                    versions
                    author
                    concepts
                    templates
                    patterns]
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
    (map->Profile
     json-result))
  (write-key-fn [this k]
    (let [nn (name k)]
      (if (= nn "_context")
        "@context"
        nn)))
  (write-body-fn [this]
    this))
