(ns com.yetanalytics.datasim.input.profile
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.protocols :as p]
            [clojure.string :as cs]
            [com.yetanalytics.pan.objects.profile :as profile]
            [clojure.walk :as w]
            [com.yetanalytics.datasim.util :as u])
  (:import [java.io Reader Writer]))

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
    (s/explain-data ::profile/profile (u/remove-nil-vals this)))

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
    (u/remove-nil-vals this)))
