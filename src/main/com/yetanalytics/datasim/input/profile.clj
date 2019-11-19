(ns com.yetanalytics.datasim.input.profile
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.protocols :as p]
            [clojure.string :as cs]
            [com.yetanalytics.pan.objects.profile :as profile]
            [clojure.data.json :as json])
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
    (s/explain-data ::profile/profile this))

  p/Serializable
  (deserialize [this r]
    (map->Profile
     (json/read r :key-fn (fn [^String k]
                            (keyword nil
                                     (cs/replace k \@ \_))))))
  (serialize [this w]))
