(ns com.yetanalytics.datasim.profile
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.io :as dio]
            [com.yetanalytics.datasim.protocols :as p]
            [clojure.string :as cs]
            [com.yetanalytics.pan.objects.profile :as profile]))

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
  (read-in [this location]
    (map->Profile (dio/read-location
                   location
                   :fmt :json
                   :parser-opts [:key-fn (fn [^String k]
                                           (keyword nil
                                                    (cs/replace k \@ \_)))])))
  (validate [this]
    (s/explain-data ::profile/profile this)))
