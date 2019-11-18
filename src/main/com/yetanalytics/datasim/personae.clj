(ns com.yetanalytics.datasim.personae
  (:require [clojure.spec.alpha :as s]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.protocols :as p]
            [clojure.data.json :as json])
  (:import [java.io Reader Writer]))

;; We model the input personae as an xAPI group.
;; It can be anonymous, but the name may be used in some way.

;; If functionality is added to express further groupings we'll have to revise
;; this strategy.
(defrecord Personae [member
                     objectType
                     name

                     ;; prob not used
                     mbox
                     mbox_sha1sum
                     openid
                     account]
  p/FromInput
  (validate [this]
    (s/explain-data ::xs/group
                    ;; Remove the nils from being a record
                    (reduce-kv
                     (fn [m k v]
                       (if (nil? v)
                         m
                         (assoc m k v)))
                     {}
                     this)))

  p/Serializable
  (deserialize [this r]
    (map->Personae
     (json/read r :key-fn keyword)))
  (serialize [this w]))
