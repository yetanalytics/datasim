(ns com.yetanalytics.datasim.input.personae
  (:require [clojure.spec.alpha :as s]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.datasim.xapi :as xapi]
            [com.yetanalytics.datasim.util :as u])
  (:import [java.io Reader Writer]))



;; We model the input personae as an xAPI group.
;; It can be anonymous, but the name may be used in some way.

;; If functionality is added to express further groupings we'll have to revise
;; this strategy.



(s/def ::ifi-map
  (s/map-of ::xapi/agent-id
            ::xs/actor
            :min-count 1))

;; An open-validating group spec, ignores extra nils
(s/def ::personae
  (s/and
   (s/conformer u/remove-nil-vals)
   ::xs/group))



(defrecord Personae [member
                     objectType
                     ;; name

                     ;; prob not used
                     mbox
                     mbox_sha1sum
                     openid
                     account]
  p/FromInput
  (validate [this]
    (s/explain-data ::personae
                    this))

  p/JSONRepresentable
  (read-key-fn [this k]
    (keyword nil (name k)))
  (read-body-fn [this json-result]
    (map->Personae
     json-result))
  (write-key-fn [this k]
    (name k))
  (write-body-fn [this]
    (u/remove-nil-vals this)))
