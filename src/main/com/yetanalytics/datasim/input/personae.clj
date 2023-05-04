(ns com.yetanalytics.datasim.input.personae
  (:require [clojure.spec.alpha :as s]
            [clojure.walk :as w]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.datasim.xapi :as xapi]
            [com.yetanalytics.datasim.util :as u]
            [com.yetanalytics.datasim.util.errors :as errs]))

;; We model the input personae as an xAPI group.
;; It can be anonymous, but the name may be used in some way.

;; If functionality is added to express further groupings we'll have to revise
;; this strategy.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personae Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: This spec is unused
(s/def ::ifi-map
  (s/map-of ::xapi/agent-id
            ::xs/actor
            :min-count 1))

;; We cannot apply xapi-schema specs directly, as xapi-schema restricts which
;; properties can be in the Group, including the `role` property.
;;
;; We still use :agent and :group spec namespaces from xapi-schema.

(s/def ::role string?)

(s/def ::agent
  (s/keys :req-un [(or :agent/mbox
                       :agent/mbox_sha1sum
                       :agent/openid
                       :agent/account)]
          :opt-un [:agent/name
                   :agent/objectType
                   ::role]))

(s/def ::member
  (s/coll-of ::agent :kind vector? :min-count 1 :gen-max 3))

(s/def ::group
  (s/or :anonymous  (s/keys :req-un [:group/objectType
                                     (or :group/mbox
                                         :group/mbox_sha1sum
                                         :group/openid
                                         :group/account)]
                            :opt-un [:group/name ::member])
        :identified (s/keys :req-un [:group/objectType ::member]
                            :opt-un [:group/name])))

;; An open-validating group spec, ignores extra nils
(s/def ::personae
  (s/and (s/conformer u/remove-nil-vals)
         (s/conformer w/keywordize-keys w/stringify-keys)
         ::group))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personae Record
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Personae [member
                     objectType
                     mbox
                     mbox_sha1sum
                     openid
                     account]
  p/FromInput
  (validate [this]
    (when-some [ed (s/explain-data ::personae this)]
      (errs/explain-to-map-coll ::personae ed)))

  p/JSONRepresentable
  (read-key-fn [_this k]
    (keyword nil (name k)))
  (read-body-fn [_this json-result]
    (map->Personae
     json-result))
  (write-key-fn [_this k]
    (name k))
  (write-body-fn [this]
    (u/remove-nil-vals this)))
