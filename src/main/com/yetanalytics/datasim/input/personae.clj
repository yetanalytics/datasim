(ns com.yetanalytics.datasim.input.personae
  "Personae input specs and parsing."
  (:require [clojure.spec.alpha :as s]
            [clojure.walk       :as w]
            [com.yetanalytics.datasim.xapi.actor  :as agent]
            [com.yetanalytics.datasim.util.errors :as errs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We model the input personae as an xAPI group.
;; It can be anonymous, but the name may be used in some way.

;; If functionality is added to express further groupings we'll have to revise
;; this strategy.

;; Note: We cannot apply xapi-schema specs directly, as xapi-schema restrict
;; which properties can be in the Group, including the `role` property.
;; We still use `agent` and `group` spec namespaces from xapi-schema.

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

(defn- remove-nil-vals
  "Remove nil values from an associative structure. Does not recurse."
  [m]
  (reduce-kv
   (fn [m* k v] (cond-> m* (some? v) (assoc k v)))
   {}
   m))

;; An open-validating group spec, ignores extra nils
(s/def ::personae
  (s/and (s/conformer remove-nil-vals)
         (s/conformer w/keywordize-keys w/stringify-keys)
         ::group))

(defn- distinct-member-ids?
  [personaes]
  (let [member-ids (->> personaes
                        (map :member)
                        (apply concat)
                        (map agent/actor-ifi))]
    (= (-> member-ids count)
       (-> member-ids distinct count))))

(s/def ::personae-array
  (s/and (s/every ::personae :min-count 1 :into [])
         distinct-member-ids?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn validate-personae
  [personae]
  (some->> (s/explain-data ::personae personae)
           (errs/explain-to-map-coll ::personae)))

(defn validate-personae-array
  [personae-array]
  (some->> (s/explain-data ::personae-array personae-array)
           (errs/explain-to-map-coll ::personae-array)))
