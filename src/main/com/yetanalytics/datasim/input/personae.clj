(ns com.yetanalytics.datasim.input.personae
  (:require [clojure.spec.alpha :as s]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.protocols :as p]
            [clojure.data.json :as json]
            [com.yetanalytics.datasim.util.xapi :as xapi])
  (:import [java.io Reader Writer]))



;; We model the input personae as an xAPI group.
;; It can be anonymous, but the name may be used in some way.

;; If functionality is added to express further groupings we'll have to revise
;; this strategy.

(s/def ::agent-id
  (s/and string?
         not-empty
         (fn [^String s]
           (or (.startsWith s "mbox::")
               (.startsWith s "account::")
               (.startsWith s "mbox_sha1sum::")
               (.startsWith s "openid::")))))

(s/def ::ifi-map
  (s/map-of ::agent-id
            ::xs/actor
            :min-count 1))

(def private-props
  "private properties that aren't on xapi groups"
  #{:ifi-map})

;; An open-validating group spec, ignores extra stuff we put in there.
;; TODO: Better validation w/o working around closed maps from xapi schema
(s/def ::personae
  (s/and
   (s/keys :req-un [::ifi-map])
   (s/conformer (fn [x]
                        (reduce-kv
                         (fn [m k v]
                           (if (or (private-props k)
                                   (nil? v))
                             m
                             (assoc m k v)))
                         {}
                         x)))
   ::xs/group))

(defn make-ifi-map
  "Given an xapi group, make a map of ifis"
  [{:keys [member] :as group}]
  (into {}
        (for [actor member
              :let [id (xapi/agent-id actor)]
              :when id]
          [id actor])))

(defrecord Personae [member
                     objectType
                     name

                     ;; prob not used
                     mbox
                     mbox_sha1sum
                     openid
                     account

                     ;; internal
                     ifi-map]
  p/FromInput
  (validate [this]
    (s/explain-data ::personae
                    this))

  p/Serializable
  (deserialize [this r]
    (map->Personae
     (let [g (json/read r :key-fn keyword)
           ifi-m (make-ifi-map g)]
       (assoc g :ifi-map ifi-m))))
  (serialize [this w])

  p/IdIndexed
  (get-id [this id]
    (get ifi-map id))
  (flat-map [this]
    ifi-map))
