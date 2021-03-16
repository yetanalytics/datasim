(ns com.yetanalytics.datasim.input.alignments
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.pan.objects.profile :as profile]
            [com.yetanalytics.datasim.iri :as iri]
            [com.yetanalytics.datasim.xapi :as xapi]))

;;Alignment: Map of Component and Weight

(s/def ::component
  iri/iri-spec)

(s/def ::weight
  (s/double-in :min -1.0 :max 1.0
               :infinite? false
               :NaN? false))

(s/def ::alignment
  (s/keys :req-un [::component
                   ::weight]))

;; Actor-Alignment: Map of Actor to collection of Alignments

(s/def ::id string?)

(s/def ::type #{"Agent" "Group" "Role"})

(s/def ::alignments (s/every ::alignment))

(defmulti actor-alignment? :type)

(defmethod actor-alignment? "Agent" [_]
  (fn [align] (->> align :id (s/valid? ::xapi/agent-id))))

(defmethod actor-alignment? :default [_] ; "Group" and "Role"
  (constantly true))

(s/def ::actor-alignment
  (s/and (s/keys :req-un [::id ::type ::alignments])
         (s/multi-spec actor-alignment? :type)))

;;Alignment-vector: Collection of Actor-Alignment

(s/def ::alignment-vector
  (s/every ::actor-alignment))

;;Alignment input
(s/def ::alignments-input
  (s/keys :req-un [::alignment-vector]))

(defrecord Alignments [alignment-vector]
  p/FromInput
  (validate [this]
    (s/explain-data ::alignments-input
                    this))
  p/JSONRepresentable
  (read-key-fn [this k]
    (keyword nil (name k)))
  (read-body-fn [this json-result]
    (map->Alignments
     {:alignment-vector json-result}))
  (write-key-fn [this k]
    (name k))
  (write-body-fn [this]
    alignment-vector))
