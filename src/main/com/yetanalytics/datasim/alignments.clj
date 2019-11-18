(ns com.yetanalytics.datasim.alignments
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.pan.objects.profile :as profile]
            [com.yetanalytics.datasim.personae :as personae]
            [clojure.data.json :as json]
            [com.yetanalytics.datasim.iri :as iri]))

;; Alignments are expressed as a map of:
;; Actor IFI -> map
;;              IRI -> weight


(s/def ::alignment-map
  (s/map-of ::personae/agent-id
            (s/map-of iri/iri-spec
                      (s/double-in :min -1.0 :max 1.0
                                   :infinite? false
                                   :NaN? false))))

(s/def ::alignments
  (s/keys :req-un [::alignment-map]))


(defrecord Alignments [alignment-map]
  p/FromInput
  (validate [this]
    (s/explain-data ::alignments
                    this))

  p/Serializable
  (deserialize [this r]
    (map->Alignments
     {:alignment-map (json/read r :key-fn str)}))
  (serialize [this w])

  p/IdIndexed
  (get-id [this id]
    (get alignment-map id))
  (flat-map [this]
    alignment-map))
