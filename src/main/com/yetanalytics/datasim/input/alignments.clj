(ns com.yetanalytics.datasim.input.alignments
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.pan.objects.profile :as profile]
            [clojure.data.json :as json]
            [com.yetanalytics.datasim.iri :as iri]
            [com.yetanalytics.datasim.xapi :as xapi]))

;; Alignments are expressed as a map of:
;; Actor IFI -> map
;;              IRI -> weight


(s/def ::alignment-map
  (s/map-of ::xapi/agent-id
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
  p/JSONRepresentable
  (read-key-fn [this k]
    (str k))
  (read-value-fn [this k v]
    v)
  (read-body-fn [this json-result]
    (map->Alignments
     {:alignment-map json-result}))
  (write-key-fn [this k]
    (name k))
  (write-value-fn [this k v]
    v)

  p/Serializable
  (deserialize [this r]
    (map->Alignments
     {:alignment-map (json/read r :key-fn str)}))
  (serialize [this w]))
