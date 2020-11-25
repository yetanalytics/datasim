(ns com.yetanalytics.datasim.input.alignments
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.pan.objects.profile :as profile]
            [com.yetanalytics.datasim.iri :as iri]
            [com.yetanalytics.datasim.xapi :as xapi]))

;; Alignments are expressed as a map of:
;; Actor IFI -> map
;;              IRI -> weight

(s/def :alignment-map/actor-alignment
  (s/map-of iri/iri-spec
            (s/double-in :min -1.0 :max 1.0
                         :infinite? false
                         :NaN? false)))

(s/def ::alignment-map
  (s/map-of ::xapi/agent-id
            :alignment-map/actor-alignment))

(s/def ::alignments
  (s/keys :req-un [::alignment-map]))




(defrecord Alignments [alignment-map]
  p/FromInput
  (validate [this]
    (s/explain-data ::alignment-map
                    this))
  p/JSONRepresentable
  (read-key-fn [this k]
    (name k))
  (read-body-fn [this json-result]
    (map->Alignments
     {:alignment json-result}))
  (write-key-fn [this k]
    (name k))
  (write-body-fn [this]
    alignment-map))


(comment


  (gen/sample (s/gen :alignment-map/actor-alignment))



  (clojure.spec.alpha/def :alignment-map/actor-alignment
    (clojure.spec.alpha/map-of string?
              (clojure.spec.alpha/double-in :min -1.0 :max 1.0
                           :infinite? false
                           :NaN? false)))

  (clojure.spec.alpha/def ::alignment-map
    (clojure.spec.alpha/map-of string?
                               :alignment-map/actor-alignment))

  (clojure.spec.alpha/def ::alignments
    (clojure.spec.alpha/keys :req-un [::alignment-map]))

  (clojure.spec.alpha/explain-data ::alignment-map {"mbox::mailto:cliff@yetanalytics.com" {"https://www.google.com" 0.6}})

  (clojure.spec.alpha/explain-data ::alignments {"string" {"string"  1.0}})


  )
