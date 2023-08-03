(ns com.yetanalytics.datasim.input.model
  "Model input specs and parsing."
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.util.errors                  :as errs]
            [com.yetanalytics.datasim.input.model.alignments       :as alignments]
            [com.yetanalytics.datasim.input.model.personae         :as personae]
            [com.yetanalytics.datasim.input.model.object-overrides :as obj-override]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- distinct-personae?
  [model-maps]
  (let [personaes (map :personae model-maps)]
    (= (-> personaes count)
       (-> personaes distinct count))))

(s/def ::personae personae/personae-spec)
(s/def ::alignments alignments/alignments-spec)
(s/def ::objectOverrides obj-override/object-overrides-spec)

(def model-spec
  (s/keys :opt-un [::personae
                   ::alignments
                   ::objectOverrides]))

(s/def ::models
  (s/and (s/every model-spec)
         distinct-personae?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn validate-models
  [alignments]
  (some->> (s/explain-data ::models alignments)
           (errs/explain-to-map-coll ::models)))
