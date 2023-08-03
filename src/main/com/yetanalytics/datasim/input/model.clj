(ns com.yetanalytics.datasim.input.model
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.input.model.alignments       :as alignments]
            [com.yetanalytics.datasim.input.model.personae         :as personae]
            [com.yetanalytics.datasim.input.model.object-overrides :as obj-override]))

(defn- distinct-personae?
  [model-maps]
  (let [personaes (map :personae model-maps)]
    (= (-> personaes count)
       (-> personaes distinct count))))

(s/def ::personae personae/personae-spec)
(s/def ::alignments alignments/alignments-spec)
(s/def ::objectOverrides obj-override/object-overrides-spec)

(def model-map-spec
  (s/keys :opt-un [::personae
                   ::alignments
                   ::objectOverrides]))

(s/def ::model
  (s/and (s/every model-map-spec)
         distinct-personae?))
