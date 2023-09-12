(ns com.yetanalytics.datasim.input.model
  "Model input specs and parsing."
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.util.errors            :as errs]
            [com.yetanalytics.datasim.input.model.alignments :as alignments]
            [com.yetanalytics.datasim.input.model.personae   :as personae]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- distinct-personae?
  [model-maps]
  (let [personaes (map :personae model-maps)]
    (= (-> personaes count)
       (-> personaes distinct count))))

(s/def ::personae
  (s/every personae/persona-spec :kind vector? :min-count 1))

(s/def ::verbs
  (s/every alignments/verb-spec :kind vector?))

(s/def ::activities
  (s/every alignments/activity-spec :kind vector?))

(s/def ::activityTypes
  (s/every alignments/activity-type-spec :kind vector?))

(s/def ::patterns
  (s/every alignments/pattern-spec :kind vector?))

(s/def ::templates
  (s/every alignments/template-spec :kind vector?))

(s/def ::objectOverrides
  (s/every alignments/object-override-spec :kind vector?))

(def model-spec
  (s/keys :opt-un [::personae
                   ::verbs
                   ::activities
                   ::activityTypes
                   ::patterns
                   ::templates
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
