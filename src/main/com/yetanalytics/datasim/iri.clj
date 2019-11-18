(ns com.yetanalytics.datasim.iri
  (:require [clojure.spec.alpha :as s])
  (:import [org.apache.jena.iri Violation IRIFactory IRI]))

(defonce ^IRIFactory iri-factory
  (IRIFactory/semanticWebImplementation))

(defn check-iri-string
  "Given a string, check if it is a valid IRI, returning nil if so. Otherwise,
  Return violations"
  [^String s & {:keys [^Boolean include-warnings?]
                :or {include-warnings? false}}]
  (let [^IRI iri (.create iri-factory s)]
    (when (.hasViolation iri include-warnings?)
      (for [^Violation v (iterator-seq (.violations iri include-warnings?))]
        {:code (.getViolationCode v)
         :type (if (.isError v)
                 ::error
                 ::warning)
         :component (.component v)
         :message-short (.getShortMessage v)
         :message-long (.getLongMessage v)
         ;; :specification (.getSpecificationURL v) ;; NYI
         }))))

(def iri-spec
  (s/and string?
         #(nil? (check-iri-string %))))
