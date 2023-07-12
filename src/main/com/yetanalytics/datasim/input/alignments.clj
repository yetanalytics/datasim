(ns com.yetanalytics.datasim.input.alignments
  "Alignment input specs and parsing."
  (:require [clojure.spec.alpha :as s]
            [clojure.walk       :as w]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.datasim.xapi.actor  :as agent]
            [com.yetanalytics.datasim.util.errors :as errs]
            [clojure.spec.gen.alpha :as sgen]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Alignment: Map of Component, Weight, and Object Override properties

;; Due to limitations of keywords, we cannot have IRI keys, limiting extensions
(defn- no-iri-keys?
  "Returns false if there exists a key made from an IRI, e.g.
   (name :https://foo.org) => \"/foo.org\""
  [obj]
  (cond
    (map? obj)
    (if-not (->> obj vals (map no-iri-keys?) (some false?))
      (->> obj keys (some (partial re-matches #".*/.*")) not)
      false)
    (vector? obj)
    (every? no-iri-keys? obj)
    :else
    true))

(s/def ::objectOverride
  (s/with-gen (s/and (s/conformer w/stringify-keys w/keywordize-keys)
                     no-iri-keys?
                     :statement/object) ; from xapi-schema
    (fn [] ; TODO: More comprehensive gen
      (sgen/return
       {"id" "http://example.com/object-override"
        "objectType" "Activity"}))))

(s/def ::component
  ::xs/iri)

(s/def ::weight
  (s/double-in :min -1.0 :max 1.0
               :infinite? false
               :NaN? false))

(s/def ::alignment
  (s/keys :req-un [::component
                   ::weight]
          :opt-un [::objectOverride]))

;; Actor-Alignment: Map of Actor ID, Actor Type, and collection of Alignments

(s/def ::id string?)

(s/def ::type #{"Agent" "Group" "Role"})

(s/def ::alignments (s/every ::alignment))

(defmulti actor-alignment? :type)

(defmethod actor-alignment? "Agent" [_]
  (fn [{agent-id :id}] (s/valid? ::agent/actor-ifi agent-id)))

(defmethod actor-alignment? "Group" [_]
  (constantly true))

(defmethod actor-alignment? "Role" [_]
  (constantly true))

(s/def ::actor-alignment
  (s/and (s/keys :req-un [::id ::type ::alignments])
         (s/multi-spec actor-alignment? :type)))

;; Alignment-vector: Collection of Actor-Alignment

(s/def ::alignment-vector
  (s/every ::actor-alignment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn validate-alignments
  [alignments]
  (some->> (s/explain-data ::alignment-vector alignments)
           (errs/explain-to-map-coll ::alignment-vector)))
