(ns com.yetanalytics.datasim.xapi.profile.verb
  "Creation of `verb-map` for Profile compilation."
  (:require [clojure.spec.alpha :as s]
            [clojure.walk       :as w]
            [xapi-schema.spec   :as xs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::verb-map
  (s/map-of ::xs/iri ::xs/verb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- profile->statement-verb
  [{:keys [id prefLabel]}]
  {"id"      id
   "display" (w/stringify-keys prefLabel)})

;; TODO: Bring in type-iri-map spec using :as-alias in Clojure 1.11
(s/fdef create-verb-map
  :args (s/cat :type-iri-map map?)
  :ret ::verb-map)

(defn create-verb-map
  "Create a map of verb IDs to Statement verbs out of Profile Verbs from
   `type-iri-map`."
  [type-iri-map]
  (reduce-kv (fn [m id verb] (assoc m id (profile->statement-verb verb)))
             {}
             (get type-iri-map "Verb")))
