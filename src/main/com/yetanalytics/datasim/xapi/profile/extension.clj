(ns com.yetanalytics.datasim.xapi.profile.extension
  (:require [clojure.spec.alpha       :as s]
            [com.yetanalytics.schemer :as schemer]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::activity s/spec?)
(s/def ::context s/spec?)
(s/def ::result s/spec?)

(s/def ::extension-spec-map
  (s/keys :req-un [::activity ::context ::result]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef create-extension-spec-map
  :args (s/cat :type-iri-map map?)
  :ret ::extension-spec-map)

(defn create-extension-spec-map
  [type-iri-map]
  (let [ext->spec   #(some->> % :inlineSchema (schemer/schema->spec nil))
        reduce-ext  (partial reduce-kv
                             (fn [m id ext] (assoc m id (ext->spec ext)))
                             {})
        act-iri-map (get type-iri-map "ActivityExtension")
        ctx-iri-map (get type-iri-map "ContextExtension")
        res-iri-map (get type-iri-map "ResultExtension")]
    {:activity (reduce-ext act-iri-map)
     :context  (reduce-ext ctx-iri-map)
     :result   (reduce-ext res-iri-map)}))
