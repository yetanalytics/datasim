(ns com.yetanalytics.datasim.xapi.profile.extension
  "Creation of `extension-spec-map` for Profile compilation."
  (:require [clojure.spec.alpha          :as s]
            [com.yetanalytics.schemer    :as schemer]
            [com.yetanalytics.pan.axioms :as ax]
            [com.yetanalytics.datasim.xapi.profile :as-alias profile]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def json-schema-spec
  (s/or :keyword (s/and keyword? s/get-spec)
        :spec s/spec?))

(s/def ::activity (s/map-of ::ax/iri (s/nilable json-schema-spec)))
(s/def ::context (s/map-of ::ax/iri (s/nilable json-schema-spec)))
(s/def ::result (s/map-of ::ax/iri (s/nilable json-schema-spec)))

(s/def ::extension-spec-map
  (s/keys :req-un [::activity ::context ::result]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef create-extension-spec-map
  :args (s/cat :type-iri-map ::profile/type-iri-map)
  :ret ::extension-spec-map)

(defn create-extension-spec-map
  [type-iri-map]
  (let [ext->spec   (fn [ext]
                      (some->> ext :inlineSchema (schemer/schema->spec nil)))
        reduce-exts (fn [ext-map]
                      (update-vals ext-map ext->spec))
        act-iri-map (get type-iri-map "ActivityExtension")
        ctx-iri-map (get type-iri-map "ContextExtension")
        res-iri-map (get type-iri-map "ResultExtension")]
    {:activity (reduce-exts act-iri-map)
     :context  (reduce-exts ctx-iri-map)
     :result   (reduce-exts res-iri-map)}))
