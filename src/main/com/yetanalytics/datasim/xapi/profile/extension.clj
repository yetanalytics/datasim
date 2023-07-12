(ns com.yetanalytics.datasim.xapi.profile.extension
  "Creation of `extension-spec-map` for Profile compilation."
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

;; TODO: Bring in type-iri-map spec using :as-alias in Clojure 1.11
(s/fdef create-extension-spec-map
  :args (s/cat :type-iri-map map?)
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
