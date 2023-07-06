(ns com.yetanalytics.datasim.xapi.statement
  "Generate Statements"
  (:require [clojure.spec.alpha :as s]
            [clojure.walk       :as w]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.datasim.math.random            :as random]
            [com.yetanalytics.datasim.input.alignments       :as alignments]
            [com.yetanalytics.datasim.xapi.profile           :as profile]
            [com.yetanalytics.datasim.xapi.profile.template  :as t]
            [com.yetanalytics.datasim.xapi.registration      :as reg]
            [com.yetanalytics.datasim.xapi.rule              :as rule]
            [com.yetanalytics.datasim.xapi.statement.healing :as heal])
  (:import [java.time Instant]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Inputs

;; The actor for the statement (may have to stringify keys?)
(s/def ::actor ::xs/actor)

;; The alignment, a map of IRI to a map of `:weights` from -1.0 to 1.0 and
;; a nilable `:object-override` object.
(s/def ::alignment ::alignments/alignment)

;; Simulation time, in ms since epoch.
(s/def ::sim-t pos-int?)

;; A seed to generate with. Note that if you're calling more seeded
;; generators, you'll need to make a seed from this one for each.
(s/def ::seed ::random/seed)

;; TODO: subregistration from :pattern-ancestors logic
;; -> "https://w3id.org/xapi/profiles/extensions/subregistration"
;;    -> subregistration extension key
;;    -> only necessary when a primary pattern contains another primary pattern
(s/def ::sub-registration
  any?) ; TODO: replace `any?` with real spec

(s/def ::inputs
  (s/merge ::profile/profile-map
           ::reg/registration-map
           (s/keys :req-un [::actor ::alignment ::sim-t ::seed]
                   :opt-un [::sub-registration])))

;; Metadata

;; The duration, in milliseconds, of the returned statement.
;; This is so we can resume processing AFTER the statement timestamp + duration.
(s/def ::end-ms pos-int?)

(s/def ::timestamp-ms pos-int?)

(s/def ::meta
  (s/keys :req-un [::timestamp-ms
                   ::end-ms]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- time-ms->timestamp
  [time-ms]
  (.toString (Instant/ofEpochMilli time-ms)))

(def additional-time-ms-mean 600000.0)

(def additional-time-ms-sd 0.5)

(defn- end-time-ms [start-time-ms rng]
  (->> (random/rand-gauss rng additional-time-ms-mean additional-time-ms-sd)
       long
       (+ start-time-ms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Object Override
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- select-object-override
  [rng alignment]
  (some->> alignment
           not-empty
           keys
           (random/choose rng alignment)
           (get alignment)
           :object-override
           w/stringify-keys))

(defn- remove-object-rules
  [rules ?object-override]
  (cond->> rules
    ?object-override
    (filterv (partial rule/property-rule? "object"))))

(defn- apply-object-override
  [statement ?object-override]
  (cond-> statement
    ?object-override
    (assoc "object" ?object-override)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- base-statement
  [template-base {:keys [sim-t registration]} rng]
  (-> template-base
      (assoc-in ["id"] (random/rand-uuid rng))
      (assoc-in ["timestamp"] (time-ms->timestamp sim-t))
      (assoc-in ["context" "registration"] registration)))

;; TODO: Add duration ms in the meta?
(defn- statement-meta
  [template sim-t rng]
  {:end-ms       (end-time-ms sim-t rng)
   :timestamp-ms sim-t
   :template     template})

(s/fdef generate-statement
  :args (s/cat :args-map ::inputs)
  :ret (s/and ::xs/statement
              (s/conformer meta)
              ::meta))

;; TODO: ERROR/WARNING if generated statement fails spec (e.g. a required
;; property is excluded)
(defn generate-statement
  "Generate a single xAPI Statement. The `inputs` map accepts the following
   map arguments:
   
   | Argument | Description
   | ---      | ---
   | `type-iri-map`       | A map from Profile object types to IDs to the object maps.
   | `activity-map`       | A map from Activity Type IDs to Activity IDs to the Activity maps.
   | `statement-base-map` | A map from Template IDs to the Statement base created using `template->statement-base`.
   | `parsed-rules-map`   | A map from Template IDs to its parsed rules parsed using `template->parsed-rules`.
   | `actor`              | The Actor used in the Statement.
   | `alignment`          | The alignment map used for choosing Statement elements.
   | `template`           | The Template used to generate this Statement.
   | `registration`       | The registration UUID for the overall generated Statement sequence.
   | `seed`               | The seed used to generate random numbers during generation.
   | `pattern-ancestors`  | The coll of Patterns visited en route to `template`.
   | `sub-registration`   | (NOT YET IMPLEMENTED) The sub-registration object of the Statement.
   | `sim-t`              | The time (in ms) of this simulation.
   
   Returns a Statement with a map of the following as metadata:

   | Metadata       | Description
   | ---            | ---
   | `end-ms`       | The time (in epoch ms) after which the `actor` can continue.
   | `timestamp-ms` | The time of the timestamp (in epoch ms). It must be `> sim-t` and `<= end-ms`; for simplicity we just make it `sim-t`.
   | `template`     | The Template used to generate this Statement"
  #_{:clj-kondo/ignore [:unused-binding]} ; unused args are used in helper fns
  [{:keys [type-iri-map
           verb-map
           activity-map
           extension-map
           statement-base-map
           parsed-rules-map
           actor
           alignment
           template
           seed
           pattern-ancestors
           registration
           sub-registration
           sim-t]
    :as inputs}]
  (let [;; Template Prep
        template-id
        (:id template)
        template-base
        (or (get statement-base-map template-id)
            (t/template->statement-base template))
        template-rules
        (or (get parsed-rules-map template-id)
            (->> template
                 t/template->parsed-rules
                 (rule/add-rules-valuegen inputs)))
        ;; Basics
        rng             (random/seed-rng seed)
        object-override (select-object-override rng alignment)
        template-rules* (remove-object-rules template-rules object-override)]
    (-> template-base
        (base-statement inputs rng)
        (apply-object-override object-override)
        (rule/apply-inclusion-rules template-rules* rng)
        (heal/complete-statement inputs rng)
        (rule/apply-exclusion-rules template-rules*)
        (with-meta (statement-meta template sim-t rng)))))
