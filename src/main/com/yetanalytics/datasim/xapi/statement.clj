(ns com.yetanalytics.datasim.xapi.statement
  "Statement generation."
  (:require [clojure.spec.alpha :as s]
            [clojure.walk       :as w]
            [java-time.api      :as jt]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.datasim.math.random            :as random]
            [com.yetanalytics.datasim.model                  :as model]
            [com.yetanalytics.datasim.xapi.profile           :as profile]
            [com.yetanalytics.datasim.xapi.profile.template  :as t]
            [com.yetanalytics.datasim.xapi.rule              :as rule]
            [com.yetanalytics.datasim.xapi.statement.healing :as heal])
  (:import [java.time LocalDateTime Duration]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Inputs

(s/def ::actor ::xs/actor)

(s/def ::alignments ::model/alignments)

(s/def ::object-overrides ::model/object-overrides)

(s/def ::seed ::random/seed)

;; TODO: subregistration from :pattern-ancestors logic
;; -> "https://w3id.org/xapi/profiles/extensions/subregistration"
;;    -> subregistration extension key
;;    -> only necessary when a primary pattern contains another primary pattern
(s/def ::sub-registration
  any?) ; TODO: replace `any?` with real spec

(s/def ::timestamp #(instance? LocalDateTime %))

(s/def ::timezone string?)

(s/def ::time-since-last #(instance? Duration %))

(s/def ::template ::t/template)

(s/def ::inputs
  (s/merge ::profile/profile-map
           (s/keys :req-un [::actor
                            ::alignments
                            ::seed
                            ::timestamp
                            ::timezone
                            ::time-since-last
                            ::template
                            ::registration]
                   :opt-un [::object-overrides
                            ::sub-registration])))

;; Metadata

(s/def ::time-ms pos-int?)

(s/def ::time-since-ms pos-int?)

(s/def ::meta (s/keys :req-un [::time-ms ::time-since-ms ::template]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Object Override
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- select-object-override
  [rng {:keys [weights objects]}]
  (some->> objects
           not-empty
           (random/choose rng weights)
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
  [template-base {:keys [timestamp timezone registration]} rng]
  (-> template-base
      (assoc-in ["id"] (random/rand-uuid rng))
      (assoc-in ["timestamp"] (str (jt/instant timestamp timezone)))
      (assoc-in ["context" "registration"] registration)))

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
   | `alignments`         | The alignments map used for choosing Statement elements.
   | `object-overrides`   | The map of objects that override Template-specified objects.
   | `template`           | The Template used to generate this Statement.
   | `registration`       | The registration UUID for the overall generated Statement sequence.
   | `seed`               | The seed used to generate random numbers during generation.
   | `pattern-ancestors`  | The coll of Patterns visited en route to `template`.
   | `sub-registration`   | (NOT YET IMPLEMENTED) The sub-registration object of the Statement.
   | `timestamp`          | The LocalDateTime timestamp at which the statement occurs; becomes the timestamp value.
   | `timezone`           | The time zone string in whicht the statement event occurs.
   | `time-since-last`    | The Duration since the previous statement.
   
   Returns a Statement with `template`, `time-ms`, and `duration-ms` as metadata."
  #_{:clj-kondo/ignore [:unused-binding]} ; unused args are used in helper fns
  [{:keys [type-iri-map
           verb-map
           activity-map
           extension-map
           statement-base-map
           parsed-rules-map
           actor
           alignments
           object-overrides
           template
           seed
           pattern-ancestors
           registration
           sub-registration
           timestamp
           timezone
           time-since-last]
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
        object-override (select-object-override rng object-overrides)
        template-rules* (remove-object-rules template-rules object-override)
        timestamp-inst  (jt/instant timestamp timezone)
        statement-meta  {:timestamp       timestamp-inst
                         :time-ms         (.toEpochMilli timestamp-inst)
                         :time-since-last time-since-last
                         :time-since-ms   (.toMillis time-since-last)
                         :template        template}]
    (-> template-base
        (base-statement inputs rng)
        (apply-object-override object-override)
        (rule/apply-inclusion-rules template-rules* rng)
        (heal/complete-statement inputs rng)
        (rule/apply-exclusion-rules template-rules*)
        (with-meta statement-meta))))
