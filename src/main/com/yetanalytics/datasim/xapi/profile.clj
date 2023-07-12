(ns com.yetanalytics.datasim.xapi.profile
  "Profile compilation.
   
   Creates a `profile-map` data structure that is used for the simulation
   of an entire Profile cosmos."
  (:require [clojure.spec.alpha :as s]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.input.profile    :as profile]
            [com.yetanalytics.datasim.input.parameters :as params]
            [com.yetanalytics.pan.objects.profile  :as pan-profile]
            [com.yetanalytics.pan.objects.concept  :as pan-concept]
            [com.yetanalytics.pan.objects.pattern  :as pan-pattern]
            [com.yetanalytics.pan.objects.template :as pan-template]
            [com.yetanalytics.datasim.xapi.profile.activity  :as act]
            [com.yetanalytics.datasim.xapi.profile.extension :as ext]
            [com.yetanalytics.datasim.xapi.profile.pattern   :as pat]
            [com.yetanalytics.datasim.xapi.profile.template  :as tmp]
            [com.yetanalytics.datasim.xapi.profile.verb      :as vrb]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::_profile-id ::pan-profile/id)

(def profile-types
  #{"Verb" "ActivityType" "AttachmentUsageTypes"
    "ActivityExtension" "ContextExtension" "ResultExtension"
    "StateResource" "AgentProfileResource" "ActivityProfileResource"
    "Activity" "StatementTemplate" "Pattern"})

(def profile-object-spec
  (s/and (s/keys :req [::_profile-id])
         (s/or :concept  ::pan-concept/concept
               :pattern  ::pan-pattern/pattern
               :template ::pan-template/template)))

(s/def ::type-iri-map
  (s/map-of profile-types (s/map-of ::xs/iri profile-object-spec)))

(s/def ::profile-map
  (s/keys :req-un [::type-iri-map
                   ::act/activity-map
                   ::vrb/verb-map
                   ::ext/extension-spec-map
                   ::tmp/statement-base-map
                   ::tmp/parsed-rules-map
                   ::pat/pattern-walk-fn]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profile -> IRI Map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- assoc-profile-id [{profile-id :id :as profile}]
  (let [update-object  (fn [obj] (assoc obj ::_profile-id profile-id))
        update-objects (fn [objects] (map update-object objects))]
    (-> profile
        (update :concepts update-objects)
        (update :patterns update-objects)
        (update :templates update-objects))))

(s/fdef profiles->type-iri-map
  :args (s/cat :profiles (s/every ::profile/profile))
  :ret ::type-iri-map)

(defn profiles->type-iri-map
  "Given a collection of profiles, return a map of type (e.g. \"Verb\",
   \"StatementTemplate\", \"Pattern\"), to a map from object ID IRIs to
   the objects themselves."
  [profiles]
  (let [profiles   (map assoc-profile-id profiles)
        concepts   (mapcat :concepts profiles)
        templates  (mapcat :templates profiles)
        patterns   (mapcat :patterns profiles)
        type-obj-m (cond-> (group-by :type concepts)
                     (not-empty templates) (assoc "StatementTemplate" templates)
                     (not-empty patterns)  (assoc "Pattern" patterns))
        objects->m (fn [objects]
                     (reduce
                      (fn [m {:keys [id] :as object}] (assoc m id object))
                      {}
                      objects))]
    (update-vals type-obj-m objects->m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primary Pattern Selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We don't put this in `profile.pattern` since this is part of the process
;; of creating the type-iri-map.

(s/fdef select-primary-patterns
  :args (s/cat :type-iri-map ::type-iri-map
               :params ::params/parameters)
  :ret ::type-iri-map)

(defn select-primary-patterns
  "Given `type-iri-map` and the `gen-profiles` and `gen-patterns` params,
   update the Pattern map to further specify primary patterns for generation.
   Primary patterns in this context must be specified by `gen-profiles` or
   `gen-patterns`, or else they will no longer be counted as primary patterns."
  [type-iri-map {:keys [gen-profiles gen-patterns]}]
  (let [?profile-set   (some-> gen-profiles not-empty set)
        ?pattern-set   (some-> gen-patterns not-empty set)
        primary-pat?   (fn [profile-id pattern-id]
                         (and (or (nil? ?profile-set)
                                  (contains? ?profile-set profile-id))
                              (or (nil? ?pattern-set)
                                  (contains? ?pattern-set pattern-id))))
        update-pattern (fn [{profile-id ::_profile-id
                             pattern-id :id
                             primary?   :primary
                             :as pattern}]
                         (cond-> pattern
                           primary?
                           (assoc :primary
                                  (primary-pat? profile-id pattern-id))))
        update-pat-map (fn [pattern-map]
                         (update-vals pattern-map update-pattern))]
    (update type-iri-map "Pattern" update-pat-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Putting it all Together
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef profiles->profile-map
  :args (s/cat :profiles       ::profile/profiles
               :pattern-params ::params/parameters
               :activity-seed  int?)
  :ret ::profile-map)

(defn profiles->profile-map
  "Create a map from `profiles` that contains the following:
   
   - `type-iri-map`: A map from object types to IDs to the object maps (i.e.
   Concepts, Statement Templates, and Patterns).
   - `activity-map`: A map from Activity Type IDs to Activity IDs to the
   Activity maps (in Statement, not Profile, form).
   - `verb-map`: A map from Verb IDs to Verbs (in Statement, not Profile,
   form).
   - `extension-spec-map`: A map from one of `:activity`, `:context`, or
   `:result` to an Extension ID to the Extension spec derived from its
   `inlineSchema property`.
   - `statement-base-map`: A map from Template IDs to the Template's
   xAPI Statement base, as derived from its determining properties and inScheme.
   - `parsed-rules-map`: A map from Template IDs to the Template's parsed
   rules.
   - `pattern-walk-fn`: A function that, when passed in `alignment` and `rng`
   arguments, generates a lazy sequence of visited Templates for a particular
   primary Pattern.
   
   Uses `pattern-params` to narrow down primary Patterns and `activity-seed` to
   generate additional Activity IDs in the cosmos."
  [profiles pattern-params activity-seed]
  (let [type-iri-map*      (profiles->type-iri-map profiles)
        type-iri-map       (select-primary-patterns type-iri-map* pattern-params)
        activity-map       (act/create-activity-map type-iri-map activity-seed)
        verb-map           (vrb/create-verb-map type-iri-map)
        extension-spec-map (ext/create-extension-spec-map type-iri-map)
        statement-base-map (tmp/create-statement-base-map type-iri-map)
        parsed-rules-map   (tmp/create-parsed-rules-map type-iri-map)
        pattern-walk-fn    (pat/create-pattern-walk-fn type-iri-map)
        profile-map*       {:type-iri-map       type-iri-map
                            :activity-map       activity-map
                            :verb-map           verb-map
                            :extension-spec-map extension-spec-map
                            :statement-base-map statement-base-map
                            :parsed-rules-map   parsed-rules-map
                            :pattern-walk-fn    pattern-walk-fn}]
    (update profile-map*
            :parsed-rules-map
            (partial tmp/update-parsed-rules-map profile-map*))))
