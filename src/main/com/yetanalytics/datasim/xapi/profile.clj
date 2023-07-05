(ns com.yetanalytics.datasim.xapi.profile
  (:require [clojure.spec.alpha :as s]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.input.parameters :as params]
            [com.yetanalytics.pan.objects.profile :as profile]
            [com.yetanalytics.pan.objects.concept :as concept]
            [com.yetanalytics.pan.objects.pattern :as pattern]
            [com.yetanalytics.pan.objects.template :as template]
            [com.yetanalytics.datasim.xapi.statement :as stmt]
            [com.yetanalytics.datasim.xapi.activity :as activity]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::_profile-id ::profile/id)

(s/def ::iri-map
  (s/map-of ::xs/iri
            ;; TODO: using s/and or s/merge to add ::_profile-id won't work
            ;; It will be present and should be expected
            (s/or :concept ::concept/concept
                  :pattern ::pattern/pattern
                  :template ::template/template)))

(def profile-types
  #{"Verb" "ActivityType" "AttachmentUsageTypes"
    "ActivityExtension" "ContextExtension" "ResultExtension"
    "StateResource" "AgentProfileResource" "ActivityProfileResource"
    "Activity" "StatementTemplate" "Pattern"})

(def profile-object-spec
  (s/or :concept  ::concept/concept
        :pattern  ::pattern/pattern
        :template ::template/template))

(s/def ::type-iri-map
  (s/map-of profile-types (s/map-of ::xs/iri profile-object-spec)))

;; TODO: Consolidate these specs with those in `xapi.statement`
(s/def ::seed int?)
(s/def ::pattern-ancestors (s/every ::pattern/pattern))

(s/def ::registration-map
  (s/keys :req-un [::template/template
                   ::xs/registration
                   ::seed
                   ::pattern-ancestors]))

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
  (let [profiles  (map assoc-profile-id profiles)
        concepts  (mapcat :concepts profiles)
        templates (mapcat :templates profiles)
        patterns  (mapcat :patterns profiles)]
    (->> (cond-> (group-by :type concepts)
           (not-empty templates) (assoc "StatementTemplate" templates)
           (not-empty patterns)  (assoc "Pattern" patterns))
         (reduce-kv
          (fn [m type objects]
            (->> objects
                 (reduce (fn [m* {:keys [id] :as object}]
                           (assoc m* id object))
                         {})
                 (assoc m type)))
          {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profile Templates Prep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::statement-base-map
  (s/map-of ::template/id map?))

(s/fdef profiles->base-statement-map
  :args (s/cat :profiles (s/every ::profile/profile))
  :ret ::statement-base-map)

(defn profiles->base-statement-map
  "Given `profiles`, return a map from those profiles' Statement Template
   IDs to the base xAPI Statements they form."
  [profiles]
  (let [template-coll        (mapcat :templates profiles)
        ->id-statement-pairs (juxt :id stmt/template->statement-base)]
    (->> template-coll (map ->id-statement-pairs) (into {}))))

;; TODO: More precise activity-map and parsed-rule specs

(s/def ::parsed-rules-map
  (s/map-of ::template/id map?))

(s/fdef profiles->parsed-rule-map
  :args (s/cat :profiles (s/every ::profile/profile)
               :type-iri-map ::type-iri-map
               :activity-map map?)
  :ret ::parsed-rules-map)

(defn profiles->parsed-rule-map
  "Given `profiles`, as well as `type-iri-map` and `activity-map` (the latter
   of which are ultimately derived from `profiles`), return a map from template
   IDs to those Statement Template's parsed rules"
  [profiles]
  (let [template-coll    (mapcat :templates profiles)
        ->id-rules-pairs (juxt :id stmt/template->parsed-rules)]
    (->> template-coll (map ->id-rules-pairs) (into {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primary Pattern Selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef select-primary-patterns
  :args (s/cat :iri-map ::iri-map
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
                                  (primary-pat? profile-id pattern-id))))]
    ;; TODO: Use clojure.core/update-vals instead once we update to Clojure 1.11
    (update type-iri-map
            "Pattern"
            (fn [iri-map]
              (reduce-kv
               (fn [m k pattern] (assoc m k (update-pattern pattern)))
               (empty iri-map)
               iri-map)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Putting it all Together
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef profiles->profile-map
  :args (s/cat :profiles (s/every ::profile)
               :pattern-params ::params/parameters
               :activity-seed int?)
  :ret (s/keys :req-un [::type-iri-map
                        ::activity/activity-map
                        ::statement-base-map
                        ::parsed-rules-map]))

(defn profiles->profile-map
  "Create a map from `profiles` that contains `type-iri-map`, `activity-map`,
   `statement-base-map`, and `parsed-rules-map`. Uses `pattern-params` to
   narrow down primary Patterns and `activity-seed` to generate additional
   activities in the cosmos."
  [profiles pattern-params activity-seed]
  (let [type-iri-map*      (profiles->type-iri-map profiles)
        type-iri-map       (select-primary-patterns type-iri-map* pattern-params)
        statement-base-map (profiles->base-statement-map profiles)
        parsed-rules-map*  (profiles->parsed-rule-map profiles)
        activity-map       (activity/create-activity-map type-iri-map activity-seed)
        parsed-rules-map   (reduce-kv
                            (fn [m id parsed-rules]
                              (->> parsed-rules
                                   (stmt/update-parsed-rules type-iri-map activity-map)
                                   (assoc m id)))
                            {}
                            parsed-rules-map*)]
    {:type-iri-map       type-iri-map
     :activity-map       activity-map
     :statement-base-map statement-base-map
     :parsed-rules-map   parsed-rules-map}))
