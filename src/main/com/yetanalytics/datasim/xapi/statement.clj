(ns com.yetanalytics.datasim.xapi.statement
  "Generate Statements"
  (:require [clojure.set :as cset]
            [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as stest]
            [clojure.walk :as w]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.pan.objects.template :as template]
            [com.yetanalytics.datasim.random :as random]
            [com.yetanalytics.datasim.input] ; for input spec
            [com.yetanalytics.datasim.input.alignments :as alignments]
            [com.yetanalytics.datasim.xapi.profile :as profile]
            [com.yetanalytics.datasim.xapi.path :as xp]
            [com.yetanalytics.datasim.xapi.profile.template.rule :as rule])
  (:import [java.time Instant]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Inputs

;; Input for the whole simulation.
(s/def ::input :com.yetanalytics.datasim/input)

;; Flat map of profile iris to objects
(s/def ::iri-map ::profile/iri-map)

;; All the activities we can use, by activity type:
;; a map of activity type IRIs to activity IRIs to activities
(s/def ::activities
  (s/map-of ::xs/iri
            (s/map-of ::xs/iri
                      ::xs/activity)))

;; The actor for the statement (may have to stringify keys?)
(s/def ::actor ::xs/actor)

;; The alignment, a map of IRI to a map of `:weights` from -1.0 to 1.0 and
;; a nilable `:object-override` object.
(s/def ::alignment ::alignments/alignment)

;; The statement template to generate from
(s/def ::template ::template/template)

;; Antecedent patterns to the current template, and whether or not they're primary.
;; TODO: replace `map?` with a real spec
(s/def ::pattern-ancestors
  (s/every map?))

;; Simulation time, in ms since epoch.
(s/def ::sim-t pos-int?)

;; A seed to generate with. Note that if you're calling more seeded
;; generators, you'll need to make a seed from this one for each.
(s/def ::seed ::random/seed)

;; A registration UUID string.
(s/def ::registration
  ::xs/uuid)

;; TODO: subregistration from :pattern-ancestors logic
;; -> "https://w3id.org/xapi/profiles/extensions/subregistration"
;;    -> subregistration extension key
;;    -> only necessary when a primary pattern contains another primary pattern
(s/def ::sub-registration
  any?) ; TODO: replace `any?` with real spec

(s/def ::inputs
  (s/keys :req-un [::input
                   ::iri-map
                   ::activities
                   ::actor
                   ::alignment
                   ::template
                   ::pattern-ancestors
                   ::sim-t
                   ::seed
                   ::registration]
          :opt-un [::sub-registration]))

;; Metadata

;; The duration, in milliseconds, of the returned statement.
;; This is so we can resume processing AFTER the statement timestamp + duration.
(s/def ::end-ms pos-int?)

(s/def ::timestamp-ms pos-int?)

(s/def ::meta
  (s/keys :req-un [::timestamp-ms
                   ::end-ms]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Base
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- activity-type->activity-base
  [activity-type]
  {"definition" {"type" activity-type}})

(defn- usage-type->attachment-base
  [attachment-usage-type]
  {"usageType" attachment-usage-type})

(defn- template->base-statement
  "Form the base of a statement from the Determining Properties of
   the Template. Elements of array-valued properties (the context
   activity types and the attachment usage types) are added in order."
  [{profile-version-id      :inScheme
    verb-id                 :verb
    object-activity-type    :objectActivityType
    category-activity-types :contextCategoryActivityType
    grouping-activity-types :contextGroupingActivityType
    parent-activity-types   :contextParentActivityType
    other-activity-types    :contextOtherActivityType
    attachment-usage-types  :attachmentUsageType
    ;; TODO: StatementRef properties
    ;; object-statement-ref  :objectStatementRefTemplate
    ;; context-statement-ref :contextStatementRefTemplate
    }]
  (cond-> {}
    verb-id
    (assoc-in ["verb" "id"] verb-id)
    object-activity-type
    (assoc-in ["object" "definition" "type"] object-activity-type)
    category-activity-types
    (assoc-in ["context" "contextActivities" "category"]
              (mapv activity-type->activity-base category-activity-types))
    grouping-activity-types
    (assoc-in ["context" "contextActivities" "grouping"]
              (mapv activity-type->activity-base grouping-activity-types))
    parent-activity-types
    (assoc-in ["context" "contextActivities" "parent"]
              (mapv activity-type->activity-base parent-activity-types))
    other-activity-types
    (assoc-in ["context" "contextActivities" "other"]
              (mapv activity-type->activity-base other-activity-types))
    attachment-usage-types
    (assoc-in ["attachments"]
              (mapv usage-type->attachment-base attachment-usage-types))
    profile-version-id ; always true
    (update-in ["context" "contextActivities" "category"]
               (fnil conj [])
               {"id" profile-version-id})))

(defn base-statement
  [template-base {:keys [sim-t registration]} rng]
  (-> template-base
      (assoc-in ["id"] (random/rand-uuid rng))
      (assoc-in ["timestamp"] (.toString (Instant/ofEpochMilli sim-t)))
      (assoc-in ["context" "registration"] registration)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Rule Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- template-rules
  "Return a collection of parsed rules derived from the template `rules`.
   Uses information from `iri-map` and `activities` maps, as well as from
   object-related Determining Properties, to complete the parsed rules."
  [iri-map
   activities
   {object-activity-type :objectActivityType
    object-statement-ref :objectStatementRefTemplate
    profile-id           :inScheme
    rules                :rules}]
  (let [parsed-rules   (rule/parse-rules rules)
        spec-hints     (cond-> (rule/rules->object-types parsed-rules)
                         object-activity-type
                         (update ["object"] cset/intersection #{"activity"})
                         object-statement-ref
                         (update ["object"] cset/intersection #{"statement-ref"}))
        ;; TODO: More efficient data structures
        verbs          (->> iri-map vals (filter #(= "Verb" (:type %))) set)
        verb-ids       (->> verbs (map :id) set)
        activityies    (->> activities vals (mapcat vals) (into #{{"id" profile-id}}))
        activity-ids   (->> activities vals (mapcat keys) (into #{profile-id}))
        activity-types (->> activities keys set)
        value-sets     {:verbs          verbs
                        :verb-ids       verb-ids
                        :activities     activityies
                        :activity-ids   activity-ids
                        :activity-types activity-types}]
    (mapv (partial rule/add-rule-valuegen iri-map spec-hints value-sets)
          parsed-rules)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Move these statement completion/"healing" functions to their own ns.ç

;; Helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- merge-nested
  "Perform a nested merge of `m2` onto `m1`. Recurses on maps and replaces
   all other values."
  [m1 m2]
  (merge-with
   (fn [v1 v2] (if (and (map? v1) (map? v2)) (merge-nested v1 v2) v2))
   m1
   m2))

;; Verbs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def verb-id-gen (s/gen :verb/id))

(defn- generate-verb [rng _]
  {"id" (stest/generate verb-id-gen 1 (random/rand-long rng))})

(defn profile->statement-verb
  [{:keys [id prefLabel]}]
  {"id"      id
   "display" (w/stringify-keys prefLabel)})

(s/fdef complete-verb
  :args (s/cat :verb   (s/nilable map?)
               :inputs (s/keys :req-un [::profile/iri-map ::alignment])
               :rng    ::random/rng)
  :ret ::xs/verb)

(defn complete-verb
  [{:strs [id] :as verb} {:keys [iri-map alignment]} rng]
  (let [return-verb (fn [_] verb)
        merge-verb  (fn [v] (merge-nested v verb))]
    (or
     ;; Verb found by ID
     (some->> id
              (get iri-map)
              profile->statement-verb
              merge-verb)
     ;; Verb w/ ID not found, return as-is
     (some->> id
              return-verb)
     ;; Verb w/o ID not found, generate ID
     (some->> verb
              (generate-verb rng)
              merge-verb)
     ;; Choose random verb
     (some->> iri-map
              vals
              (filter #(= "Verb" (:type %)))
              (map :id)
              (random/choose rng alignment)
              (get iri-map)
              profile->statement-verb))))

;; Activities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def activity-id-gen (s/gen :activity/id))

(defn- generate-activity [rng _]
  {"id" (stest/generate activity-id-gen 1 (random/rand-long rng))
   "objectType" "Activity"})

(s/fdef complete-activity
  :args (s/cat :activity (s/nilable map?)
               :inputs   (s/keys :req-un [::activities ::alignment])
               :rng      ::random/rng)
  :ret ::xs/activity)

(defn complete-activity
  [{:strs [id] {:strs [type]} "definition" :as activity}
   {:keys [activities alignment]}
   rng]
  (let [return-activity (fn [_] activity)
        merge-activity  (fn [a] (merge-nested a activity))]
    (or
     ;; Get activity by ID
     (some->> id
              (get (reduce merge {} (vals activities)))
              merge-activity)
     ;; Get activity by type
     (some->> type
              (get activities)
              keys
              (random/choose rng alignment)
              (conj [type])
              (get-in activities)
              merge-activity)
     ;; Activity w/ ID not found, return as-is
     (some->> id
              return-activity)
     ;; Activity w/o ID not found, assoc generated
     (some->> activity
              (generate-activity rng)
              merge-activity)
     ;; Choose random activity
     (some->> activities
              keys
              (random/choose rng alignment)
              (get activities)
              keys
              (random/choose rng alignment)
              (get (reduce merge {} (vals activities)))))))

;; Agents ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- get-ifi
  [{:strs [mbox mbox_sha1sum openid account]}]
  (or mbox mbox_sha1sum openid account))

(def agent-gen (s/gen ::xs/agent))

(def agent-account-gen (s/gen :agent/account))

(s/fdef complete-agent
  :args (s/cat :agent map?
               :rng ::random/rng)
  :ret ::xs/agent)

(defn complete-agent [agent rng]
  (let [ifi     (get-ifi agent)
        account (get agent "account")
        agent*  (assoc agent "objectType" "Agent")]
    (cond
      (nil? ifi)
      (merge (stest/generate agent-gen 1 (random/rand-long rng)) agent*)
      (and account
           (or (not (contains? account "homePage"))
               (not (contains? account "name"))))
      (update agent*
              "account"
              (partial merge (stest/generate agent-account-gen
                                             1
                                             (random/rand-long rng))))
      :else
      agent*)))

;; Groups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def group-gen (s/gen ::xs/group))

(def group-account-gen (s/gen :group/account))

(s/fdef complete-group
  :args (s/cat :group map?
               :rng ::random/rng)
  :ret ::xs/group)

(defn complete-group [group rng]
  (let [ifi     (get-ifi group)
        account (get group "account")
        members (not-empty (get group "member"))
        group*  (cond-> group
                  true    (assoc "objectType" "Group")
                  members (assoc "member" (mapv #(complete-agent % rng)
                                                members)))]
    (cond
      (and (nil? ifi) (nil? members))
      (merge (stest/generate group-gen 1 (random/rand-long rng)) group*)
      (and account
           (or (not (contains? account "homePage"))
               (not (contains? account "name"))))
      (update group*
              "account"
              (partial merge (stest/generate group-account-gen
                                             1
                                             (random/rand-long rng))))
      :else
      group*)))

;; Context ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef complete-context
  :args (s/cat :context (s/nilable map?)
               :inputs  (s/keys :req-un [::activities ::alignment])
               :rng     ::random/rng)
  :ret ::xs/context)

(defn complete-context
  [{:strs [instructor team]
    {:strs [category grouping parent other]} "contextActivities"
    :as context}
   inputs
   rng]
  (let [complete-activities (partial mapv #(complete-activity % inputs rng))
        group-instructor?   #(or (-> % (get "objectType") #{"Group"})
                                 (-> % (contains? "member")))]
    (cond-> context
      (and instructor
           (group-instructor? instructor))
      (update-in ["instructor"] complete-group rng)
      (and instructor
           (not (group-instructor? instructor)))
      (update-in ["instructor"] complete-agent rng)
      team     (update-in ["team"] complete-group rng)
      category (update-in ["contextActivities" "category"] complete-activities)
      grouping (update-in ["contextActivities" "grouping"] complete-activities)
      parent   (update-in ["contextActivities" "parent"] complete-activities)
      other    (update-in ["contextActivities" "other"] complete-activities))))

;; Attachments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def attachment-gen (s/gen ::xs/attachment))

(defn- complete-attachment
  [rng attachment]
  (merge (stest/generate attachment-gen 1 (random/rand-long rng)) attachment))

(s/fdef complete-attachments
  :args (s/cat :attachments (s/coll-of map?)
               :rng ::random/rng)
  :ret ::xs/attachments)

(defn complete-attachments
  [attachments rng]
  (mapv (partial complete-attachment rng) attachments))

;; Authority ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- replace-ifi-with-account
  [{:strs [mbox mbox_sha1sum openid account] :as agent}]
  (cond-> agent
    (or mbox mbox_sha1sum openid)
    (dissoc "mbox" "mbox_sha1sum" "openid")
    (not account)
    (assoc "account" {})))

(s/fdef complete-authority
  :args (s/cat :authority map?
               :rng ::random/rng)
  :ret (s/or :agent ::xs/agent
             :oauth-consumer ::xs/oauth-consumer
             :three-legged-oauth-group ::xs/tlo-group))

;; TODO: Should this function even exist, or should we ban authorities from
;; being set.
(defn complete-authority
  [{:strs [objectType member] :as authority} rng]
  (if (= "Group" objectType)
    ;; Three-legged OAuth Group
    (-> (condp #(= %1 (count %2)) member
          0 (assoc authority
                   "member"
                   [(complete-agent {"account" {}} rng)
                    (complete-agent {} rng)])
          1 (assoc authority
                   "member"
                   [(complete-agent (-> member first replace-ifi-with-account) rng)
                    (complete-agent {} rng)])
          2 (assoc authority
                   "member"
                   [(complete-agent (-> member first replace-ifi-with-account) rng)
                    (complete-agent (-> member second) rng)])
          (throw (ex-info "Cannot have authority with more than 2 group members"
                          {:type     ::invalid-3-legged-oauth-authority
                           :authority authority})))
        (complete-group rng))
    ;; Regular Authority Agent
    (complete-agent authority rng)))

;; StatementRef ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def statement-ref-id-gen (s/gen :statement-ref/id))

(s/fdef complete-statement-ref
  :args (s/cat :statement-ref map?
               :rng ::random/rng)
  :ret ::xs/statement-ref)

(defn complete-statement-ref
  [{:strs [id] :as statement-ref} rng]
  (cond-> (assoc statement-ref "objectType" "StatementRef")
    (nil? id) (assoc "id" (stest/generate statement-ref-id-gen
                                          1
                                          (random/rand-long rng)))))

;; SubStatement ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- complete-sub-object
  [object inputs rng]
  (case (get object "objectType")
    "Activity"     (complete-activity object inputs rng)
    "Agent"        (complete-agent object rng)
    "Group"        (complete-group object rng)
    "StatementRef" (complete-statement-ref object rng)
    "SubStatement" (throw (ex-info "Cannot have nested SubStatements!"
                                   {:type  ::nested-substatement
                                    :object object}))
    (let [props (keys object)
          types (reduce (fn [types* prop]
                          (-> (get xp/spec-hint-properties prop)
                              (cset/intersection types*)))
                        (get xp/default-spec-hints ["object"])
                        props)]
      (condp #(contains? %2 %1) types
        "activity"      (complete-activity object inputs rng)
        "agent"         (complete-agent object rng)
        "group"         (complete-group object rng)
        "statement-ref" (complete-statement-ref object rng)
        (throw (ex-info "Unknown Statement object type"
                        {:kind   ::unknown-object-type
                         :object object
                         :types  types}))))))

(s/fdef complete-sub-statement
  :args (s/cat :sub-statement map?
               :inputs (s/keys :req-un [::profile/iri-map
                                        ::activities
                                        ::alignment])
               :rng ::random/rng)
  :ret ::xs/sub-statement)

;; Unlike top-level statements, sub-statements cannot have IDs or authorities
(defn complete-sub-statement
  [sub-statement inputs rng]
  (let [{:strs [context attachments]} sub-statement
        actor (-> inputs :actor w/stringify-keys)]
    (cond-> (-> sub-statement
                (assoc "objectType" "SubStatement")
                (update "actor" merge actor) ; TODO: Check that actor conforms to template
                (update "verb" complete-verb inputs rng)
                (update "object" complete-sub-object inputs rng))
      ;; Optional statement properties
      context     (update "context" complete-context inputs rng)
      attachments (update "attachments" complete-attachments rng))))

;; Statement ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def id-gen (s/gen :statement/id))

(defn- complete-id [id rng]
  (if id id (stest/generate id-gen 1 (random/rand-long rng))))

(defn- complete-object
  [object inputs rng]
  (case (get object "objectType")
    "Activity"     (complete-activity object inputs rng)
    "Agent"        (complete-agent object rng)
    "Group"        (complete-group object rng)
    "StatementRef" (complete-statement-ref object rng)
    "SubStatement" (complete-sub-statement object inputs rng)
    (let [props (keys object)
          types (reduce (fn [types* prop]
                          (-> (get xp/spec-hint-properties prop)
                              (cset/intersection types*)))
                        (get xp/default-spec-hints ["object"])
                        props)]
      (condp #(contains? %2 %1) types
        "activity"      (complete-activity object inputs rng)
        "agent"         (complete-agent object rng)
        "group"         (complete-group object rng)
        "statement-ref" (complete-statement-ref object rng)
        "sub-statement" (complete-sub-statement object inputs rng)
        (throw (ex-info "Unknown Statement object type"
                        {:kind   ::unknown-object-type
                         :object object
                         :types  types}))))))

(s/fdef complete-statement
  :args (s/cat :statement map?
               :inputs (s/keys :req-un [::profile/iri-map
                                        ::activities
                                        ::alignment])
               :rng ::random/rng)
  :ret ::xs/statement)

(defn complete-statement
  [statement inputs rng]
  (let [{:strs [context attachments authority]} statement
        ;; TODO: Check that actor conforms to template
        actor (-> inputs :actor w/stringify-keys)]
    (cond-> (-> statement
                (update "id" complete-id rng)
                (update "actor" merge actor)
                (update "verb" complete-verb inputs rng)
                (update "object" complete-object inputs rng))
      ;; Optional statement properties
      ;; Result is not updated since it has no required properties
      context     (update "context" complete-context inputs rng)
      attachments (update "attachments" complete-attachments rng)
      authority   (update "authority" complete-authority rng))))

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

(defn- statement-meta
  "Return a map of statement metadata, consisting of a map of the following:
   - `:end-ms`: The time (in milliseconds since the epoch) after which the
     actor can continue the activity.
   - `timestamp-ms: The time of the timestamp (in milliseconds since the epoch).
     Note that it just has to be greater that `sim-t` and less than or equal
     to `end-ms`; for the stub we just make it `sim-t`.
   - `:template`: The template used to generate the statement, which will be
     useful for a bunch of things."
  [template sim-t rng]
  {:end-ms       (+ sim-t (long (random/rand-gauss rng 600000.0 0.5)))
   :timestamp-ms sim-t
   :template     template})

(s/fdef generate-statement
  :args (s/cat :args-map ::inputs)
  :ret (s/and ::xs/statement
              (s/conformer meta)
              ::meta))

(defn generate-statement
  #_{:clj-kondo/ignore [:unused-binding]} ; unused args are used in helper fns
  [{{:keys [profiles]} :input
    iri-map            :iri-map
    activities         :activities
    actor              :actor
    alignment          :alignment
    sim-t              :sim-t
    seed               :seed
    template           :template
    pattern-ancestors  :pattern-ancestors
    registration       :registration
    ?sub-registration  :sub-registration
    :as inputs}]
  (let [;; Prep
        ;; TODO: Precompile these two so that they don't happen on
        ;; every statement gen
        template-base  (template->base-statement template)
        template-rules (template-rules iri-map activities template)
        ;; Basics
        rng             (random/seed-rng seed)
        object-override (select-object-override rng alignment)
        template-rules* (remove-object-rules template-rules object-override)]
    (-> template-base
        (base-statement inputs rng)
        (apply-object-override object-override)
        (rule/apply-inclusion-rules template-rules* rng)
        (complete-statement inputs rng)
        (rule/apply-exclusion-rules template-rules*)
        (with-meta (statement-meta template sim-t rng)))))
