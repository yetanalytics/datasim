(ns com.yetanalytics.datasim.xapi.statement
  "Generate Statements"
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as stest]
            [clojure.set :as cset]
            [clojure.string :as string]
            [clojure.walk :as w]
            [com.yetanalytics.datasim.xapi.profile :as profile]
            [com.yetanalytics.datasim.xapi.activity :as activity]
            [com.yetanalytics.datasim.input :as input]
            [com.yetanalytics.datasim.input.alignments :as alignments]
            [com.yetanalytics.datasim.input.personae :as personae]
            [com.yetanalytics.pan.objects.template :as template]
            [com.yetanalytics.datasim.random :as random]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.xapi.path :as xp]
            [com.yetanalytics.datasim.xapi.profile.template.rule :as rule]
            [com.yetanalytics.datasim.json.schema :as j-schema]
            [com.yetanalytics.datasim.xapi.extensions :as ext])
  (:import [java.time Instant]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The duration, in milliseconds, of the returned statement
;; This is so we can resume processing AFTER the statement timestamp + duration
(s/def ::end-ms
  pos-int?)

(s/def ::timestamp-ms
  pos-int?)

(s/def ::meta
  (s/keys :req-un [::timestamp-ms
                   ::end-ms]))

(s/def ::alignment
  ::alignments/alignment)

(s/def ::sim-t pos-int?)

#_(s/def ::seed int?)

(s/def ::registration
  ::xs/uuid)

(s/def ::sub-registration
  ::xs/uuid)

;; TODO: this is a stub for the real ones
(s/def ::pattern-ancestors
  (s/every map?))

;; a stub for a map of activities by IRI
(s/def ::activities
  (s/map-of ::xs/iri ;; activity type
            (s/map-of ::xs/iri
                      ::xs/activity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Base
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- activity-type->activity-base
  [activity-type]
  {"definition" {"type" activity-type}})

(defn- usage-type->attachment-base
  [attachment-usage-type]
  {"usageType" attachment-usage-type})

(defn template->base-statement
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
  [template-base {:keys [sim-t registration object-override]} rng]
  (cond-> (-> template-base
              (assoc-in ["id"]
                        (random/rand-uuid rng))
              (assoc-in ["timestamp"]
                        (.toString (Instant/ofEpochMilli sim-t)))
              (assoc-in ["context" "registration"]
                        registration))
    object-override
    (assoc "object" object-override)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Rule Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn template-rules
  [iri-map
   activities
   {object-activity-type :objectActivityType
    object-statement-ref :objectStatementRefTemplate
    profile-id           :inScheme
    rules                :rules}]
  (let [parsed-rules   (rule/parse-rules rules)
        spec-hints     (cond-> (rule/rules->spec-hints parsed-rules)
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

;; Helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- merge-nested
  "Perform a nested merge of `m2` onto `m1`. Recurses on maps and replaces
   all other values."
  [m1 m2]
  (merge-with
   (fn [v1 v2] (if (and (map? v1) (map? v2)) (merge-nested v1 v2) v2))
   m1
   m2))

(defn- super-nested?
  "Is `m1` a \"supermap\" of `m2`? Checks recursively on map values."
  [m1 m2]
  (let [keys-1 (keys m1)
        keys-2 (keys m2)]
    (if (cset/superset? (set keys-1) (set keys-2))
      (->> m2
           (merge-with
            (fn [v1 v2]
              (if (and (map? v1) (map? v2))
                (super-nested? v1 v2)
                (= v1 v2)))
            (select-keys m1 keys-2))
           vals
           (reduce (fn [acc res] (and acc res)) true))
      false)))

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

(defn complete-group
  [group rng]
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

(defn complete-sub-object
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

(defn complete-object
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

(s/fdef generate-statement
  :args
  (s/cat
   :args-map
   (s/keys
    :req-un [;; input for the whole simulation
             :com.yetanalytics.datasim/input
             ;; flat map of profile iris to objects
             ::profile/iri-map
             ;; all the activities we can use, by activity type
             ::activities
             ;; the actor for the statement
             ;; (may have to stringify keys?)
             ::xs/actor
             ;; their alignment, a map of IRI to -1.0->1.0
             ::alignment
             ;; a statement template to generate from
             ::template/template
             ;; antecedent patterns to the current template, and whether or not
             ;; they are primary
             ::pattern-ancestors
             ;; Simulation time, in ms since epoch
             ::sim-t
             ;; A seed to generate with. Note that if you're calling more seeded
             ;; generators, you'll need to make a seed from this one for each.
             ::random/seed
             ;; A registration UUID string
             ::registration]
    :opt-un [::sub-registration]))
  :ret (s/and ::xs/statement
              (s/conformer meta)
              ::meta))

(defn- search-rules
  [template-rules target-location]
  ;; filter `template-rules` for a rule whose location is set to `target-location`
  (->> template-rules
       (filterv (fn [r] (= (:location r) target-location)))
       first
       not-empty))

(defn- handle-rule-vals [rng alignment the-rule]
  ;; handle `:any` and/or `:all` within `the-rule`
  (let [{any-coll :any
         all-coll :all} the-rule
        n-any           (count any-coll)
        n-all           (count all-coll)]
    ;; returns single item from `:any` or `:all` or returns nil
    (cond (and (> n-any 0) (> n-all 0))
          ;; both any and all for some reason
          ;; -> use all to filter any
          (let [remaining (into [] (cset/intersection (set all-coll) (set any-coll)))]
            (random/choose rng alignment remaining))
          (= n-all 1)
          ;; not handling (> n-all 1) as it doesn't make sense in isolation
          (first all-coll)
          (> n-any 0)
          ;; allow for users to shoot themselves in the foot
          (random/choose rng alignment any-coll))))

;; TODO: subregistration from :pattern-ancestors logic
;; -> "https://w3id.org/xapi/profiles/extensions/subregistration"
;;    -> subregistration extension key
;;    -> only necessary when a primary pattern contains another primary pattern

(defn generate-statement
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
        (with-meta {:end-ms       (+ sim-t (long (random/rand-gauss rng 600000.0 0.5)))
                    :timestamp-ms sim-t
                    :template     template}))))

#_(defn generate-statement
  [{{:keys [profiles]} :input
    iri-map :iri-map
    activities :activities
    actor :actor
    alignment :alignment
    sim-t :sim-t
    seed :seed
    {template-iri :id
     ?verb-id :verb
     ?activity-type :objectActivityType
     template-rules :rules
     template-in-scheme :inScheme
     :as template} :template
    pattern-ancestors :pattern-ancestors
    registration :registration
    ?sub-registration :sub-registration}]
  (let [rng           (random/seed-rng seed)
        ?obj-override (some->> alignment
                               not-empty
                               keys
                               (random/choose rng alignment)
                               (get alignment)
                               :object-override
                               w/stringify-keys)
        ;; components of `base-stmt`
        stmt-id    (random/rand-uuid rng)
        stmt-actor (w/stringify-keys actor)
        stmt-verb  (or
                    ;; explicit verb
                    (and ?verb-id
                         (merge {"id" ?verb-id}
                                (when-let [lmap (get-in iri-map [?verb-id :prefLabel])]
                                  {"display" (w/stringify-keys lmap)})))
                    ;; choose a verb from the prof
                    (when-let [verbs (not-empty (into {}
                                                      (filter (comp (partial = "Verb")
                                                                    :type second))
                                                      iri-map))]
                      (let [some-v-id (random/choose rng alignment (keys verbs))
                            v-concept (get verbs some-v-id)]
                        (merge {"id" some-v-id}
                               (when-let [lmap (get v-concept :prefLabel)]
                                 {"display" (w/stringify-keys lmap)}))))
                    {"id" "http://adlnet.gov/expapi/verbs/experienced"
                     "display" {"en" "experienced"}})
        stmt-obj   (or
                    ;; object override is valid for the template and is present
                    ;; in the alignment
                    ;; - note: ?obj-override will get overriden; we are using
                    ;;   it as a placeholder
                    (when ?obj-override ?obj-override)
                    ;; quick scan rules for "$.object.id"
                    ;; -> when found, use `?activity-type` or look for "$.object.definition.type" rule
                    ;;    -> use `obj-at` to lookup `rule-obj-id` in `activities`, nil if not found
                    ;;    -> remove activity type level from `activities` and search for `rule-obj-id`, nil if not found
                    (when-let [rule-obj-id (handle-rule-vals rng alignment (search-rules template-rules "$.object.id"))]
                      ;; there's a rule specifying object.id, prep for lookup in `activities`
                      (if-let [obj-at (or ?activity-type
                                          (when-let [obj-type-rule (search-rules template-rules "$.object.definition.type")]
                                            (handle-rule-vals rng alignment obj-type-rule)))]
                        (get-in activities [obj-at rule-obj-id])
                        (get (reduce merge (vals activities)) rule-obj-id)))
                    ;; there's an activity type we choose one of those
                    (and ?activity-type
                         (let [some-activity-id
                               (random/choose rng
                                              alignment
                                              (keys (get activities ?activity-type)))]
                           (get-in activities [?activity-type some-activity-id])))
                    ;; no type, choose any activity
                    (let [flat-activities
                          (reduce merge
                                  (vals activities))

                          some-activity-id
                          (random/choose rng
                                         alignment
                                         (keys flat-activities))]
                      (get flat-activities some-activity-id)))
        stmt-ctx   {"contextActivities" {"category" [{"id" template-in-scheme}]}
                    "registration"      registration}
        stmt-ts    (.toString (Instant/ofEpochMilli sim-t))
        ;; Start with this and reduce over the rules
        base-stmt  {"id"        stmt-id
                    "actor"     stmt-actor
                    "verb"      stmt-verb
                    "object"    stmt-obj
                    "context"   stmt-ctx
                    "timestamp" stmt-ts}
        ;; addition of `:spec` key to 0 or more `template-rules`
        template-rules! (ext/derive-generation-hint profiles rng template-rules)]
    (with-meta
      (let [stmt (rule/apply-rules-gen base-stmt
                                       template-rules!
                                       :seed (random/rand-long rng))]
        ;; Need to override object again, in case object is changed
        ;; by the Template rules.
        (if ?obj-override (assoc stmt "object" ?obj-override) stmt))
      ;; The duration in MS so we can continue the sim
      {;; The time (in millis since the epoch) after which the actor can
       ;; continue activity
       :end-ms (+ sim-t
                  (long
                   (random/rand-gauss
                    rng 600000.0 0.5)))
       ;; the time of the timestamp (in millis since the epoch)
       ;; note that just has to be greater that sim-t and less than or eq to end-ms,
       ;; it's up to you. For the stub we just make it sim-t
       :timestamp-ms sim-t
       ;; Return the template, useful for a bunch of things
       :template template})))
