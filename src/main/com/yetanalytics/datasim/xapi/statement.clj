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
            [com.yetanalytics.datasim.xapi.path :as xp]
            [com.yetanalytics.datasim.xapi.profile.template.rule :as rule])
  (:import [java.time Instant]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Inputs

;; TODO: Consolidate some of these specs with those in `xapi.profile`

;; Map of profile types -> IDs -> objects
;; TODO: Real specs
(s/def ::type-iri-map
  (s/map-of string? (s/map-of string? map?)))

(s/def ::statement-base-map
  (s/map-of ::template/id map?))

(s/def ::parsed-rules-map
  (s/map-of ::template/id ::rule/parsed-rule))

;; All the activities we can use, by activity type:
;; a map of activity type IRIs to activity IRIs to activities
(s/def ::activity-map
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
  (s/keys :req-un [::type-iri-map
                   ::activity-map
                   ::statement-base-map
                   ::parsed-rules-map
                   ::actor
                   ::alignment
                   ::template
                   ::pattern-ancestors
                   ::seed
                   ::registration
                   ::sim-t]
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
;; Statement Base
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- activity-type->activity-base
  [activity-type]
  {"definition" {"type" activity-type}})

(defn- usage-type->attachment-base
  [attachment-usage-type]
  {"usageType" attachment-usage-type})

(s/fdef template->statement-base
  :args (s/cat :template ::template/template)
  :ret map?)

(defn template->statement-base
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
      (assoc-in ["timestamp"] (time-ms->timestamp sim-t))
      (assoc-in ["context" "registration"] registration)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Rule Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef template->parsed-rules
  :args (s/cat :type-iri-map ::type-iri-map
               :activity-map ::activity-map
               :template     ::template/template)
  :ret ::rule/parsed-rule)

(defn template->parsed-rules
  "Return a collection of parsed rules derived from the template `rules`.
   Uses information from `iri-map` and `activities` maps, as well as from
   object-related Determining Properties, to complete the parsed rules."
  [type-iri-map
   activity-map
   {object-activity-type :objectActivityType
    object-statement-ref :objectStatementRefTemplate
    profile-id           :inScheme
    rules                :rules}]
  (let [parsed-rules   (cond
                         object-activity-type
                         (rule/parse-rules :activity-type rules)
                         object-statement-ref
                         (rule/parse-rules :statement-ref rules)
                         :else
                         (rule/parse-rules rules))
        prof-act-set   #{{"id" profile-id}}
        prof-id-set    #{profile-id}
        verbs          (->> (get type-iri-map "Verb") vals set)
        verb-ids       (->> (get type-iri-map "Verb") keys set)
        activities     (->> activity-map vals (mapcat vals) (into prof-act-set))
        activity-ids   (->> activity-map vals (mapcat keys) (into prof-id-set))
        activity-types (->> activity-map keys set)
        value-sets     {:verbs          verbs
                        :verb-ids       verb-ids
                        :activities     activities
                        :activity-ids   activity-ids
                        :activity-types activity-types}]
    (mapv (partial rule/add-rule-valuegen type-iri-map value-sets)
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
               :inputs (s/keys :req-un [::type-iri-map ::alignment])
               :rng    ::random/rng)
  :ret ::xs/verb)

(defn complete-verb
  [{:strs [id] :as verb} {:keys [type-iri-map alignment]} rng]
  (let [iri-map     (get type-iri-map "Verb")
        return-verb (fn [_] verb)
        merge-verb  (fn [v] (merge-nested v verb))]
    (or
     ;; Verb found by ID
     (some->> id
              iri-map
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
              keys
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
               :inputs   (s/keys :req-un [::activity-map ::alignment])
               :rng      ::random/rng)
  :ret ::xs/activity)

(defn complete-activity
  [{:strs [id] {:strs [type]} "definition" :as activity}
   {:keys [activity-map alignment]}
   rng]
  (let [return-activity (fn [_] activity)
        merge-activity  (fn [a] (merge-nested a activity))]
    (or
     ;; Get activity by ID
     (some->> id
              (get (reduce merge {} (vals activity-map)))
              merge-activity)
     ;; Get activity by type
     (some->> type
              (get activity-map)
              keys
              (random/choose rng alignment)
              (conj [type])
              (get-in activity-map)
              merge-activity)
     ;; Activity w/ ID not found, return as-is
     (some->> id
              return-activity)
     ;; Activity w/o ID not found, assoc generated
     (some->> activity
              (generate-activity rng)
              merge-activity)
     ;; Choose random activity
     (some->> activity-map
              keys
              (random/choose rng alignment)
              (get activity-map)
              keys
              (random/choose rng alignment)
              (get (reduce merge {} (vals activity-map)))))))

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

;; Agents and Groups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef complete-actor
  :args (s/cat :actor map?
               :rng ::random/rng)
  :ret (s/or :agent ::xs/agent
             :group ::xs/group))

;; TODO: WARNING if Template actually requires an authority (since it's
;; supposed to only be set by the LRS)
(defn complete-actor
  [{:strs [objectType member] :as authority} rng]
  (if (or (= "Group" objectType)
          (some? member))
    (complete-group authority rng)
    (complete-agent authority rng)))

;; Context ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef complete-context
  :args (s/cat :context (s/nilable map?)
               :inputs  (s/keys :req-un [::activity-map ::alignment])
               :rng     ::random/rng)
  :ret ::xs/context)

(defn complete-context
  [{:strs [instructor team]
    {:strs [category grouping parent other]} "contextActivities"
    :as context}
   {{profile-ver-id :inScheme} :template
    registration :registration :as inputs}
   rng
   & {:keys [sub-statement?] :or {sub-statement? false}}]
  (let [complete-activities (partial mapv #(complete-activity % inputs rng))]
    (cond-> context
      ;; Context Agents + Groups
      instructor (update-in ["instructor"] complete-actor rng)
      team       (update-in ["team"] complete-group rng)
      ;; Context Activities
      category (update-in ["contextActivities" "category"] complete-activities)
      grouping (update-in ["contextActivities" "grouping"] complete-activities)
      parent   (update-in ["contextActivities" "parent"] complete-activities)
      other    (update-in ["contextActivities" "other"] complete-activities)
      ;; Restore profile version ID if needed
      ;; TODO: WARNING if the Template actively overwrites or contradicts
      (and (not sub-statement?)
           (->> (get-in context ["contextActivities" "category"])
                (some (fn [{id "id"}] (#{profile-ver-id} id)))
                not))
      (update-in ["contextActivities" "category"] conj {"id" profile-ver-id})
      ;; Restore registration if needed
      ;; TODO: WARNING if the Template actively overwrites
      (and (not sub-statement?)
           (->> (get context "registration")
                (not= registration)))
      (assoc-in ["registration"] registration))))

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
                          (-> (get xp/spec-hint-properties-m prop)
                              (cset/intersection types*)))
                        (get xp/default-object-type-m ["object"])
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
               :inputs (s/keys :req-un [::type-iri-map
                                        ::activity-map
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
      context     (update "context" complete-context inputs rng :sub-statement? true)
      attachments (update "attachments" complete-attachments rng))))

;; Statement ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                          (-> (get xp/spec-hint-properties-m prop)
                              (cset/intersection types*)))
                        (get xp/default-object-type-m ["object"])
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
               :inputs (s/keys :req-un [::type-iri-map
                                        ::activity-map
                                        ::alignment])
               :rng ::random/rng)
  :ret ::xs/statement)

(defn complete-statement
  [statement inputs rng]
  (let [;; TODO: WARNING if Template contradicts on actor
        ;; TODO: WARNING if Template overwrites ID or timestamp
        {:strs [context attachments authority]} statement
        actor (-> inputs :actor w/stringify-keys)]
    (cond-> (-> statement
                (update "actor" merge actor)
                (update "verb" complete-verb inputs rng)
                (update "object" complete-object inputs rng))
      ;; Optional statement properties
      ;; Result is not updated since it has no required properties
      context     (update "context" complete-context inputs rng)
      attachments (update "attachments" complete-attachments rng)
      authority   (update "authority" complete-actor rng))))

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
           activity-map
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
  (let [;; Prep
        template-base
        (or (get statement-base-map (:id template))
            (template->statement-base template))
        template-rules
        (or (get parsed-rules-map (:id template))
            (template->parsed-rules type-iri-map activity-map template))
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
