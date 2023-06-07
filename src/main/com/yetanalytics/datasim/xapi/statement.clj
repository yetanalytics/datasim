(ns com.yetanalytics.datasim.xapi.statement
  "Generate Statements"
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
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
    attachment-usage-types  :attachemntUsageType
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
               conj
               {"id" profile-version-id})))

(defn base-statement
  [{:keys [template sim-t registration object-override]} rng]
  (cond-> (-> (template->base-statement template)
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
  [{:keys [iri-map activities object-override]} rng {rules :rules}]
  (let [;; TODO: More efficient data structures
        verb-set          (->> iri-map vals (filter #(= "Verb" (:type %))) set)
        verb-id-set       (->> verb-set (map :id) set)
        activity-set      (->> activities vals (mapcat vals) set)
        activity-id-set   (->> activities vals (mapcat keys) set)
        activity-type-set (->> activities keys set)
        add-rule-valuegen (partial rule/add-rule-valuegen
                                   iri-map
                                   {:verbs          verb-set
                                    :verb-ids       verb-id-set
                                    :activities     activity-set
                                    :activity-ids   activity-id-set
                                    :activity-types activity-type-set})
        parsed-rules      (->> rules
                               (map rule/parse-rule-2)
                               (mapcat rule/separate-rule)
                               (map rule/add-rule-specpath))
        add-rule-specname (partial rule/add-rule-specname
                                   (rule/->path-rule-map parsed-rules))
        not-object-rule?  (if object-override
                            (partial rule/property-rule? "object")
                            (constantly true))]
    (->> rules
         (map add-rule-specname)
         (map add-rule-valuegen)
         (filter not-object-rule?)
         vec)))

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

(defn- generate-verb [_]
  {"id" (sgen/generate verb-id-gen)})

(defn profile->statement-verb
  [{:keys [id prefLabel]}]
  {"id"      id
   "display" (w/stringify-keys prefLabel)})

(s/fdef complete-verb
  :args (s/cat :inputs (s/keys :req-un [::profile/iri-map ::alignment])
               :rng    ::random/rng
               :verb   (s/nilable map?))
  :ret ::xs/verb)

(defn complete-verb
  [{:keys [iri-map alignment]} rng {:strs [id] :as verb}]
  (-> (or (some->> id
                   (get iri-map)
                   profile->statement-verb)
          (some->> verb
                   generate-verb)
          (some->> iri-map
                   vals
                   (filter #(= "Verb" (:type %)))
                   (map :id)
                   (random/choose rng alignment)
                   (get iri-map)))
      (merge-nested verb)))

;; Activities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def activity-id-gen (s/gen :activity/id))

(defn- generate-activity [_]
  {"id" (sgen/generate activity-id-gen)
   "objectType" "Activity"})

(s/fdef complete-activity
  :args (s/cat :inputs   (s/keys :req-un [::profile/iri-map
                                          ::activities
                                          ::alignment])
               :rng      ::random/rng
               :activity (s/nilable map?))
  :ret ::xs/activity)

(defn complete-activity
  [{:keys [activities alignment]}
   rng
   {:strs [id] {:strs [type]} "definition" :as activity}]
  (-> (or (some->> id
                   (get (reduce merge {} (vals activities))))
          (some->> type
                   (get activities)
                   keys
                   (random/choose rng alignment)
                   (apply get-in activities type))
          (some->> activity
                   generate-activity)
          (some->> activities
                   keys
                   (random/choose rng alignment)
                   (get activities)
                   keys
                   (random/choose rng alignment)
                   (get (reduce merge {} (vals activities)))))
      (merge-nested activity)))

;; Agents ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- get-ifi
  [{:strs [mbox mbox_sha1sum openid account]}]
  (or mbox mbox_sha1sum openid account))

(def agent-gen (s/gen ::xs/agent))

(def agent-account-gen (s/gen :agent/account))

(s/fdef complete-agent
  :args (s/cat :agent map?)
  :ret ::xs/agent)

(defn complete-agent [agent]
  (let [ifi     (get-ifi agent)
        account (get agent "account")
        agent*  (assoc agent "objectType" "Agent")]
    (cond
      (nil? ifi)
      (merge (sgen/generate agent-gen) agent*)
      (and account
           (or (not (contains? account "homePage"))
               (not (contains? account "name"))))
      (update agent*
              "account"
              (partial merge (sgen/generate agent-account-gen)))
      :else
      agent*)))

;; Groups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def group-gen (s/gen ::xs/group))

(def group-account-gen (s/gen :group/account))

(s/fdef complete-group
  :args (s/cat :group map?)
  :ret ::xs/group)

(defn complete-group
  [group]
  (let [ifi     (get-ifi group)
        account (get group "account")
        members (not-empty (get group "member"))
        group*  (cond-> group
                  true    (assoc "objectType" "Group")
                  members (update "member" (partial mapv complete-agent)))]
    (cond
      (and (nil? ifi) (nil? members))
      (merge (sgen/generate group-gen) group*)
      (and account
           (or (not (contains? account "homePage"))
               (not (contains? account "name"))))
      (update group*
              "account"
              (partial merge (sgen/generate group-account-gen)))
      :else
      group*)))

;; Context ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef complete-context
  :args (s/cat :inputs  (s/keys :req-un [::profile/iri-map
                                         ::activities
                                         ::alignment])
               :rng     ::random/rng
               :context (s/nilable map?))
  :ret ::xs/context)

(defn complete-context
  [inputs
   rng
   {:strs [instructor team]
    {:strs [category grouping parent other]} "contextActivities"
    :as context}]
  (let [complete-activities (partial mapv (partial complete-activity inputs rng))]
    (cond-> context
      instructor (update-in ["instructor"] complete-agent)
      team       (update-in ["team"] complete-group)
      category   (update-in ["contextActivities" "category"] complete-activities)
      grouping   (update-in ["contextActivities" "grouping"] complete-activities)
      parent     (update-in ["contextActivities" "parent"] complete-activities)
      other      (update-in ["contextActivities" "other"] complete-activities))))

;; Attachments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def attachment-gen (s/gen ::xs/attachment))

(defn- complete-attachment
  [attachment]
  (merge (sgen/generate attachment-gen) attachment))

(s/fdef complete-attachments
  :args (s/cat :attachments (s/coll-of map?))
  :ret ::xs/attachments)

(defn complete-attachments
  [attachments]
  (mapv complete-attachment attachments))

;; Authority ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- replace-ifi-with-account
  [{:strs [mbox mbox_sha1sum openid account] :as agent}]
  (cond-> agent
    (or mbox mbox_sha1sum openid)
    (dissoc "mbox" "mbox_sha1sum" "openid")
    (not account)
    (assoc "account" {})))

(s/fdef complete-authority
  :args (s/cat :authority map?)
  :ret (s/or :agent ::xs/agent
             :oauth-consumer ::xs/oauth-consumer
             :three-legged-oauth-group ::xs/tlo-group))

(defn complete-authority
  [{:strs [objectType member] :as authority}]
  (if (= "Group" objectType)
    ;; Three-legged OAuth Group
    (complete-group
     (condp #(= %1 (count %2)) member
       0 (assoc authority
                "member"
                [(complete-agent {"account" {}})
                 (complete-agent {})])
       1 (assoc authority
                "member"
                [(complete-agent (-> member first replace-ifi-with-account))
                 (complete-agent {})])
       2 (assoc authority
                "member"
                [(complete-agent (-> member first replace-ifi-with-account))
                 (complete-agent (-> member second))])
       (throw (ex-info "Cannot have authority with more than 2 group members"
                       {:type     ::invalid-3-legged-oauth-authority
                        :authority authority}))))
    ;; Regular Authority Agent
    (complete-agent authority)))

;; StatementRef ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def statement-ref-id-gen (s/gen :statement-ref/id))

(s/fdef complete-statement-ref
  :args (s/cat :statement-ref map?)
  :ret ::xs/statement-ref)

(defn complete-statement-ref
  [{:strs [id] :as statement-ref}]
  (cond-> (assoc statement-ref "objectType" "StatementRef")
    (nil? id) (assoc "id" (sgen/generate statement-ref-id-gen))))

;; SubStatement ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn complete-sub-object
  [inputs rng object]
  (case (get object "objectType")
    "Activity"     (complete-activity inputs rng object)
    "Agent"        (complete-agent object)
    "Group"        (complete-group object)
    "StatementRef" (complete-statement-ref object)
    "SubStatement" (throw (ex-info "Cannot have nested SubStatements!"
                                   {:type  ::nested-substatement
                                    :object object}))
    (complete-activity inputs rng object)))

(s/fdef complete-sub-statement
  :args (s/cat :inputs (s/keys :req-un [::profile/iri-map
                                        ::activities
                                        ::alignment])
               :rng ::random/rng
               :sub-statement map?)
  :ret ::xs/sub-statement)

;; Unlike top-level statements, sub-statements cannot have IDs or authorities
(defn complete-sub-statement
  [inputs rng sub-statement]
  (let [{:strs [context attachments]} sub-statement
        actor (-> inputs :actor w/stringify-keys)
        ;; Functions
        complete-verb    (partial complete-verb inputs rng)
        complete-object  (partial complete-sub-object inputs rng)
        complete-context (partial complete-context inputs rng)]
    (cond-> (-> sub-statement
                (update "actor" merge actor) ; TODO: Check that actor conforms to template
                (update "verb" complete-verb)
                (update "object" complete-object))
      ;; Optional statement properties
      context     (update "context" complete-context)
      attachments (update "attachments" complete-attachments))))

;; Statement ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def id-gen (s/gen :statement/id))

(defn- complete-id [id]
  (if id id (sgen/generate id-gen)))

(defn complete-object
  [inputs rng object]
  (case (get object "objectType")
    "Activity"     (complete-activity inputs rng object)
    "Agent"        (complete-agent object)
    "Group"        (complete-group object)
    "StatementRef" (complete-statement-ref object)
    "SubStatement" (complete-sub-statement inputs rng object)
    (complete-activity inputs rng object)))

(s/fdef complete-statement
  :args (s/cat :inputs (s/keys :req-un [::profile/iri-map
                                        ::activities
                                        ::alignment])
               :rng ::random/rng
               :statement map?)
  :ret ::xs/statement)

(defn complete-statement
  [inputs rng statement]
  (let [{:strs [context attachments authority]} statement
        actor (-> inputs :actor w/stringify-keys)
        ;; Functions
        complete-verb    (partial complete-verb inputs rng)
        complete-object  (partial complete-object inputs rng)
        complete-context (partial complete-context inputs rng)]
    (cond-> (-> statement
                (update "id" complete-id)
                (update "actor" merge actor) ; TODO: Check that actor conforms to template
                (update "verb" complete-verb)
                (update "object" complete-object))
      ;; Optional statement properties
      context     (update "context" complete-context)
      attachments (update "attachments" complete-attachments)
      authority   (update "authority" complete-authority))))

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
