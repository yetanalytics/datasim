(ns com.yetanalytics.datasim.xapi.statement.healing
  "Statement healing, i.e. the repair of Statements that are missing
   required properties."
  (:require [clojure.set                   :as cset]
            [clojure.spec.alpha            :as s]
            [clojure.test.check.generators :as stest]
            [clojure.walk                  :as w]
            [xapi-schema.spec              :as xs]
            [com.yetanalytics.datasim.util.random           :as random]
            [com.yetanalytics.datasim.model                 :as model]
            [com.yetanalytics.datasim.xapi.path             :as xp]
            [com.yetanalytics.datasim.xapi.profile          :as profile]
            [com.yetanalytics.datasim.xapi.profile.activity :as activity]
            [com.yetanalytics.datasim.xapi.profile.verb     :as verb]
            [com.yetanalytics.datasim.xapi.profile.template :as template]))

;; Statement healing is currently limited to inserting missing required
;; properties; it does not fix properties that are not included but have
;; invalid values.

;; TODO: Either implement improved healing or apply warnings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::type-iri-map
  ::profile/type-iri-map)

(s/def ::activity-map
  ::activity/activity-map)

(s/def ::verb-map
  ::verb/verb-map)

(s/def ::alignments
  ::model/alignments)

(s/def ::template
  ::template/template)

(s/def ::registration
  uuid?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- merge-nested
  "Perform a nested merge of `m2` onto `m1`. Recurses on maps and replaces
   all other values."
  [m1 m2]
  (merge-with
   (fn [v1 v2] (if (and (map? v1) (map? v2)) (merge-nested v1 v2) v2))
   m1
   m2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verbs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def verb-id-gen (s/gen :verb/id))

(defn- generate-verb [rng _]
  {"id" (stest/generate verb-id-gen 1 (random/rand-unbound-int rng))})

(s/fdef complete-verb
  :args (s/cat :verb   (s/nilable map?)
               :inputs (s/keys :req-un [::verb-map ::alignments])
               :rng    ::random/rng)
  :ret ::xs/verb)

(defn complete-verb
  [{verb-id "id" :as verb} {:keys [verb-map weights]} rng]
  (let [return-verb   (fn [_] verb)
        merge-verb    (fn [v] (merge-nested v verb))
        verb-weights  (:verbs weights)]
    (or
     ;; Verb found by ID
     (some->> verb-id
              verb-map
              merge-verb)
     ;; Verb w/ ID not found, return as-is
     (some->> verb-id
              return-verb)
     ;; Verb w/o ID not found, generate ID
     (some->> verb
              (generate-verb rng)
              merge-verb)
     ;; Choose random verb
     (some->> verb-map
              not-empty
              (random/choose-map rng verb-weights))
     ;; Generate random verb as verb map is empty
     (some->> {}
              (generate-verb rng)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def activity-id-gen (s/gen :activity/id))

(defn- generate-activity [rng _]
  {"id" (stest/generate activity-id-gen 1 (random/rand-unbound-int rng))
   "objectType" "Activity"})

(s/fdef complete-activity
  :args (s/cat :activity (s/nilable map?)
               :inputs   (s/keys :req-un [::activity-map ::alignments])
               :rng      ::random/rng)
  :ret ::xs/activity)

(defn complete-activity
  [{activity-id "id" {activity-type "type"} "definition" :as activity}
   {:keys [activity-map weights]}
   rng]
  (let [return-activity   (fn [_] activity)
        merge-activity    (fn [a] (merge-nested a activity))
        activity-weights  (get weights :activities)
        act-type-weights  (get weights :activity-types)]
    (or
     ;; Get activity by ID
     (some->> activity-id
              (get (reduce merge {} (vals activity-map)))
              merge-activity)
     ;; Get activity by type
     (some->> activity-type
              (get activity-map)
              not-empty
              (random/choose-map rng activity-weights)
              merge-activity)
     ;; Activity w/ ID not found, return as-is
     (some->> activity-id
              return-activity)
     ;; Activity w/o ID not found, assoc generated
     (some->> activity
              (generate-activity rng)
              merge-activity)
     ;; Choose random activity
     (some->> activity-map
              not-empty
              (random/choose-map rng act-type-weights)
              (random/choose-map rng activity-weights))
     ;; Generate random activity as activity map is empty
     (some->> {}
              (generate-activity rng)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (merge (stest/generate agent-gen 1 (random/rand-unbound-int rng)) agent*)
      (and account
           (or (not (contains? account "homePage"))
               (not (contains? account "name"))))
      (update agent*
              "account"
              (partial merge (stest/generate agent-account-gen
                                             1
                                             (random/rand-unbound-int rng))))
      :else
      agent*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (merge (stest/generate group-gen 1 (random/rand-unbound-int rng)) group*)
      (and account
           (or (not (contains? account "homePage"))
               (not (contains? account "name"))))
      (update group*
              "account"
              (partial merge (stest/generate group-account-gen
                                             1
                                             (random/rand-unbound-int rng))))
      :else
      group*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actors (Agents and Groups)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef complete-context
  :args (s/cat :context (s/nilable map?)
               :inputs  (s/keys :req-un [::activity-map
                                         ::alignments
                                         ::template
                                         ::registration])
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attachments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def attachment-gen (s/gen ::xs/attachment))

(defn- complete-attachment
  [rng attachment]
  (merge (stest/generate attachment-gen 1 (random/rand-unbound-int rng)) attachment))

(s/fdef complete-attachments
  :args (s/cat :attachments (s/coll-of map?)
               :rng ::random/rng)
  :ret ::xs/attachments)

(defn complete-attachments
  [attachments rng]
  (mapv (partial complete-attachment rng) attachments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Ref
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                                          (random/rand-unbound-int rng)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sub Statement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                        (get xp/default-object-type-m ["object" "object"])
                        props)]
      (condp #(contains? %2 %1) types
        "activity"      (complete-activity object inputs rng)
        "agent"         (complete-agent object rng)
        "group"         (complete-group object rng)
        "statement-ref" (complete-statement-ref object rng)
        (throw (ex-info "Unknown SubStatement object type"
                        {:kind   ::unknown-object-type
                         :sub?   true
                         :object object
                         :types  types}))))))

(s/fdef complete-sub-statement
  :args (s/cat :sub-statement map?
               :inputs (s/keys :req-un [::type-iri-map
                                        ::activity-map
                                        ::alignments
                                        ::template
                                        ::registration])
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                         :sub?   false
                         :object object
                         :types  types}))))))

(s/fdef complete-statement
  :args (s/cat :statement map?
               :inputs (s/keys :req-un [::verb-map
                                        ::activity-map
                                        ::alignments
                                        ::registration
                                        ::template])
               :rng ::random/rng)
  :ret ::xs/statement)

(defn complete-statement
  "Given `statement`, perform statement healing by filling in missing
   values using `rng` and values from `inputs`."
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
