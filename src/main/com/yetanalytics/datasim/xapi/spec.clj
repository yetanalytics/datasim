(ns com.yetanalytics.datasim.xapi.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.walk :as w]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.random :as random]))

(def agent-gen (s/gen ::xs/agent))

(def group-gen (s/gen ::xs/group))

(def verb-gen (s/gen ::xs/verb))

(def activity-gen (s/gen ::xs/activity))

(def attachment-gen (s/gen ::xs/attachment))

(def authority-gen (s/gen (s/or :agent ::xs/agent
                                :oauth-consumer ::xs/oauth-consumer
                                :three-legged-oauth-group ::xs/tlo-group)))

(defn- fill-agent [_inputs agent]
  (merge (sgen/generate agent-gen)
         agent))

(defn- fill-group [_inputs group]
  (merge (sgen/generate group-gen)
         group))

(defn- get-by-pref-label
  [coll lang-map]
  (some (fn [{:keys [prefLabel] :as v}]
          (when (every? (fn [[ltag lval]]
                          (= lval (get prefLabel (keyword ltag))))
                        lang-map)
            v))
        coll))

(defn- merge-objects
  [input object]
  (merge-with
   (fn [in-val obj-val]
     (cond
       (and (not-empty in-val)
            (not-empty obj-val)
            (not= in-val obj-val))
       (throw (ex-info "Contradiction between input and template value"
                       {:type ::profile-contradiction}))
       (and (not-empty in-val)
            (empty? obj-val))
       in-val
       :else
       obj-val))
   input
   object))

(comment
  (merge-objects
   {"foo" "bar" "111" "222"}
   {"foo" "bar"}))

(defn- fill-verb
  "Return `verb` merged with the corresponding Verb concept found in the
   inputs. Generates the required `id` property and omits the optional
   `display` property if not found."
  [{:keys [rng iri-map alignments]}
   {:strs [id display] :as verb}]
  (let [verb-map     (->> iri-map
                          vals
                          (filter (comp #{"Verb"} :type)))
        verb-concept (cond
                       id      (get iri-map id)
                       display (get-by-pref-label verb-map display)
                       :else   (random/choose rng alignments verb-map))]
    (if (some? verb-concept)
      (merge-objects {"id"      (:id verb-concept)
                      "display" (w/stringify-keys (:prefLabel verb-concept))}
                     verb)
      (merge (sgen/generate verb-gen) verb))))

(comment
  (fill-verb {:rng (random/seed-rng 100)
              :iri-map {"http://verb.org"
                        {:id "http://verb.org"
                         :type "Verb"
                         :prefLabel {:en-us "Verb 1"}
                         :definition {:en-us "The First Verb"}}}
              :alignments {"http://verb.org"
                           {:weight 0.0}}}
             {"id" "http://verb.org"
              "display" {"en-us" "Verb 2"}}))

(def activity-definition-keyset
  #{:name :description :type :moreInfo
    :interactionType :correctResponsesPattern
    :choices :scale :source :target :steps :extensions})

(defn- fill-activity
  [{:keys [rng activities alignment]}
   {id "id" {:strs [type]} "definition" :as activity}]
  (let [all-activities
        (reduce merge (vals activities))
        validate-input-activity
        (fn [input]
          (if (not= input (merge input activity))
            (throw (ex-info "Activities from input and template rule contradict"
                            {:type          ::bad-activity
                             :activity      activity
                             :activity-id   id
                             :activity-type type
                             :input         input}))
            (merge input activity)))]
    (cond
      (and id type)
      (let [input (get-in activities [type id])]
        (validate-input-activity input))
      (and id (not type))
      (let [input (get all-activities id)]
        (validate-input-activity input))
      (and (not id) type)
      (let [input (->> (get activities type)
                       (random/choose rng alignment)
                       second)]
        (validate-input-activity input))
      :else
      (let [input (->> all-activities
                       (random/choose rng alignment)
                       second)]
        (validate-input-activity input)))))

(comment
  (fill-activity {:rng (random/seed-rng 100)
                  :activities {"http://activity-type.org"
                               {"http://activity.org"
                                {"id" "http://activity.org"
                                 "description"
                                 {"type" "http://activity-type.org"
                                  "choices" ["foo:bar"
                                             "xyz"
                                             "baz:qux"]}}}}
                  :alignments {"http://verb.org"
                               {:weight 0.0}}}
                 {"id" "http://activity.org"
                  "description"
                  {"choices" ["foo:bar"
                              "xyz"
                              "baz:qux"]}}))

(defn- fill-statement-ref
  [_inputs statement-ref]
  (merge (sgen/generate (s/gen ::xs/statement-ref)) statement-ref))

(defn- fill-sub-statement
  [_inputs sub-statement]
  (merge (sgen/generate (s/gen ::xs/sub-statement)) sub-statement))

(defmulti fill-object (fn [_ k _] k))

(defmethod fill-object :statement
  [{:keys [rng] :as inputs}
   _
   {:strs [id context authority attachments] :as statement}]
  (let [fill-object (partial fill-object inputs)]
    ;; No need to fill in results since all properties there are optional
    ;; No need to fill in optional scalars like timestamp, stored, or version
    (cond-> statement
      (not id)
      (assoc "id" (random/rand-uuid rng))
      true
      (update "actor" (partial fill-object :statement/actor))
      true
      (update "verb" (partial fill-object :statement/verb))
      true
      (update "object" (partial fill-object :statement/object))
      context
      (update "context" (partial fill-object :statement/context))
      authority
      (update "authority" (partial fill-object :statement/authority))
      attachments
      (update "attachments" (partial fill-object :statement/attachments)))))

(defmethod fill-object :statement/actor [inputs _ actor]
  (let [input (w/stringify-keys (:actor inputs))]
    (cond
      (empty? actor)
      input
      (and (not-empty actor)
           ;; Assume the input actor has every property, including name
           (not= input (merge input actor)))
      (throw (ex-info "Actors from input and template rule contradict"
                      {:type  ::bad-actor
                       :actor actor
                       :input input}))
      :else
      actor)))

(defmethod fill-object :statement/verb [inputs _ verb]
  (fill-verb inputs verb))

(defmethod fill-object :statement/object [inputs _ object]
  (let [object-type (get object "objectType")]
    (case object-type
      "Activity"
      (fill-activity inputs object)
      "Agent"
      (fill-agent inputs object)
      "Group"
      (fill-group inputs object)
      "StatementRef"
      (fill-statement-ref inputs object)
      "SubStatement"
      (fill-sub-statement inputs object)
      ; Activity by default
      (fill-activity inputs object))))

(defn- fill-context-activity
  [{:keys [rng activities alignment] :as input} context-activity]
  (if (empty? context-activity)
    (let [activity-coll (reduce merge (vals activities))
          activity-id   (random/choose rng alignment (keys activity-coll))]
      (get activity-coll activity-id))
    (fill-activity input context-activity)))

(defn- fill-context-activities
  [input context-activity-coll]
  (mapv (partial fill-context-activity input) context-activity-coll))

(defmethod fill-object :statement/context
  [inputs
   _
   {:strs [instructor team statement]
    {:strs [category grouping parent other]} "contextActivities"
    :as context}]
  (cond-> context
    instructor
    (update "instructor" (partial fill-agent inputs))
    team
    (update "team" (partial fill-group inputs))
    statement
    (update "statement" (partial fill-statement-ref inputs))
    category
    (update-in ["contextActivities" "category"]
               (partial fill-context-activities inputs))
    grouping
    (update-in ["contextActivities" "grouping"]
               (partial fill-context-activities inputs))
    parent
    (update-in ["contextActivities" "parent"]
               (partial fill-context-activities inputs))
    other
    (update-in ["contextActivities" "other"]
               (partial fill-context-activities inputs))))

(defn- fill-attachment
  [{:keys [rng iri-map alignments]}
   {:strs [usageType display description] :as attachment}]
  (let [{chosen-usage-type  :id
         chosen-display     :prefLabel
         chosen-description :definition}
        (->> iri-map
             vals
             (filter (fn [{t :type}] (= "AttachmentUsageType" t)))
             (random/choose rng alignments))
        gen-attachment
        (sgen/generate attachment-gen)]
    (cond-> (merge gen-attachment attachment)
      (and (not usageType) chosen-usage-type)
      (assoc "usageType" chosen-usage-type)
      (and (not display) chosen-display)
      (assoc "display" chosen-display)
      (and (not description) chosen-description)
      (assoc "description" chosen-description))))

(defmethod fill-object :statement/attachments
  [inputs _ attachments]
  (mapv (partial fill-attachment inputs) attachments))

(defmethod fill-object :statement/authority
  [_ _ authority]
  (merge (sgen/generate authority-gen) authority))
