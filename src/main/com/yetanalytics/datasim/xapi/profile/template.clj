(ns com.yetanalytics.datasim.xapi.profile.template
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.pan.objects.template :as template]
            [com.yetanalytics.datasim.xapi.rule    :as rule]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::statement-base-map
  (s/map-of ::template/id map?))

(s/def ::parsed-rules-map
  (s/map-of ::template/id (s/every ::rule/parsed-rule)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Rule Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef template->parsed-rules
  :args (s/cat :type-iri-map ::type-iri-map
               :activity-map ::activity-map
               :template     ::template/template)
  :ret (s/every ::rule/parsed-rule))

(defn template->parsed-rules
  "Return a collection of parsed rules derived from the template `rules`.
   Uses the object Determining Properties to assist with rule parsing."
  [{object-activity-type :objectActivityType
    object-statement-ref :objectStatementRefTemplate
    rules                :rules}]
  (cond
    object-activity-type
    (rule/parse-rules :activity-type rules)
    object-statement-ref
    (rule/parse-rules :statement-ref rules)
    :else
    (rule/parse-rules rules)))

(s/fdef update-parsed-rules
  :args (s/cat :type-iri-map ::type-iri-map
               :activity-map ::activity-map
               :parsed-rules (s/every ::rule/parsed-rule))
  :ret (s/every ::rule/parsed-rule))

(defn update-parsed-rules
  "Use information from `iri-map` and `activities` maps, to complete the
   `parsed-rules` by adding additional valuesets or spec generators."
  [type-iri-map activity-map parsed-rules]
  (let [iri-verb-map   (get type-iri-map "Verb")
        verbs          (->> iri-verb-map vals set)
        verb-ids       (->> iri-verb-map keys set)
        activities     (->> activity-map vals (mapcat vals) set)
        activity-ids   (->> activity-map vals (mapcat keys) set)
        activity-types (->> activity-map keys set)
        value-sets     {:verbs          verbs
                        :verb-ids       verb-ids
                        :activities     activities
                        :activity-ids   activity-ids
                        :activity-types activity-types}]
    (mapv (partial rule/add-rule-valuegen type-iri-map value-sets)
          parsed-rules)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profile Templates -> Statement Base + Parsed Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef create-statement-base-map
  :args (s/cat :templates (s/every ::template/template))
  :ret ::statement-base-map)

(defn create-statement-base-map
  "Given Statement Templates in `type-iri-map`, return a map from those
   templates' IDs to the base xAPI Statements they form from their
   Determining Properties and inScheme."
  [type-iri-map]
  (->> (get type-iri-map "StatementTemplate")
       (reduce-kv (fn [m id template]
                    (->> template
                         template->statement-base
                         (assoc m id)))
                  {})))

(s/fdef create-parsed-rules-map
  :args (s/cat :templates (s/every ::template/template))
  :ret ::parsed-rules-map)

(defn create-parsed-rules-map
  "Given Statement Templates in `type-iri-map`, return a map from those
   templates' IDs to those their parsed rules"
  [type-iri-map verb-map activity-map]
  ;; TODO: Use map-values in Clojure 1.11
  (->> (get type-iri-map "StatementTemplate")
       (reduce-kv (fn [m id template]
                    (->> template
                         template->parsed-rules
                         (update-parsed-rules verb-map activity-map)
                         (assoc m id)))
                  {})))
