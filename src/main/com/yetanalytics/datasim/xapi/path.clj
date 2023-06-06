(ns com.yetanalytics.datasim.xapi.path
  "Given a path into an xAPI structure, return a spec from xapi-schema"
  (:require [com.yetanalytics.datasim.json :as json]
            [clojure.spec.alpha :as s]
            [clojure.set :as cset]
            [xapi-schema.spec :as xs]))

(s/def :spec-map.map-spec/keys
  qualified-keyword?)

(s/def :spec-map.map-spec/vals
  qualified-keyword?)

(s/def :spec-map/map-spec
  (s/keys :req-un [:spec-map.map-spec/keys
                   :spec-map.map-spec/vals]))

(s/def ::spec-map
  (s/map-of
   qualified-keyword?
   (s/or :ns string?
         :keyword qualified-keyword?
         :fn fn?
         :vector vector?
         :map :spec-map/map-spec)))

(def spec-map
  {::xs/statement "statement"

   :statement/actor ::xs/actor
   :statement/verb ::xs/verb
   :statement/object
   (fn [{:strs [objectType] :as object}]
     (case objectType
       "StatementRef" ::xs/statement-ref
       "SubStatement" ::xs/sub-statement
       "Agent"        ::xs/agent
       "Group"        ::xs/group
       ::xs/activity))
   :statement/result ::xs/result
   :statement/context ::xs/context
   :statement/authority ::xs/actor
   :statement/attachments ::xs/attachments

   ::xs/attachments
   [::xs/attachment]

   ::xs/attachment
   "attachment"

   :attachment/display ::xs/language-map
   :attachment/description ::xs/language-map
   ::xs/actor
   (fn [{:strs [objectType] :as actor}]
     (if (= "Group" objectType)
       ::xs/group
       ::xs/agent))

   ::xs/agent
   "agent"

   :agent/account ::xs/account

   ::xs/group "group"

   :group/account ::xs/account
   :group/member [::xs/agent]

   ::xs/account "account"

   ::xs/verb "verb"
   :verb/display ::xs/language-map

   ::xs/statement-ref "statement-ref"

   ::xs/sub-statement "sub-statement"

   :sub-statement/actor ::xs/actor
   :sub-statement/verb ::xs/verb
   :sub-statement/result ::xs/result
   :sub-statement/context ::xs/context
   :sub-statement/attachments ::xs/attachments

   :sub-statement/object
   (fn [{:strs [objectType] :as object}]
     (case objectType
       "StatementRef" ::xs/statement-ref
       "Agent"        ::xs/agent
       "Group"        ::xs/group
       ::xs/activity))

   ::xs/activity "activity"

   :activity/definition "definition"

   :definition/name ::xs/language-map
   :definition/description ::xs/language-map
   :definition/choices ::xs/interaction-components
   :definition/scale ::xs/interaction-components
   :definition/source ::xs/interaction-components
   :definition/target ::xs/interaction-components
   :definition/steps ::xs/interaction-components
   :definition/extensions ::xs/extensions

   :definition/correctResponsesPattern [string?]

   ::xs/interaction-components
   [::xs/interaction-component]

   ::xs/interaction-component "interaction-component"

   :interaction-component/description ::xs/language-map

   ::xs/result "result"

   :result/extensions ::xs/extensions

   :result/score "score"

   ::xs/context "context"

   :context/instructor ::xs/actor
   :context/team ::xs/group
   :context/contextActivities ::xs/context-activities
   :context/statement ::xs/statement-ref
   :context/extensions ::xs/extensions

   ::xs/context-activities "contextActivities"

   :contextActivities/parent ::xs/context-activities-array
   :contextActivities/grouping ::xs/context-activities-array
   :contextActivities/category ::xs/context-activities-array
   :contextActivities/other ::xs/context-activities-array

   ::xs/context-activities-array
   [::xs/activity]

   ::xs/language-map {:keys ::xs/language-tag
                      :vals ::xs/language-map-text}
   ::xs/extensions {:keys ::xs/iri
                    :vals ::json/any}


   })

(comment
  (s/valid? ::spec-map spec-map)


  )

(defn path->spec
  "Given a root spec and a path into it, return the spec for
  that path or nil if it is not possible. Accepts hint data to dispatch
  multi-specs and the like"
  ([spec path]
   (path->spec spec path nil))
  ([spec path hint-data]
   (if (empty? path)
     (do
       (assert (or (s/get-spec spec)
                   (fn? spec)
                   (s/spec? spec))
               "Must return a valid, registered spec or a function or a spec literal")
       spec)
     (if-let [spec-entry (get spec-map spec)]
       (let [p-key (first path)]
         (cond
           ;; direct ref to another spec, these should be traversed silently
           (keyword? spec-entry)
           (recur
            spec-entry
            path
            hint-data)

           ;; an ns name, which gets used to speculatively form a keyword
           (string? spec-entry)
           (let [spec-ns spec-entry]
             (assert (string? p-key) "Path key for a map must be a string")
             (recur
              (keyword spec-ns p-key)
              (rest path)
              (get hint-data p-key)))

           ;; A vector just specifies that there is a homogenous array
           (vector? spec-entry)
           (let [[element-spec] spec-entry]
             (assert (number? p-key) "Path key for array must be a number")
             (recur
              element-spec
              (rest path)
              (get hint-data p-key)))

           ;; inference by dispatch function, must have data present or it uses
           ;; defaults
           (fn? spec-entry)
           (let [inferred-spec (spec-entry hint-data)]
             (recur
              inferred-spec
              path
              hint-data))
           ;; arbitrary maps
           (map? spec-entry)
           (let [{keys-spec :keys
                  vals-spec :vals} spec-entry]
             (assert (string? p-key) "Path key for arbitrary map must be a string")
             (if (s/valid? keys-spec p-key)
               (if (= vals-spec ::json/any)
                 ;; don't loop, any path under any-json is any-json
                 ::json/any
                 (recur
                  vals-spec
                  (rest path)
                  (get hint-data p-key)))
               (throw (ex-info "invalid key for string map"
                               {:type ::invalid-arbitrary-key
                                :key p-key
                                :spec spec
                                :keys-spec keys-spec}))))))
       (throw (ex-info "No spec in map"
                       {:type ::no-spec-in-map
                        :spec spec}))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def xs-ns "xapi-schema.spec")

(defn- array-element-spec-name
  [array-prop]
  (condp contains? array-prop
    #{"category" "grouping" "parent" "other"}
    "activity"
    #{"choices" "scale" "source" "target" "steps"}
    "interaction-component"
    #{"correctResponsesPattern"}
    "string-not-empty"
    #{"member"}
    "agent"
    #{"attachments"}
    "attachment"
    ;; else
    (throw (ex-info "Unknown array property"
                    {:type     ::invalid-array-property
                     :property array-prop}))))

(defn- follows-prefix?
  [prefix coll]
  (->> (map = prefix coll)
       (every? true?)))

(defn- rule-object-type [path-prefix rules]
  (let [path->rule-m
        (reduce (fn [m {:keys [specpath] :as rule}]
                  (cond-> m
                    (follows-prefix? path-prefix specpath)
                    (assoc specpath rule)))
                {}
                rules)
        object-type-vals
        (:valueset (get path->rule-m (conj path-prefix "objectType")))
        property-set
        (->> path->rule-m keys (drop (count path-prefix)) set)]
    {:object-type-vals object-type-vals
     :property-set     property-set}))

(defn- object-type [path-prefix rules]
  (let [{:keys [object-type-vals property-set]}
        (rule-object-type path-prefix rules)]
    (cond
      ;; Decide based on `objectType`
      ;; TODO: Take care of contradictions between objectType and
      ;; other properties
      (< 1 (count object-type-vals))
      (throw (ex-info "Multiple possible objectTypes not supported"
                      {:type ::multiple-object-types
                       :path (conj path-prefix "objectType")}))
      (= #{"Activity"} object-type-vals)     "activity"
      (= #{"Agent"} object-type-vals)        "agent"
      (= #{"Group"} object-type-vals)        "group"
      (= #{"StatementRef"} object-type-vals) "statement-ref"
      (= #{"SubStatement"} object-type-vals) "sub-statement"
      ;; Decide based on other properties
      (-> #{"member"}
          (cset/intersection property-set)
          not-empty)
      "group"
      (-> #{"name" "mbox" "mbox_sha1sum" "openid" "acount"}
          (cset/intersection property-set)
          not-empty)
      "agent" ; default to agent
      (-> #{"timestamp" "actor" "verb" "object" "context" "result" "attachments"}
          (cset/intersection property-set)
          not-empty)
      "sub-statement"
      ;; Default to activity
      :else
      "activity")))

(defn- actor-type [path-prefix rules]
  (let [{:keys [object-type-vals property-set]}
        (rule-object-type path-prefix rules)]
    (cond
      ;; Decide based on `objectType`
      (= #{"Agent"} object-type-vals) "agent"
      (= #{"Group"} object-type-vals) "group"
      ;; Decide based on other properties
      (-> #{"member"}
          (cset/intersection property-set)
          not-empty)
      "group"
      ;; Default to agent
      :else
      "agent")))

(defn rule-spec-keyword
  [parsed-rules path]
  (let [[x0 x1 x2] (take-last 3 path)]
    (cond
      ;; $.object => ::xs/activity
      (= "object" x2)
      (keyword xs-ns
               (object-type (->> [x1 x2] (filterv nil?)) parsed-rules))
      ;; $.actor => ::xs/agent
      (= "actor" x2)
      (keyword xs-ns
               (actor-type (->> [x1 x2] (filterv nil?)) parsed-rules))
      ;; $.object.id => :activity/id
      (= "object" x1)
      (keyword (object-type (->> [x0 x1] (filterv nil?)) parsed-rules)
               x2)
      ;; $.actor.name => :agent/name
      (= "actor" x1)
      (keyword (actor-type (->> [x0 x1] (filterv nil?)) parsed-rules)
               x2)
      ;; $.verb => ::xs/verb
      (and (nil? x1)
           (string? x2))
      (keyword xs-ns x2)
      ;; $.verb.id => :verb/id
      (and (string? x1)
           (string? x2))
      (keyword x1 x2)
      ;; $.attachments.* => ::xs/attachment
      (and (string? x1)
           (= '* x2))
      (keyword xs-ns (array-element-spec-name x1))
      ;; $.attachments.*.id => :attachment/id
      (and (string? x0)
           (= '* x1)
           (string? x2))
      (keyword (array-element-spec-name x0) (first x2))
      :else
      (throw (ex-info "Unsupported key-index combination"
                      {:type ::invalid-path
                       :path path})))))
