(ns com.yetanalytics.datasim.xapi.path
  "Given a path into an xAPI structure, return a spec from xapi-schema"
  (:require [com.yetanalytics.datasim.json :as json]
            [com.yetanalytics.datasim.json.schema :as jschema]
            [clojure.spec.alpha :as s]
            [clojure.set :as cset]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.util :as u]))

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

(defn prefix-path?
  [prefix path]
  (and (<= (count prefix) (count path))
       (->> (map = prefix path) (every? true?))))

(defn spec-hinted-path
  [path]
  (let [prefix-path* (fn [prefix] (when (prefix-path? prefix path) prefix))]
    (or
     ;; SubStatement paths
     (prefix-path* ["object" "actor"])
     (prefix-path* ["object" "object"])
     (prefix-path* ["object" "context" "instructor"])
     ;; Statement paths
     (prefix-path* ["actor"])
     (prefix-path* ["object"])
     (prefix-path* ["context" "instructor"])
     (prefix-path* ["authority"]))))

(def default-spec-hints
  {["object"]                        #{"activity" "agent" "group" "statement-ref" "sub-statement"}
   ["object" "object"]               #{"activity" "agent" "group" "statement-ref" "sub-statement"}
   ["actor"]                         #{"agent" "group"}
   ["object" "actor"]                #{"agent" "group"}
   ["context" "instructor"]          #{"agent" "group"}
   ["object" "context" "instructor"] #{"agent" "group"}
   ["authority"]                     #{"agent" "group"}})

(def object-type-kebab-case
  {"Activity"     "activity"
   "Agent"        "agent"
   "Group"        "group"
   "StatementRef" "statement-ref"
   "SubStatement" "sub-statement"})

(def spec-hint-properties
  {"objectType"   #{"activity" "agent" "group" "statement-ref" "sub-statement"}
   "id"           #{"activity" "statement-ref" "sub-statement"}
   "definition"   #{"activity"}
   "name"         #{"agent" "group"}
   "mbox"         #{"agent" "group"}
   "mbox_sha1sum" #{"agent" "group"}
   "openid"       #{"agent" "group"}
   "account"      #{"agent" "group"}
   "member"       #{"group"}
   "actor"        #{"sub-statement"}
   "verb"         #{"sub-statement"}
   "object"       #{"sub-statement"}
   "context"      #{"sub-statement"}
   "results"      #{"sub-statement"}
   "attachments"  #{"sub-statement"}
   "timestamp"    #{"sub-statement"}})

(defn paths->spec-hints
  ([paths]
   (paths->spec-hints #{} paths))
  ([type-set paths]
   (reduce (fn [acc path]
             (let [prop-set (get spec-hint-properties (last path))]
               (cset/intersection acc prop-set)))
           type-set
           paths)))

;; Need to fill in for otherwise-nonexistent spec
(s/def :correctResponsesPattern/string string?)

(def array-element-specname
  {"category"                "activity"
   "grouping"                "activity"
   "parent"                  "activity"
   "other"                   "activity"
   "choices"                 "interaction-component"
   "scale"                   "interaction-component"
   "source"                  "interaction-component"
   "target"                  "interaction-component"
   "steps"                   "interaction-component"
   "correctResponsesPattern" "string"
   "member"                  "agent"
   "attachments"             "attachment"})

(defn path->valueset
  "Derive the appropriate set of values, taken from the profile cosmos, from
   `path`."
  [spec-hints
   {:keys [verbs verb-ids activities activity-ids activity-types]}
   path]
  (let [path*    (cond-> path
                   (and (#{"object"} (get path 0))
                        (#{"verb" "object" "context"} (get path 1)))
                   (subvec 1))
        activity? (fn [path] ((get spec-hints path #{}) "activity"))
        drop-one  (fn [path] (if (< 1 (count path)) (-> path pop) []))
        drop-two  (fn [path] (if (< 2 (count path)) (-> path pop pop) []))]
    (case path*
      ;; Verbs
      ["verb"]
      verbs
      ["verb" "id"]
      verb-ids
      ;; Object Activities
      ["object"]
      (when (activity? path) activities)
      ["object" "id"]
      (when (activity? (drop-one path)) activity-ids)
      ["object" "definition" "type"]
      (when (activity? (drop-two path)) activity-types)
      ;; Context Activities
      ["context" "contextActivities" "category" '*]
      activities
      ["context" "contextActivities" "grouping" '*]
      activities
      ["context" "contextActivities" "parent" '*]
      activities
      ["context" "contextActivities" "other" '*]
      activities
      ["context" "contextActivities" "category" '* "id"]
      activity-ids
      ["context" "contextActivities" "grouping" '* "id"]
      activity-ids
      ["context" "contextActivities" "parent" '* "id"]
      activity-ids
      ["context" "contextActivities" "other" '* "id"]
      activity-ids
      ["context" "contextActivities" "category" '* "definition" "type"]
      activity-types
      ["context" "contextActivities" "grouping" '* "definition" "type"]
      activity-types
      ["context" "contextActivities" "parent" '* "definition" "type"]
      activity-types
      ["context" "contextActivities" "other" '* "definition" "type"]
      activity-types
      ;; Otherwise none
      nil)))

;; (object-or "statement-object" #{"agent" "group"})
;; => (s/or :agent ::statement-object/agent :group ::statement-object/group)
(defn object-or
  [spec-ns object-types]
  (->> object-types
       (map (fn [obj-type] [(keyword obj-type) (keyword spec-ns obj-type)]))
       u/dynamic-or))

;; (object-property-or #{"agent" "group"} "name")
;; => (s/or :agent :agent/name :group :group/name)
(defn object-property-or
  [object-types prop-name]
  (->> object-types
       (map (fn [obj-type] [(keyword obj-type) (keyword obj-type prop-name)]))
       u/dynamic-or))

(defn path->spec-2
  "Given `path`, derive the appropriate spec."
  [iri-map spec-hints path]
  (let [len (count path)
        x2  (when (< 0 len) (get path (- len 1)))
        x1  (when (< 1 len) (get path (- len 2)))
        x0  (when (< 2 len) (get path (- len 3)))]
    (cond
      ;; $.object.object => :sub-statement/activity
      (= ["object" "object"] path)
      (object-or "sub-statement-object" (get spec-hints path))
      ;; $.object => :statement/activity
      (= ["object"] path)
      (object-or "statement-object" (get spec-hints path))
      ;; $.actor => ::xs/agent
      (#{["actor"] ["authority"] ["context" "instructor"]
         ["object" "actor"] ["object" "context" "instructor"]}
       path)
      (object-or xs-ns (get spec-hints path))
      ;; $.actor.name => :agent/name
      (and (#{["object"] ["actor"] ["context" "instructor"] ["authority"]
              ["object" "actor"] ["object" "object"] ["object" "context" "instructor"]}
            (vec (butlast path)))
           (string? x2))
      (object-property-or (get spec-hints path) x2)
      ;; $.result.extension['http://foo.org/extension']
      (and (#{"definition" "context" "result"} x0)
           (#{"extensions"} x1)
           (string? x2))
      (or (some->> x2 (get iri-map) :inlineSchema (jschema/schema->spec nil))
          any?)
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
      (keyword xs-ns (get array-element-specname x1))
      ;; $.attachments.*.id => :attachment/id
      (and (string? x0)
           (= '* x1)
           (string? x2))
      (keyword (get array-element-specname x0) x2)
      :else
      (throw (ex-info (format "Unsupported key-index combination in path: %s" path)
                      {:type ::invalid-path
                       :path path})))))
