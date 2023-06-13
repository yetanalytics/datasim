(ns com.yetanalytics.datasim.xapi.path
  "Given a path into an xAPI structure, return a spec from xapi-schema"
  (:require [com.yetanalytics.datasim.json :as json]
            [com.yetanalytics.datasim.json.schema :as jschema]
            [clojure.spec.alpha :as s]
            [clojure.set :as cset]
            [xapi-schema.spec :as xs]))

(defn prefix-path?
  [prefix path]
  (and (<= (count prefix) (count path))
       (->> (map = prefix path) (every? true?))))

(defn spec-hinted-path
  [path]
  (let [prefix-path* (fn [coll prefix]
                       (cond-> coll (prefix-path? prefix path) (conj prefix)))]
    (-> []
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
   ["object" "object"]               #{"activity" "agent" "group" "statement-ref"}
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
  {nil            #{"activity" "agent" "group" "statement-ref" "sub-statement"}
   "objectType"   #{"activity" "agent" "group" "statement-ref" "sub-statement"}
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
   "result"       #{"sub-statement"}
   "attachments"  #{"sub-statement"}
   "timestamp"    #{"sub-statement"}})

(defn paths->spec-hints
  [initial-type-set prefix paths]
  (let [prop-idx (count prefix)]
    (reduce (fn [acc path]
              (let [prop-set (->> prop-idx
                                  (get path)
                                  (get spec-hint-properties))]
                (cset/intersection acc prop-set)))
            initial-type-set
            paths)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  {::xs/statement                       "statement"

   :statement/actor                    ::xs/actor
   :statement/verb                     ::xs/verb
   :statement/object                   (fn [{:strs [objectType]
                                             :as   object}]
                                         (case objectType
                                           "StatementRef" ::xs/statement-ref
                                           "SubStatement" ::xs/sub-statement
                                           "Agent"        ::xs/agent
                                           "Group"        ::xs/group
                                           ::xs/activity))
   :statement/result                   ::xs/result
   :statement/context                  ::xs/context
   :statement/authority                ::xs/actor
   :statement/attachments              ::xs/attachments

   ::xs/attachments                     [::xs/attachment]

   ::xs/attachment                      "attachment"

   :attachment/display                 ::xs/language-map
   :attachment/description             ::xs/language-map
   ::xs/actor                           (fn [{:strs [objectType]
                                              :as   actor}]
                                          (if (= "Group" objectType)
                                            ::xs/group
                                            ::xs/agent))

   ::xs/agent                           "agent"

   :agent/account                      ::xs/account

   ::xs/group                           "group"

   :group/account                      ::xs/account
   :group/member                       [::xs/agent]

   ::xs/account                         "account"

   ::xs/verb                            "verb"
   :verb/display                       ::xs/language-map

   ::xs/statement-ref                   "statement-ref"

   ::xs/sub-statement                   "sub-statement"

   :sub-statement/actor                ::xs/actor
   :sub-statement/verb                 ::xs/verb
   :sub-statement/result               ::xs/result
   :sub-statement/context              ::xs/context
   :sub-statement/attachments          ::xs/attachments

   :sub-statement/object               (fn [{:strs [objectType]
                                             :as   object}]
                                         (case objectType
                                           "StatementRef" ::xs/statement-ref
                                           "Agent"        ::xs/agent
                                           "Group"        ::xs/group
                                           ::xs/activity))

   ::xs/activity                        "activity"

   :activity/definition                "definition"

   :definition/name                    ::xs/language-map
   :definition/description             ::xs/language-map
   :definition/choices                 ::xs/interaction-components
   :definition/scale                   ::xs/interaction-components
   :definition/source                  ::xs/interaction-components
   :definition/target                  ::xs/interaction-components
   :definition/steps                   ::xs/interaction-components
   :definition/extensions              ::xs/extensions

   :definition/correctResponsesPattern [string?]

   ::xs/interaction-components          [::xs/interaction-component]

   ::xs/interaction-component           "interaction-component"

   :interaction-component/description  ::xs/language-map

   ::xs/result                          "result"

   :result/extensions                  ::xs/extensions

   :result/score                       "score"

   ::xs/context                         "context"

   :context/instructor                 ::xs/actor
   :context/team                       ::xs/group
   :context/contextActivities          ::xs/context-activities
   :context/statement                  ::xs/statement-ref
   :context/extensions                 ::xs/extensions

   ::xs/context-activities              "contextActivities"

   :contextActivities/parent           ::xs/context-activities-array
   :contextActivities/grouping         ::xs/context-activities-array
   :contextActivities/category         ::xs/context-activities-array
   :contextActivities/other            ::xs/context-activities-array

   ::xs/context-activities-array        [::xs/activity]

   ::xs/language-map                    {:keys ::xs/language-tag
                                         :vals ::xs/language-map-text}
   ::xs/extensions                      {:keys ::xs/iri
                                         :vals ::json/any}})

(defn- validate-string-path-key [path-key]
  (when-not (string? path-key)
    (throw (ex-info (format "Key %s is not a string key" path-key)
                    {:type ::invalid-path-map-key
                     :key  path-key}))))

(defn- validate-wildcard-path-key [path-key]
  (when-not (= '* path-key)
    (throw (ex-info (format "Key %s does not represent an array index" path-key)
                    {:type ::invalid-path-array-key
                     :key  path-key}))))

(defn- validate-spec-path-key [spec path-key]
  (when-not (s/valid? spec path-key)
    (throw (ex-info (format "Key %s does not conform to spec" path-key)
                    {:type ::invalid-path-spec-key
                     :key  path-key
                     :spec spec}))))

(defn- throw-unsupported-object-types
  [spec path object-types]
  (throw (ex-info (format "Unsupported combination of object types on path %s for %s: %s"
                          path
                          spec
                          object-types)
                  {:type ::unsuppored-object-types
                   :spec spec
                   :path path
                   :object-types object-types})))

;; TODO: Distinguish between activity, context, and result extensions
(defn- path-spec-extensions
  [extension-id {:keys [iri-map]}]
  (or (some->> extension-id
               (get iri-map)
               :inlineSchema
               (jschema/schema->spec nil))
      ::json/any))

(defmulti path-spec
  (fn path-spec-dispatch [spec _path _p _hint-data] spec))

(defmethod path-spec :default [spec path p _]
  (throw (ex-info
          (if (keyword? spec)
            (format "Spec %s is a scalar or not defined in xapi-schema" spec)
            (format "Spec is not a keyword"))
          {:type ::unknown-path-spec
           :spec spec
           :path path
           :next p})))

;; Statement specs

(defmethod path-spec ::xs/statement [_ path p _]
  (validate-string-path-key p)
  [(conj path p) (keyword "statement" p)])

(defn- statement-object-spec-dispatch
  [object-type-m path]
  (let [types (get object-type-m path)]
    (case types
      #{"activity"}      ::xs/activity
      #{"agent" "group"} ::xs/actor
      #{"agent"}         ::xs/agent
      #{"group"}         ::xs/group
      #{"statement-ref"} ::xs/statement-ref
      #{"sub-statement"} ::xs/sub-statement
      (if (contains? types "activity")
        ::xs/activity
        (throw-unsupported-object-types :statement/object path types)))))

(defmethod path-spec :statement/object [_ path _ {:keys [object-types]}]
  ;; path = ["object"]
  [path (statement-object-spec-dispatch object-types path)])

(defmethod path-spec :statement/actor [_ path _ _]
  [path ::xs/actor])
(defmethod path-spec :statement/verb [_ path _ _]
  [path ::xs/verb])
(defmethod path-spec :statement/result [_ path _ _]
  [path ::xs/result])
(defmethod path-spec :statement/context [_ path _ _]
  [path ::xs/context])
(defmethod path-spec :statement/authority [_ path _ _]
  [path ::xs/actor])
(defmethod path-spec :statement/attachments [_ path _ _]
  [path ::xs/attachments])

;; Actor specs

;; custom specs that allow us to gen agents or groups
(s/def :actor/objectType #{"Agent" "Group"})
(s/def ::actor (s/or :agent ::xs/agent :group ::xs/group))

(defn- actor-type-dispatch [object-type-map path]
  (let [types (get object-type-map path)]
    (case types
      #{"agent" "group"} ::actor
      #{"agent"}         ::xs/agent
      #{"group"}         ::xs/group
      (throw-unsupported-object-types ::xs/actor path types))))

(defmethod path-spec ::xs/actor [_ path _ {:keys [object-types]}]
  [path (actor-type-dispatch object-types path)])

(defmethod path-spec ::actor [_ path p _]
  (case p
    "objectType"
    [(conj path p) :actor/objectType]
    "member"
    [(conj path p) :group/member]
    ;; else
    [(conj path p) (keyword "agent" p)]))

(defmethod path-spec ::xs/account [_ path p _]
  [(conj path p) (keyword "account" p)])

;; Agent specs

(defmethod path-spec ::xs/agent [_ path p _]
  (validate-string-path-key p)
  [(conj path p) (keyword "agent" p)])

(defmethod path-spec :agent/account [_ path _ _]
  [path ::xs/account])

;; Group specs

(defmethod path-spec ::xs/group [_ path p _]
  (validate-string-path-key p)
  [(conj path p) (keyword "group" p)])

(defmethod path-spec :group/account [_ path _ _]
  [path ::xs/account])

(defmethod path-spec :group/member [_ path p _]
  (validate-wildcard-path-key p)
  [(conj path p) ::xs/agent])

;; Verb specs

(defmethod path-spec ::xs/verb [_ path p _]
  (validate-string-path-key p)
  [(conj path p) (keyword "verb" p)])

(defmethod path-spec :verb/display [_ path _ _]
  [path ::xs/language-map])

;; Statement Ref specs

(defmethod path-spec ::xs/statement-ref [_ path p _]
  (validate-string-path-key p)
  [(conj path p) (keyword "statement-ref" p)])

;; Sub Statement specs

(defmethod path-spec ::xs/sub-statement [_ path p _]
  (validate-string-path-key p)
  [(conj path p) (keyword "sub-statement" p)])

(defn- sub-statement-type-dispatch [path object-type-map]
  (let [types (get object-type-map path)]
    (case types
      #{"activity"}      ::xs/activity
      #{"agent" "group"} ::xs/actor
      #{"agent"}         ::xs/agent
      #{"group"}         ::xs/group
      #{"statement-ref"} ::xs/statement-ref
      (if (contains? types "activity")
        ;; default to activity
        ::xs/activity
        (throw-unsupported-object-types :sub-statement/object path types)))))

(defmethod path-spec :sub-statement/object [_ path _ {:keys [object-types]}]
  ;; path = ["object" "object"]
  [path (sub-statement-type-dispatch path object-types)])

(defmethod path-spec :sub-statement/actor [_ path _ _]
  [path ::xs/actor])
(defmethod path-spec :sub-statement/verb [_ path _ _]
  [path ::xs/verb])
(defmethod path-spec :sub-statement/result [_ path _ _]
  [path ::xs/result])
(defmethod path-spec :sub-statement/context [_ path _ _]
  [path ::xs/context])
(defmethod path-spec :sub-statement/attachments [_ path _ _]
  [path ::xs/attachments])

;; Activity specs

(defmethod path-spec ::xs/activity [_ path p _]
  (validate-string-path-key p)
  [(conj path p) (keyword "activity" p)])

(defmethod path-spec :activity/definition [_ path p _]
  (validate-string-path-key p)
  [(conj path p) (keyword "definition" p)])

(defmethod path-spec :definition/name [_ path _ _]
  [path ::xs/language-map])
(defmethod path-spec :definition/description [_ path _ _]
  [path ::xs/language-map])
(defmethod path-spec :definition/choices [_ path _ _]
  [path ::xs/interaction-components])
(defmethod path-spec :definition/scale [_ path _ _]
  [path ::xs/interaction-components])
(defmethod path-spec :definition/source [_ path _ _]
  [path ::xs/interaction-components])
(defmethod path-spec :definition/target [_ path _ _]
  [path ::xs/interaction-components])
(defmethod path-spec :definition/steps [_ path _ _]
  [path ::xs/interaction-components])

(defmethod path-spec :definition/extensions [_ path _ _]
  [path ::xs/extensions])

;; This is just to avoid complications with putting a fn in a place
;; where keywords normally go
(s/def :correctResponsesPattern/string string?)

(defmethod path-spec :definition/correctResponsesPattern [_ path p _]
  (validate-wildcard-path-key p)
  [(conj path p) :correctResponsesPattern/string])

(defmethod path-spec ::xs/interaction-components [_ path p _]
  (validate-wildcard-path-key p)
  [(conj path p) ::xs/interaction-component])

(defmethod path-spec ::xs/interaction-component [_ path p _]
  (validate-string-path-key p)
  [(conj path p) (keyword "interaction-component" p)])

(defmethod path-spec :interaction-component/description [_ path _ _]
  [path ::xs/language-map])

;; Result specs

(defmethod path-spec ::xs/result [_ path p _]
  (validate-string-path-key p)
  [(conj path p) (keyword "result" p)])

(defmethod path-spec :result/extensions [_ path _ _]
  [path ::xs/extensions])

(defmethod path-spec :result/score [_ path p _]
  (validate-string-path-key p)
  [(conj path p) (keyword "score" p)])

;; Context specs

(defmethod path-spec ::xs/context [_ path p _]
  (validate-string-path-key p)
  [(conj path p) (keyword "context" p)])

(defmethod path-spec :context/instructor [_ path _ _]
  [path ::xs/actor])
(defmethod path-spec :context/team [_ path _ _]
  [path ::xs/group])
(defmethod path-spec :context/contextActivities [_ path _ _]
  [path ::xs/context-activities])
(defmethod path-spec :context/statement [_ path _ _]
  [path ::xs/statement-ref])
(defmethod path-spec :context/extensions [_ path _ _]
  [path ::xs/extensions])

(defmethod path-spec ::xs/context-activities [_ path p _]
  (validate-string-path-key p)
  [(conj path p) (keyword "contextActivities" p)])

(defmethod path-spec :contextActivities/parent [_ path _ _]
  [path ::xs/context-activities-array])
(defmethod path-spec :contextActivities/grouping [_ path _ _]
  [path ::xs/context-activities-array])
(defmethod path-spec :contextActivities/category [_ path _ _]
  [path ::xs/context-activities-array])
(defmethod path-spec :contextActivities/other [_ path _ _]
  [path ::xs/context-activities-array])

(defmethod path-spec ::xs/context-activities-array [_ path p _]
  (validate-wildcard-path-key p)
  [(conj path p) ::xs/activity])

;; Attachment specs

(defmethod path-spec ::xs/attachments [_ path p _]
  (validate-wildcard-path-key p)
  [(conj path p) ::xs/attachment])

(defmethod path-spec ::xs/attachment [_ path p _]
  (validate-string-path-key p)
  [(conj path p) (keyword "attachment" p)])

(defmethod path-spec :attachment/display [_ path _ _]
  [path ::xs/language-map])
(defmethod path-spec :attachment/description [_ path _ _]
  [path ::xs/language-map])

;; Map axiom specs

(defmethod path-spec ::xs/language-map [_ path p _]
  (validate-spec-path-key ::xs/language-tag p)
  [(conj path p) ::xs/language-map-text])

(defmethod path-spec ::xs/extensions [_ path p _]
  (validate-spec-path-key ::xs/iri p)
  [(conj path p) ::json/any])

(defn path->spec-3
  "Given a root `spec` and a `path` into it, return the spec for
   that path, or throw an exception if not possible.
   Accepts `hint-data` for polymorphic objectTypes and extensions."
  [spec path hint-data]
  (loop [spec   spec
         prefix []
         suffix path]
    (if-some [p (first suffix)]
      (let [[prefix* new-spec] (path-spec spec prefix p hint-data)
            suffix* (cond
                      ;; Short-circuit on extension
                      (= ::json/any new-spec)
                      []
                      ;; We advanced one spot in the path
                      (= (-> prefix count inc) (-> prefix* count))
                      (rest suffix)
                      ;; Silent traversal along equivalent specs 
                      :else suffix)]
        (recur new-spec prefix* suffix*))
      (cond
        ;; Treat extensions specially
        (= ::json/any spec)
        (path-spec-extensions (peek prefix) hint-data)
        ;; Recognized spec
        (or (s/get-spec spec)
            (fn? spec)
            (s/spec? spec))
        spec
        ;; Bad or unrecognized spec
        :else
        (throw (ex-info "Must return a valid, registered spec or a function or a spec literal"
                        {:type ::invalid-spec
                         :spec spec
                         :path prefix
                         :hint hint-data}))))))

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

(comment
  (path->spec
   ::xs/statement
   ["object"]
   {"object" {"objectType" "Activity"}}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn path->valueset
  "Derive the appropriate set of values, taken from the profile cosmos, from
   `path`."
  [spec-hints
   {:keys [verbs verb-ids activities activity-ids activity-types]}
   path]
  (let [;; If `path` points to a SubStatement property, lop off the prefix
        path*    (cond-> path
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
      ["context" "contextActivities" "category" *]
      activities
      ["context" "contextActivities" "grouping" *]
      activities
      ["context" "contextActivities" "parent" *]
      activities
      ["context" "contextActivities" "other" *]
      activities
      ["context" "contextActivities" "category" * "id"]
      activity-ids
      ["context" "contextActivities" "grouping" * "id"]
      activity-ids
      ["context" "contextActivities" "parent" * "id"]
      activity-ids
      ["context" "contextActivities" "other" * "id"]
      activity-ids
      ["context" "contextActivities" "category" * "definition" "type"]
      activity-types
      ["context" "contextActivities" "grouping" * "definition" "type"]
      activity-types
      ["context" "contextActivities" "parent" * "definition" "type"]
      activity-types
      ["context" "contextActivities" "other" * "definition" "type"]
      activity-types
      ;; Otherwise none
      nil)))
