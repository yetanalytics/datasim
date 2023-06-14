(ns com.yetanalytics.datasim.xapi.path
  "Given a path into an xAPI structure, return a spec from xapi-schema"
  (:require [com.yetanalytics.datasim.json :as json]
            [clojure.spec.alpha :as s]
            [clojure.set :as cset]
            [xapi-schema.spec :as xs]))

(s/def ::path
  (s/coll-of (s/or :key string? :index #{(symbol "*")}) :kind vector?))

(s/def ::iri-map
  (s/map-of string? any?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path -> Object Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note that these functions are tested by the rule-test namespace instead
;; of the path-test namespace.

;; object type strings match the keyword names found in xapi-schema, e.g
;; `:statement-object/statement-ref`

(def object-type-strings
  #{"activity" "agent" "group" "statement-ref" "sub-statement"})

(s/def ::object-types
  (s/map-of ::path (s/coll-of object-type-strings :kind set?)))

(def object-type-kebab-case
  {"Activity"     "activity"
   "Agent"        "agent"
   "Group"        "group"
   "StatementRef" "statement-ref"
   "SubStatement" "sub-statement"})

(def default-object-type-m
  "Map from paths to their default objectTypes, if there were no constraints."
  {;; Objects
   ["object"] #{"activity" "agent" "group" "statement-ref" "sub-statement"}
   ["object" "object"] #{"activity" "agent" "group" "statement-ref"}
   ;; Actors
   ["actor"]                         #{"agent" "group"}
   ["object" "actor"]                #{"agent" "group"}
   ["context" "instructor"]          #{"agent" "group"}
   ["object" "context" "instructor"] #{"agent" "group"}
   ["authority"]                     #{"agent" "group"}})

(def spec-hint-properties-m
  "Map from property to the object types that the containing object can be.
   For example, `id` can be found in both activities and statement-refs."
  {nil            #{"activity" "agent" "group" "statement-ref" "sub-statement"}
   "objectType"   #{"activity" "agent" "group" "statement-ref" "sub-statement"}
   "id"           #{"activity" "statement-ref"}
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

(defn prefix-path?
  "Is `prefix` a prefix of the `path` vector?"
  [prefix path]
  (and (<= (count prefix) (count path))
       (->> (map = prefix path) (every? true?))))

(s/fdef object-type-paths
  :args (s/cat :path ::path)
  :ret (s/coll-of ::path))

(defn object-type-paths
  "Does `path` point to anywhere that can have multiple object types?
   Includes actor, object, context instructors, authority, and their
   SubStatement equivalents. Returns a coll of possible path prefixes.
   
   Returns `nil` if `path` does not potentially point to multiple object
   types (so, for example, context activities don't count since they
   can only be activities)."
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

(s/fdef path-object-type-set
  :args (s/cat :initial-type-set
               (s/? (s/coll-of object-type-strings :kind set?))
               :prefix ::path
               :paths (s/coll-of ::path))
  :ret (s/coll-of object-type-strings :kind set?))

(defn path-object-type-set
  "Derive the set of possible object types based off of all the `paths`,
   starting with an optional `initial-type-set`."
  ([prefix paths]
   (path-object-type-set (default-object-type-m prefix) prefix paths))
  ([initial-type-set prefix paths]
   (let [prop-idx (count prefix)]
     (reduce (fn [acc path]
               (let [prop-set (->> prop-idx
                                   (get path)
                                   (get spec-hint-properties-m))]
                 (cset/intersection acc prop-set)))
             initial-type-set
             paths))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path -> Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmulti path-spec
  "Return a `[path spec]` pair, where `path` is the new path that
   points to the value in the statement validated by `spec`."
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

;; TODO: Should ["object"] return ::xs/activity, etc. instead of
;; :statement/object? Likewise for ["actor"], ["context" "instructor"],
;; and ["authority"].

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

(defmethod path-spec :statement/object [_ path _ object-types]
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

(defmethod path-spec ::xs/actor [_ path _ object-types]
  [path (actor-type-dispatch object-types path)])

(defmethod path-spec ::actor [_ path p _]
  (validate-string-path-key p)
  (case p
    "objectType"
    [(conj path p) :actor/objectType]
    "member"
    [(conj path p) :group/member]
    ;; else
    [(conj path p) (keyword "agent" p)]))

(defmethod path-spec ::xs/account [_ path p _]
  (validate-string-path-key p)
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

;; TODO: Should ["object" "object"] return ::xs/activity, etc. instead of
;; :sub-statement/object? Likewise for ["object" "actor"] and
;; ["object" "context" "instructor"].

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

(defmethod path-spec :sub-statement/object [_ path _ object-types]
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

(s/fdef path->spec
  :args (s/cat :spec (s/or :keyword s/get-spec :spec-obj s/spec?)
               :path ::path
               :object-types ::object-types)
  :ret (s/or :keyword s/get-spec
             :function fn?
             :spec-obj s/spec?))

(defn path->spec
  "Given a root `spec` and a `path` into it, return the spec for
   that path, or throw an exception if not possible.
   Accepts `object-types` for polymorphic objectTypes."
  [spec path object-types]
  (loop [spec   spec
         prefix []
         suffix path]
    (if-some [p (first suffix)]
      (let [[prefix* new-spec] (path-spec spec prefix p object-types)
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
      (if (or (s/get-spec spec)
              (fn? spec)
              (s/spec? spec))
        spec
        ;; Bad or unrecognized spec
        (throw (ex-info "Must return a valid, registered spec or a function or a spec literal"
                        {:type         ::invalid-spec
                         :spec         spec
                         :path         prefix
                         :object-types object-types}))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path -> Valueset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Technically these specs should be more specific than `any?` for entries
;; but at this stage we don't really care.
(s/def ::verbs (s/coll-of any? :kind set?))
(s/def ::verb-ids (s/coll-of any? :kind set?))
(s/def ::activities (s/coll-of any? :kind set?))
(s/def ::activity-ids (s/coll-of any? :kind set?))
(s/def ::activity-types (s/coll-of any? :kind set?))

(s/fdef path->valueset
  :args (s/cat :spec-hints (s/keys :req-un [::object-types
                                            ::iri-map])
               :valuesets (s/keys :req-un [::verbs
                                           ::verb-ids
                                           ::activities
                                           ::activity-ids
                                           ::activity-types])
               :path ::path)
  :ret (s/nilable set?))

;; TODO: We can change this to a spec->valueset function in a straightforward
;; fashion; however, we cannot do so until we make the specs returned for
;; ["object"] by `path->spec` be `::xs/activity` or the like, instead of
;; `:statement/object` like it currently does.
(defn path->valueset
  "Derive the appropriate set of values, taken from the profile cosmos, from
   `path`."
  [object-types
   {:keys [verbs verb-ids activities activity-ids activity-types]}
   path]
  (let [;; If `path` points to a SubStatement property, lop off the prefix
        path*    (cond-> path
                   (and (#{"object"} (get path 0))
                        (#{"verb" "object" "context"} (get path 1)))
                   (subvec 1))
        activity? (fn [path] ((get object-types path #{}) "activity"))
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
