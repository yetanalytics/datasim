(ns com.yetanalytics.datasim.xapi.profile.template.rule
  "Apply statement template rules for generation"
  (:require [clojure.core.memoize           :as memo]
            [clojure.set                    :as cset]
            [clojure.string                 :as cstr]
            [clojure.spec.alpha             :as s]
            [clojure.test.check.generators  :as gen]
            [clojure.walk                   :as w]
            [xapi-schema.spec               :as xs]
            [com.yetanalytics.pathetic      :as path]
            [com.yetanalytics.pathetic.path :as jpath]
            [com.yetanalytics.pan.objects.templates.rule :as rule]
            [com.yetanalytics.datasim.json      :as j]
            [com.yetanalytics.datasim.xapi.path :as xp]
            [com.yetanalytics.datasim.random    :as random]
            [com.yetanalytics.datasim.json.schema :as jschema]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::location ::jpath/paths)

(s/def ::presence
  #{:included :excluded :recommended})

(s/def ::path
  ::xp/path)

(s/def ::any
  (s/every ::j/any :kind set? :into #{}))

(s/def ::all
  (s/every ::j/any :kind set? :into #{}))

(s/def ::none
  (s/every ::j/any :kind set? :into #{}))

(s/def ::valueset
  (s/every ::j/any :kind set? :into #{} :min-count 1))

(s/def ::spec
  (s/or :keyword s/get-spec :pred s/spec?))

(s/def ::generator
  gen/generator?)

(s/def ::parsed-rule
  (s/keys :req-un [::location
                   ::path
                   ::spec]
          :opt-un [::presence
                   ::any
                   ::all
                   ::none
                   ::valueset
                   ::generator]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Object Type derivation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- rules->prefix-rule-m
  "Construct a map from path prefixes to rules."
  [parsed-rules]
  (->> parsed-rules
       (filter ; no excluded rules in the coll
        (fn [{:keys [presence]}]
          (not= :excluded presence)))
       (reduce ; group rules by specific path prefixes (e.g. ["object"])
        (fn [m* {:keys [path] :as parsed-rule}]
          (reduce (fn [m** prefix]
                    (update m** prefix (fnil conj []) parsed-rule))
                  m*
                  (xp/object-type-paths path)))
        {["object"]                        []
         ["object" "object"]               []
         ["actor"]                         []
         ["object" "actor"]                []
         ["context" "instructor"]          []
         ["object" "context" "instructor"] []
         ["authority"]                     []})))

(defn- rule-object-types
  "Return the valueset at the `prefix + \"objectType\"` path."
  [prefix parsed-rules]
  (let [object-type-path (conj prefix "objectType")]
    (some (fn [{:keys [path valueset]}]
            (when (= object-type-path path)
              (->> valueset
                   (map xp/object-type-kebab-case)
                   set
                   not-empty)))
          parsed-rules)))

(defn rules->object-types
  "Derive object types from `parsed-rules` and return a map from paths
   to the set of possible object types at that location (e.g. one
   possible key-value pair is `[\"actor\"] #{\"agent\" \"group\"}`)."
  [parsed-rules]
  (let [prefix-rule-m (rules->prefix-rule-m parsed-rules)]
    (reduce-kv
     (fn [m prefix rules]
       (let [?object-types (rule-object-types prefix parsed-rules)
             rule-paths    (map :path rules)
             object-types  (if ?object-types
                             (xp/path-object-type-set ?object-types
                                                   prefix
                                                   rule-paths)
                             (xp/path-object-type-set prefix
                                                   rule-paths))]
         (if (not-empty object-types)
           (assoc m prefix object-types)
           (throw (ex-info (format "Contradiction on path: $.%s"
                                   (cstr/join "." prefix))
                           {:type  ::invalid-object-types
                            :path  prefix
                            :rules rules})))))
     {}
     prefix-rule-m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Parse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- parse-json-path*
  [json-path-str]
  (path/parse-paths json-path-str {:strict? true}))

(def ^:private parse-json-path
  "Memoized version of `parse-json-path*`."
  (memo/lru parse-json-path* {} :lru/threshold 4096))

(defn- join-location-and-selector
  [location selector]
  (vec (for [loc location
             sel selector]
         (vec (concat loc sel)))))

(defn- throw-empty-valueset
  [valueset ?any ?all ?none] 
  (if (empty? valueset)
    (throw (ex-info "Intersection of all and any is empty"
                    (cond-> {:type ::invalid-rule-values}
                      ?any  (assoc :any ?any)
                      ?all  (assoc :all ?all)
                      :none (assoc :none ?none))))
    valueset))

(defn- rule-value-set
  ([?all ?none]
   (rule-value-set nil ?all ?none))
  ([?any ?all ?none]
   (cond
     (and ?any ?all ?none)
     (-> (cset/difference (cset/intersection ?any ?all) ?none)
         (throw-empty-valueset ?any ?all ?none))
     (and ?any ?none)
     (-> (cset/difference ?any ?none)
         (throw-empty-valueset ?any nil ?none))
     (and ?all ?none)
     (-> (cset/difference ?all ?none)
         (throw-empty-valueset nil ?all ?none))
     (and ?any ?all)
     (-> (cset/intersection ?any ?all)
         (throw-empty-valueset ?any ?all nil))
     ?all  ?all
     ?any  ?any
     :else nil)))

(defn- parse-rule
  "Parse the rule by doing the following:
   - Convert `location` and `selector` JSONPath strings into a single
     Pathetic-compatible `location` vector.
   - Keywordize `presence`.
   - Convert `any`, `all`, and `none` into sets.
   - Add a `valueset` that consists of the intersection of `any` and
     `all` minus `none`."
  [{:keys [location selector presence any all none]}]
  (let [paths (cond-> (parse-json-path location)
                selector
                (join-location-and-selector (parse-json-path selector)))
        ?any  (not-empty (set any))
        ?all  (not-empty (set all))
        ?none (not-empty (set none))]
    (cond-> {:location paths}
      presence       (assoc :presence (keyword presence))
      (or ?any ?all) (assoc :valueset (rule-value-set ?any ?all ?none))
      ?any           (assoc :any ?any)
      ?all           (assoc :all ?all)
      ?none          (assoc :none ?none))))

(defn- separate-rule
  "Given a single `parsed-rule`, return a vector of multiple parsed
   rules such that each resulting `location` does not have multiple
   `|`-conjoined paths or multiple string keys per element."
  [{:keys [location] :as parsed-rule}]
  (let [rules (map (fn [loc] (assoc parsed-rule :location [loc]))
                   location)]
    (loop [idx   0
           loc   (first location)
           rules rules]
      (if-some [loc-element (first loc)]
        (let [loc-rest (rest loc)]
          (cond
            (or (= '* loc-element)
                (= 1 (count loc-element))
                (every? int? loc-element))
            (recur (inc idx)
                   loc-rest
                   rules)
            (every? string? loc-element)
            (recur (inc idx)
                   loc-rest
                   (for [rule  rules
                         loc-e loc-element]
                     (assoc-in rule [:location 0 idx] [loc-e])))
            :else
            (throw (ex-info "Rule location cannot mix integer and string keys."
                            {:type ::invalid-rule-location
                             :rule parsed-rule}))))
        (vec rules)))))

(defn- add-rule-specpath
  "Given `parsed-rule`, apply a `path` that is the `location` but
   flattened into a single vector of string keys and wildcards.
   Turns any integer elements into wildcards. Assumes that the
   rules were separated using `separate-rule`."
  [{:keys [location] :as parsed-rule}]
  (->> location
       first
       (mapv (fn [loc-elements]
               (cond
                 (or (-> loc-elements #{'*})
                     (-> loc-elements first int?))
                 '*
                 :else
                 (first loc-elements))))
       (assoc parsed-rule :path)))

(defn- add-rule-spec
  [object-types {:keys [path] :as rule}]
  (let [spec (xp/path->spec ::xs/statement path object-types)]
    (assoc rule :spec spec)))

(defn- parse-rules*
  [init-obj-types rules]
  (let [parsed-rules (->> rules
                          (map parse-rule)
                          (mapcat separate-rule)
                          (map add-rule-specpath))
        object-types* (rules->object-types parsed-rules)
        object-types  (-> object-types*
                          (update ["object"] cset/intersection init-obj-types))]
    (mapv (partial add-rule-spec object-types) parsed-rules)))

(s/fdef parse-rules
  :args (s/cat :object-property (s/? #{:activity-type :statement-ref})
               :rules (s/coll-of ::rule/rule))
  :ret (s/coll-of ::parsed-rule))

(defn parse-rules
  "Parse a collection of `rules` and return a coll of parsed and
   separated rules with specs and generators applied. `object-property`
   may be provided if the containing Template contains an Object-related
   Determining property."
  ([rules]
   (parse-rules* (xp/default-object-type-m ["object"]) rules))
  ([object-property rules]
   (case object-property
     :activity-type (parse-rules* #{"activity"} rules)
     :statement-ref (parse-rules* #{"statement-ref"} rules)
     (parse-rules rules))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Extra Values/Gen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- spec->valueset
  [{:keys [verbs verb-ids activities activity-ids activity-types]}
   spec]
  (case spec
    ::xs/verb        verbs
    :verb/id         verb-ids
    ::xs/activity    activities
    :activity/id     activity-ids
    :definition/type activity-types
    nil))

;; TODO: Distinguish between activity, context, and result extensions
(defn- extension-spec
  [type-iri-map extension-id spec]
  (let [act-iri-map (get type-iri-map "ActivityExtension")
        ctx-iri-map (get type-iri-map "ContextExtension")
        res-iri-map (get type-iri-map "ResultExtension")
        ext->spec   #(some->> % :inlineSchema (jschema/schema->spec nil))]
    (or (some->> extension-id (get act-iri-map) ext->spec)
        (some->> extension-id (get ctx-iri-map) ext->spec)
        (some->> extension-id (get res-iri-map) ext->spec)
        spec)))

(defn- spec-generator [spec]
  (try (s/gen spec)
       (catch Exception e
         (throw (ex-info (cond-> "Unable to create generator for spec"
                           (keyword? spec) (str spec))
                         {:type ::generator-failure
                          :spec spec}
                         e)))))

(defn add-rule-valuegen
  "If `parsed-rule` does not already have a `valueset`, then either
   derive one from the profile cosmos (i.e. the `valuesets` arg), or
   add a `:spec` and `:generator` to generate random values. This will
   ensure that during rule application, the rule will always be able to
   come up with a value.
   
   Also revises any extension specs to those specified in `extension-map`."
  [extension-map
   valuesets
   {:keys [presence path valueset none spec] :as parsed-rule}]
  (if (= :excluded presence)
    parsed-rule
    (let [spec*    (if (= ::j/any spec) ; only extensions have this spec
                     (extension-spec extension-map (peek path) spec)
                     spec)
          ?all-set (not-empty (spec->valueset valuesets spec))]
      (cond-> (assoc parsed-rule :spec spec*)
        (and (not valueset)
             ?all-set)
        (assoc :valueset (rule-value-set ?all-set none))
        (and (not valueset)
             (not ?all-set))
        (assoc :generator (spec-generator spec*))))))

;; TODO: Move to common util namespace
(defn- profile->statement-verb
  [{:keys [id prefLabel]}]
  {"id"      id
   "display" (w/stringify-keys prefLabel)})

(defn add-rules-valuegen
  "Use information from `iri-map` and `activities` maps, to complete the
   `parsed-rules` by adding additional valuesets or spec generators."
  [type-iri-map activity-map parsed-rules]
  (let [iri-verb-map   (get type-iri-map "Verb")
        verbs          (->> iri-verb-map vals (map profile->statement-verb) set)
        verb-ids       (->> iri-verb-map keys set)
        activities     (->> activity-map vals (mapcat vals) set)
        activity-ids   (->> activity-map vals (mapcat keys) set)
        activity-types (->> activity-map keys set)
        value-sets     {:verbs          verbs
                        :verb-ids       verb-ids
                        :activities     activities
                        :activity-ids   activity-ids
                        :activity-types activity-types}]
    (mapv (partial add-rule-valuegen type-iri-map value-sets)
          parsed-rules)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Follow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Statement Template validation logic:
;; https://github.com/adlnet/xapi-profiles/blob/master/xapi-profiles-communication.md#21-statement-template-validation

(defn- not-empty?
  "Like `not-empty` but returns a boolean."
  [coll]
  (boolean (not-empty coll)))

(defn- follows-rule-values?
  [?any ?all ?none ?values]
  (and (or (not ?all)
           (and ?values (cset/superset? ?all (set ?values))))
       (or (not ?any)
           (not-empty? (cset/intersection (set ?values) ?any)))
       (or (not ?none)
           (empty? (cset/intersection (set ?values) ?none)))))

(s/fdef follows-rule?
  :args (s/cat :statement ::xs/statement
               :parsed-rule ::parsed-rule)
  :ret boolean?)

(defn follows-rule?
  "Simple predicate check to see if `parsed-rule` satisfies `statement`.
   
   Note that in this function, `recommended` presence acts exactly like
   `included` presence. This is more strict than what the xAPI Profile spec
   specifies (in which `recommended` rules do not require values to be
   present at the location), but this is so `recommended` rules
   can still be applied to the `statement`."
  [statement {:keys [location any all none presence] :as _parsed-rule}]
  (let [?values (not-empty (path/get-values* statement location))]
    (case presence
      :excluded    ; values must not be present
      (nil? ?values)
      :recommended ; values must be present, must always follow rule vals
      (and (some? ?values)
           (follows-rule-values? any all none ?values))
      :included    ; values must be present, must always follow rule vals
      (and (some? ?values)
           (follows-rule-values? any all none ?values))
      ; no presence, values can be missing but must follow rule vals if present
      (follows-rule-values? any all none ?values))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic Properties

(def max-enumerated-paths 10)

;; IDs and IFIs (including account properties).
;; Technically Agent/Group names don't HAVE to be distinct, but it would
;; be weird if names get repeated all the time.
(def distinct-value-properties
  #{"id" "mbox" "mbox_sha1sum" "openid" "account" "homePage" "name"})

;; Generators

(defn- generate-xapi
  [generator none rng gen-size]
  (let [?none-filter (when (not-empty none)
                       (partial (complement contains?) none))
        generator    (cond->> generator
                       ?none-filter (gen/such-that ?none-filter))
        generate-fn  #(gen/generate generator 30 (random/rand-long rng))]
    (try (vec (repeatedly gen-size generate-fn))
         (catch clojure.lang.ExceptionInfo exi
           (throw (ex-info "Generation error!"
                           {:type ::gen-error
                            :gen  generator}
                           exi))))))

;; Rule Application

(defn- distinct-values?
  "Does the rule `path` point to a property with distinct values like an
   ID or IFI?"
  [{:keys [path]}]
  (contains? distinct-value-properties (last path)))

;; TODO: ERROR when multiple `all` rules at same location contradict each other.
(defn- rule-value-coll
  "Turn `value-set` into the ordered collection of values to apply at the
   locations. If `distinct-vals?` is true (and there are enough locations
   to put them) than any values are present exactly once; otherwise repeat
   some values until there are enough for `num-locations`.
   
   Note that this approach has the limitation of not being able to add rules
   outside the `any` coll (and the rule doesn't also have `any`); however,
   this is something we can live with for our purposes."
  [value-set rng num-locations distinct-vals?]
  (if (and distinct-vals?
           (>= (count value-set) num-locations))
    ;; Distinct values (e.g. IDs)
    (vec (take num-locations (random/shuffle* rng value-set)))
    ;; Either values are not distinct or there are not enough distinct
    ;; values for every location (violating the Pigenhole Principle)
    (loop [n-values num-locations
           val-set  value-set
           val-coll []]
      (cond
        (zero? n-values)
        (vec (random/shuffle* rng val-coll))
        ;; n-values is nearly exhausted - choose one of each remaining value
        (<= n-values (count val-set))
        (let [x (first val-set)]
          (recur (dec n-values)
                 (disj val-set x)
                 (conj val-coll x)))
        ;; val-set is nearly exhausted - repeat last value to fill locations
        (= 1 (count val-set))
        (let [x (first val-set)]
          (recur 0
                 (disj val-set x)
                 (into val-coll (repeat n-values x))))
        ;; choose a value and repeat it between 0 (inclusive) and n-values (exclusive) times
        :else
        (let [x (first val-set)
              n (random/rand-int* rng n-values)]
          (recur (- n-values n)
                 (disj val-set x)
                 (into val-coll (repeat n x))))))))

;; Rule Application

(s/fdef apply-inclusion-rules
  :args (s/cat :statement    map?
               :parsed-rules (s/every ::parsed-rule)
               :rng          ::random/rng)
  :ret ::xs/statement)

(defn- apply-inclusion-rule
  [statement {:keys [location valueset all generator none] :as rule} rng]
  (let [distincts? (distinct-values? rule)
        enum-max   (if (and distincts? valueset)
                     (count valueset)
                     max-enumerated-paths)
        enum-limit (inc (random/rand-int* rng enum-max))
        opt-map    {:wildcard-append? (not (some? all)) ; any only = append
                    :wildcard-limit   enum-limit}
        paths      (path/speculate-paths* statement location opt-map)
        num-paths  (count paths)
        val-coll   (cond
                     valueset
                     (rule-value-coll valueset rng num-paths distincts?)
                     generator
                     (generate-xapi generator none rng num-paths)
                     :else
                     (throw (ex-info "Parsed rule lacks a valueset or generator"
                                     {:type ::missing-rule-valuegen
                                      :rule rule})))]
    ;; It's rather unoptimized to call pathetic.json-path/speculative-path-seqs
    ;; twice, but profiling shows that this doesn't actually matter.
    (path/apply-multi-value* statement location val-coll opt-map)))

(defn apply-inclusion-rules
  "Given a partial `statement` and `parsed-rules`, apply all rules that do
   not have `excluded` presence to make the statement satisfy
   those rules. Must provide an `rng` in order to randomly choose or
   generate values; these values are then assoc'd into the proper position
   in `statement` as determined by the respective rule location."
  [statement parsed-rules rng]
  (->> parsed-rules
       (filter
        (fn [{:keys [presence]}]
          (not= :excluded presence)))
       (reduce 
        (fn [statement rule]
          (if-not (follows-rule? statement rule)
            (apply-inclusion-rule statement rule rng)
            statement))
        statement)))

(s/fdef apply-exclusion-rules
  :args (s/cat :statement    map?
               :parsed-rules (s/every ::parsed-rule))
  :ret ::xs/statement)

(defn- apply-exclusion-rule
  [statement {:keys [location]}]
  (path/excise* statement location {:prune-empty? true}))

(defn apply-exclusion-rules
  "Given a partial `statement` and `parsed-rules`, apply all rules that do
   do have `excluded` presence to make the statement satisfy
   those rules. Excises values at the specified rule location."
  [statement parsed-rules]
  (->> parsed-rules
       (filter
        (fn [{:keys [presence]}]
          (= :excluded presence)))
       (reduce
        (fn [statement rule]
          (if-not (follows-rule? statement rule)
            (apply-exclusion-rule statement rule)
            statement))
        statement)))

(s/fdef apply-rules
  :args (s/cat :statement    map?
               :parsed-rules (s/every ::parsed-rule)
               :rng          ::random/rng)
  :ret ::xs/statement)

(defn apply-rules
  "Apply all `parsed-rules` to a partial `statement`. An `rng` must be
   provided in order to randomly choose or generate values."
  [statement parsed-rules rng]
  (-> statement
      (apply-inclusion-rules parsed-rules rng)
      (apply-exclusion-rules parsed-rules)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef property-rule?
  :args (s/cat :property string?
               :parsed-rule ::parsed-rule)
  :ret boolean?)

(defn property-rule?
  "Does the first key at the rule's `location` equal `property`?"
  [property {:keys [location]}]
  (not= [property] (ffirst location)))
