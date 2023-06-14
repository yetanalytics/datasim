(ns com.yetanalytics.datasim.xapi.profile.template.rule
  "Apply statement template rules for generation"
  (:require [clojure.set                   :as cset]
            [clojure.string                :as cstr]
            [clojure.spec.alpha            :as s]
            [clojure.test.check.generators :as gen]
            [xapi-schema.spec                    :as xs]
            [com.yetanalytics.pathetic           :as path]
            [com.yetanalytics.pathetic.json-path :as jpath]
            [com.yetanalytics.pan.objects.templates.rule :as rule]
            [com.yetanalytics.datasim.json      :as j]
            [com.yetanalytics.datasim.xapi.path :as xp]
            [com.yetanalytics.datasim.random    :as random]))

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
                   ::path]
          :opt-un [::presence
                   ::any
                   ::all
                   ::none
                   ;; Either a valueset or a generator will be required
                   ::valueset
                   ::spec
                   ::generator]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Parse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- join-location-and-selector
  [location selector]
  (vec (for [loc location
             sel selector]
         (vec (concat loc sel)))))

(defn- rule-value-set
  ([?all ?none]
   (rule-value-set nil ?all ?none))
  ([?any ?all ?none]
   (cond
     (and ?any ?all ?none)
     (cset/difference (cset/intersection ?any ?all) ?none)
     (and ?any ?none)
     (cset/difference ?any ?none)
     (and ?all ?none)
     (cset/difference ?all ?none)
     (and ?any ?all)
     (cset/intersection ?any ?all)
     ?all   ?all
     ?any   ?any
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
  (let [opts  {:strict? true}
        paths (cond-> (path/parse-paths location opts)
                selector
                (join-location-and-selector (path/parse-paths selector opts)))
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
                            {:type ::invalid-rule
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

(s/fdef parse-rules
  :args (s/cat :rules (s/coll-of ::rule/rule))
  :ret (s/coll-of ::parsed-rule))

(defn parse-rules
  "Parse a collection of `rules` and return a coll of parsed and
   separated rules."
  [rules]
  (->> rules
       (map parse-rule)
       (mapcat separate-rule)
       (mapv add-rule-specpath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Valuegen
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
                  (xp/spec-hinted-path path)))
        {})))

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
                             (xp/paths->spec-hints ?object-types
                                                   prefix
                                                   rule-paths)
                             (xp/paths->spec-hints prefix
                                                   rule-paths)) ]
         (if (not-empty object-types)
           (assoc m prefix object-types)
           (throw (ex-info (format "Contradiction on path: $.%s"
                                   (cstr/join "." prefix))
                           {:type  ::invalid-object-types
                            :path  prefix
                            :rules rules})))))
     {}
     prefix-rule-m)))

(defn- rule-generator
  [spec-hints {:keys [path]}]
  (let [spec (xp/path->spec-3 ::xs/statement path spec-hints)]
    (try {:spec spec
          :generator (s/gen spec)}
         (catch Exception e
           (throw
            (ex-info (cond-> "Unable to create generator for spec"
                       (keyword? spec) (str spec))
                     {:type ::generator-failure
                      :spec spec}
                     e))))))

(defn add-rule-valuegen
  "If `parsed-rule` does not already have a `valueset`, then either
   derive one from the profile cosmos (i.e. the `valuesets` arg), where
   an `:all` set containing all appropriate values is introduced, or
   add a `:spec` and `:generator`. This will ensure that during rule
   application, the rule will always be able to come up with a value."
  [iri-map
   object-types
   valuesets
   {:keys [presence path valueset none] :as parsed-rule}]
  (if (= :excluded presence)
    parsed-rule
    (let [?all-set (not-empty (xp/path->valueset object-types valuesets path))]
      (cond-> parsed-rule
        (and (not valueset) ?all-set)
        (assoc :all      ?all-set
               :valueset (rule-value-set ?all-set none))
        (and (not valueset) (not ?all-set))
        (merge (rule-generator {:iri-map      iri-map
                                :object-types object-types}
                               parsed-rule))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Follow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Statement Template validation logic:
;; https://github.com/adlnet/xapi-profiles/blob/master/xapi-profiles-communication.md#21-statement-template-validation

(defn- not-empty?
  "Like `not-empty` but returns a boolean."
  [coll]
  (boolean (not-empty coll)))

(defn- match-rule
  "Return the matched values given by `location` and `selector`, or an empty
   coll if no values can be matched."
  [statement {:keys [location selector] :as _parsed-rule}]
  (cond-> (path/get-values* statement location)
    selector
    (->> (mapcat #(path/get-values* % selector)) vec)))

(s/fdef follows-rule?
  :args (s/cat :statement ::xs/statement
               :parsed-rule ::parsed-rule)
  :ret boolean?)

(defn follows-rule?
  "Simple predicate check to see if `parsed-rule` satisfies `statement`.
   
   Note: normally if `presence` is `recommended`, then validation
   auto-passes, but otherwise we won't be able to apply rules in those
   cases, so we validate anyways (though we pass if no values are
   matchable)."
  [statement {:keys [any all none presence] :as parsed-rule}]
  (let [strict? (not= :recommended presence)
        ?values (not-empty (match-rule statement parsed-rule))]
    (and (case presence
           :included (some? ?values)
           :excluded (nil? ?values)
           true)
         (or (= :excluded presence)
             (and (or (not all)
                      (not (or strict? ?values))
                      (and ?values
                           (cset/superset? all (set ?values))))
                  (or (not any)
                      (not (or strict? ?values))
                      (not-empty? (cset/intersection (set ?values) any)))
                  (or (not none)
                      (not (or strict? ?values))
                      (empty? (cset/intersection (set ?values) none))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic Properties

(def max-enumerated-paths 10)

(def distinct-value-properties #{"id"})

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
  "Do some `parsed-paths` end at a distinct value property (e.g. `id`)?"
  [parsed-paths]
  (->> parsed-paths
       (map last)
       (filter coll?)
       (map set)
       (some #(not-empty (cset/intersection % distinct-value-properties)))
       boolean))

;; TODO: There are still some significant limits to this approach:
;; - There would be no way to add rules outside of the `any` coll, even if
;;   they are allowed by the spec.
;; - If there are multiple `any` rules at the same location, their values
;;   will overwrite each other instead of being appended to each other.
;; - Multiple `all` rules at the same location will contradict each other.

(defn- rule-value-coll
  "Turn `value-set` into the ordered collection of values to apply at the
   locations. If `distinct-vals?` is true (and there are enough locations
   to put them) than any values are present exactly once; otherwise repeat
   some values until there are enough for `num-locations`."
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
  [statement {:keys [location valueset generator none] :as rule} rng]
  (let [distincts? (distinct-values? location)
        enum-max   (if (and distincts? valueset)
                     (count valueset)
                     max-enumerated-paths)
        enum-limit (inc (random/rand-int* rng enum-max))
        opt-map    {:multi-value?     true
                    :wildcard-append? false
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
    (path/apply-value* statement location val-coll opt-map)))

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
