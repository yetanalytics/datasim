(ns com.yetanalytics.datasim.xapi.profile.template.rule
  "Apply statement template rules for generation"
  (:require [clojure.core.memoize          :as memo]
            [clojure.set                   :as cset]
            [clojure.spec.alpha            :as s]
            [clojure.test.check.generators :as gen]
            [xapi-schema.spec                    :as xs]
            [com.yetanalytics.pathetic           :as path]
            [com.yetanalytics.pathetic.json-path :as jpath]
            [com.yetanalytics.pan.objects.templates.rule :as rules]
            [com.yetanalytics.datasim.json      :as j]
            [com.yetanalytics.datasim.xapi.path :as xp]
            [com.yetanalytics.datasim.random    :as random]
            [com.yetanalytics.datasim.json.schema :as jschema]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::location ::jpath/paths)

(s/def ::selector ::jpath/paths)

(s/def ::any
  (s/every ::j/any
           :kind set?
           :into #{}))

(s/def ::all
  (s/every ::j/any
           :kind set?
           :into #{}))

(s/def ::none
  (s/every ::j/any
           :kind set?
           :into #{}))

(s/def ::presence
  #{:included :excluded :recommended})

(s/def ::parsed-rule
  (s/keys :req-un [::location]
          :opt-un [::any
                   ::all
                   ::none
                   ::selector
                   ::presence]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Parse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-rule*
  "Parse `:location` and `:selector` in `rule`, keywordize `:presence`, and
   turn `:any`, `:all`, and `:none` into sets."
  [rule]
  (reduce-kv
   (fn [m k v]
     (let [v* (condp #(contains? %1 %2) k
                #{:location :selector} (path/parse-paths v)
                #{:presence}           (keyword v)
                #{:any :all :none}     (set v)
                ;; custom key added for extension generation hint
                ;; -> addition to rule is strictly controlled,
                ;;    see `com.yetanalytics.datasim.xapi.extensions`
                #{:spec} v
                v)] ; ignore all other keys
       (assoc m k v*)))
   {}
   rule))

(s/fdef parse-rule
  :args (s/cat :rule ::rules/rule)
  :ret ::parsed-rule)

(def parse-rule
  "Memoized version of `parse-rule*`."
  (memo/lru parse-rule* {} :lru/threshold 4096))

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

;; Parsed Path Join

(defn- join-location-and-selector
  [location selector]
  (vec (for [loc location
             sel selector]
         (vec (concat loc sel)))))

;; Generators

(defn- xapi-generator
  [spec parsed-locations statement]
  (let [ex-info-msg "Couldn't figure out xapi path"
        ex-info-map {:type     ::undefined-path
                     :key-path (first parsed-locations)}]
    (s/gen
     (or spec ;; known to be `s/gen` safe
         (try (xp/path->spec
               ::xs/statement
               (first parsed-locations)
               statement)
              (catch java.lang.AssertionError ae
                (throw (ex-info ex-info-msg ex-info-map ae)))
              (catch clojure.lang.ExceptionInfo exi
                (throw (ex-info ex-info-msg ex-info-map exi))))))))

(defn- generate-xapi
  [spec rng enumerated-paths statement {:keys [none] :as rule}]
  (let [?none-filter (when (not-empty none)
                       (partial (complement contains?) none))
        generator    (cond->> (xapi-generator spec enumerated-paths statement)
                       ?none-filter (gen/such-that ?none-filter))
        generate-fn  #(gen/generate generator 30 (random/rand-long rng))
        gen-size     (count enumerated-paths)]
    (try (vec (repeatedly gen-size generate-fn))
         (catch clojure.lang.ExceptionInfo exi
           (throw (ex-info "Generation error!"
                           {:type ::gen-error
                            :gen  generator
                            :rule rule}
                           exi))))))

;; Rule Excision

(defn- excise-rule
  "Remove all values given by the rule `location` and `selector`."
  [statement {:keys [location selector] :as _parsed-rule}]
  (let [paths (cond-> location
                selector (join-location-and-selector selector))]
    (path/excise* statement paths {:prune-empty? true})))

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
(defn- rule-value-set
  "Return the set of values to choose from when applying the rule. The
   returned set must be a subset of `all` and exclude `none`; if `any` is
   present it will also be a subset of that (which is more restrictive
   than the spec but simplifies things).
   
   If no `any`, `all`, or `none` are provided, then return `nil`. If the
   ensuing set is empty, throw an exception."
  [{:keys [any all none] :as rule}]
  (let [?value-set
        (cond
          (and any all none)
          (cset/difference (cset/intersection any all) none)
          (and any none)
          (cset/difference any none)
          (and all none)
          (cset/difference all none)
          (and any all)
          (cset/intersection any all)
          all   all
          any   any
          :else nil)]
    (if (or (nil? ?value-set)
            (not-empty ?value-set))
      ?value-set
      (throw (ex-info "Cannot generate non-empty rule value set from rule!"
                      {:type ::invalid-rule-values
                       :rule rule})))))

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

;; `spec` only in `rule` if previously shown to be `s/gen` safe and more accurate than `::j/any`
(defn- apply-rule
  "Put the values given by `rule` at the specified `location` and `selector`."
  [statement {:keys [location selector spec] :as rule} rng]
  (let [?val-set   (rule-value-set rule)
        paths*     (cond-> location
                     selector (join-location-and-selector selector))
        distincts? (distinct-values? paths*)
        enum-max   (if (and distincts? ?val-set)
                     (count ?val-set)
                     max-enumerated-paths)
        enum-limit (inc (random/rand-int* rng enum-max))
        opt-map    {:multi-value?     true
                    :wildcard-append? false
                    :wildcard-limit   enum-limit}
        paths      (path/speculate-paths* statement paths* opt-map)
        num-paths  (count paths)
        val-coll   (if ?val-set
                     (rule-value-coll ?val-set rng num-paths distincts?)
                     (generate-xapi spec rng paths statement rule))]
    ;; It's rather unoptimized to call pathetic.json-path/speculative-path-seqs
    ;; twice, but profiling shows that this doesn't actually matter.
    (path/apply-value* statement paths* val-coll opt-map)))

;; Putting it all together

(s/fdef apply-rules-gen
  :args (s/cat :partial-statement ::xs/statement
               :rules (s/every ::rules/rule)
               :options (s/keys* :req-un [::random/seed]))
  :ret ::xs/statement)

;; TODO: We ensure that the rules pass, but we do not ensure that intermediate
;; parts of the statement are valid!
(defn apply-rules-gen
  "Given a partial statement and rules, attempt to make the statement satisfy
   the rules. Additional options like `:seed` help do this deterministically."
  [partial-statement rules & {:keys [seed]}]
  (let [rng   (random/seed-rng seed)
        rules (map parse-rule rules)]
    (reduce
     (fn [statement {:keys [presence] :as rule}]
       (cond
         ;; If the statement follows the rule, continue processing!
         (follows-rule? statement rule)
         statement
         ;; The simplest case is an exclusion rule...
         (= :excluded presence)
         (excise-rule statement rule)
         ;; Otherwise, we need to apply rule values.
         :else
         (apply-rule statement rule rng)))
     partial-statement
     rules)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NEW RULE FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- rule-value-set-2
  ([?all ?none]
   (rule-value-set-2 nil ?all ?none))
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

(defn parse-rule-2
  [{:keys [location selector presence any all none]}]
  (let [paths (cond-> (path/parse-paths location)
                selector
                (join-location-and-selector (path/parse-paths selector)))
        ?any  (not-empty (set any))
        ?all  (not-empty (set all))
        ?none (not-empty (set none))]
    (cond-> {:location paths}
      presence       (assoc :presence (keyword presence))
      (or ?any ?all) (assoc :valueset (rule-value-set-2 ?any ?all ?none))
      ?any           (assoc :any ?any)
      ?all           (assoc :all ?all)
      ?none          (assoc :none ?none))))

(defn separate-rule
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
                         loc-k loc-rest]
                     (assoc-in rule [:location 0 idx] loc-k)))
            :else
            (throw (ex-info "Rule location cannot mix integer and string keys."
                            {:type ::invalid-rule
                             :rule parsed-rule}))))
        (vec rules)))))

(def xs-ns "xapi-schema.spec")

(defn- array-element-spec-name
  [element]
  (condp contains? element
    #{"category" "grouping" "parent" "other"}
    "activity"
    #{"choices" "scale" "source" "target" "steps"}
    "interaction-component"
    #{"correctResponsesPattern"}
    "string"
    #{"member"}
    "agent"
    #{"attachments"}
    "attachment"
    ;; else
    (throw (ex-info "Unknown path elements"
                    {:type ::invalid-path-element
                     :element element}))))

(defn add-rule-specpath
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
       (assoc parsed-rule :specpath)))

(defn ->path-rule-map
  [parsed-rules]
  (reduce (fn [m {:keys [specpath] :as parsed-rule}]
            (assoc m specpath parsed-rule))
          {}
          parsed-rules))

(defn- follows-prefix?
  [prefix coll]
  (and (<= (count prefix) (count coll))
       (= prefix (subvec coll 0 (count prefix)))))

(defn- object-type [path-rule-map path-prefix]
  (let [object-types (->> (conj path-prefix "objectType")
                          (get path-rule-map)
                          :valueset)
        property-set (->> path-rule-map
                          keys
                          (filter #(follows-prefix? path-prefix %))
                          (map #(get % (count path-prefix)))
                          set)]
    (cond
      (< 1 (count object-types))
      (throw (ex-info "Multiple possible objectTypes not supported"
                      {:type ::multiple-object-types
                       :path (conj path-prefix "objectType")}))
      (= 1 (count object-types))
      (condp = object-types
        #{"Activity"} "activity"
        #{"Agent"}    "agent"
        #{"Group"}    "group"
        #{"StatementRef"} "statement-ref"
        #{"SubStatement"} "sub-statement"
        ;; else - default to activity
        "activity")
      :else
      (condp #(contains? %2 %1) property-set
        "member"       "group"
        "name"         "agent"
        "mbox"         "agent"
        "mbox_sha1sum" "agent"
        "openid"       "agent"
        "account"      "agent"
        "timestamp"    "sub-statement"
        "actor"        "sub-statement"
        "verb"         "sub-statement"
        "object"       "sub-statement"
        "context"      "sub-statement"
        "result"       "sub-statement"
        "attachments"  "sub-statement"
        ;; else - default to activity
        "activity"))))

(defn- actor-type [path-rule-map path-prefix]
  (let [object-types (->> (conj path-prefix "objectType")
                          (get path-rule-map)
                          :valueset)
        property-set (->> path-rule-map
                          keys
                          (filter #(follows-prefix? path-prefix %))
                          (map #(get % (count path-prefix)))
                          set)]
    (cond
      (< 1 (count object-types))
      (throw (ex-info "Multiple possible objectTypes not supported"
                      {:type ::multiple-object-types
                       :path (conj path-prefix "objectType")}))
      (= 1 (count object-types))
      (condp = object-types
        #{"Agent"} "agent"
        #{"Group"} "group"
        ;; else - default to agent
        "agent")
      :else
      (condp #(contains? %2 %1) property-set
        "member"       "group"
        "name"         "agent"
        "mbox"         "agent"
        "mbox_sha1sum" "agent"
        "openid"       "agent"
        "account"      "agent"
        ;; else - default to agent
        "agent"))))

(defn- rule-spec-name
  [path-rule-map path]
  (let [len (count path)
        x2  (when (< 0 len) (get path (- len 1)))
        x1  (when (< 1 len) (get path (- len 2)))
        x0  (when (< 2 len) (get path (- len 3)))]
    (cond
      ;; $.object.object => :sub-statement/activity
      (and (= "object" x1)
           (= "object" x2))
      (keyword "sub-statement"
               (object-type path-rule-map ["object" "object"]))
      ;; $.object => :statement/activity
      (and (nil? x1)
           (= "object" x2))
      (keyword "statement"
               (object-type path-rule-map ["object"]))
      ;; $.actor => ::xs/agent
      (or (= "actor" x2)
          (= "authority" x2))
      (keyword xs-ns (actor-type path-rule-map path))
      ;; $.object.id => :activity/id
      (= "object" x1)
      (keyword (object-type path-rule-map path) x2)
      ;; $.actor.name => :agent/name
      (= "actor" x1)
      (keyword (actor-type path-rule-map path) x2)
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
      (keyword (array-element-spec-name x0) x2)
      :else
      (throw (ex-info (format "Unsupported key-index combination in path: %s" path)
                      {:type ::invalid-path
                       :path path})))))

(comment
  (rule-spec-name {} ["result" "score"]))

(defn add-rule-specname
  [path-rule-map {:keys [specpath] :as parsed-rule}]
  (->> (rule-spec-name path-rule-map specpath)
       (assoc parsed-rule :specname)))

(defn- rule-generator
  [iri-map specname]
  (try
    (cond
      (= "extensions" (namespace specname))
      (or (some->> (name specname)
                   (get iri-map)
                   :inlineSchema
                   (jschema/schema->spec nil)
                   s/gen)
          (s/gen any?))
      (s/get-spec specname)
      (s/gen specname)
      :else
      (s/gen any?))
    (catch Exception e
      (ex-info (format "Unable to create generator for: %s" specname)
               {:type ::generator-failure
                :spec specname}
               e))))

(defn add-rule-valuegen
  [iri-map
   {:keys [verbs verb-ids activities activity-ids activity-types]}
   {:keys [specname valueset none] :as parsed-rule}]
  (let [?all-set (case specname
                   :statement/verb verbs
                   :sub-statement/verb verbs
                   :verb/id verb-ids
                   :statement-object/activity activities
                   :sub-statement-object/activity activities
                   :activity/id activity-ids
                   :definition/type activity-types
                   nil)] 
    (cond-> parsed-rule
      (and (not valueset) ?all-set)
      (assoc :all      ?all-set
             :valueset (rule-value-set-2 ?all-set none))
      (and (not valueset) (not ?all-set))
      (assoc :generator (rule-generator iri-map specname)))))

(defn property-rule?
  [property {:keys [location]}]
  (not= [property] (ffirst location)))

(defn- generate-xapi-2
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

(defn- apply-inclusion-rule
  [statement {:keys [location valueset generator none]} rng]
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
        val-coll   (if valueset
                     (rule-value-coll valueset rng num-paths distincts?)
                     (generate-xapi-2 generator none rng num-paths))]
    ;; It's rather unoptimized to call pathetic.json-path/speculative-path-seqs
    ;; twice, but profiling shows that this doesn't actually matter.
    (path/apply-value* statement location val-coll opt-map)))

(defn apply-inclusion-rules
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

(defn- apply-exclusion-rule
  [statement {:keys [location]}]
  (path/excise* statement location {:prune-empty? true}))

(defn apply-exclusion-rules
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
