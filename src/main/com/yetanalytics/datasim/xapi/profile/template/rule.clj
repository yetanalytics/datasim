(ns com.yetanalytics.datasim.xapi.profile.template.rule
  "Apply statement template rules for generation"
  (:require [clojure.spec.alpha :as s]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.pathetic :as path]
            [com.yetanalytics.pan.objects.templates.rule :as rules]
            [com.yetanalytics.datasim.json :as j]
            [com.yetanalytics.datasim.json.path :as json-path]
            [com.yetanalytics.datasim.xapi.path :as xp]
            [com.yetanalytics.datasim.random :as random]
            [clojure.set :as cset]
            [clojure.test.check.generators :as gen]
            [clojure.core.memoize :as memo]
            [clojure.math.combinatorics :as combo]))

(s/def ::location
  :com.yetanalytics.pathetic.json-path/paths)

(s/def ::selector
  :com.yetanalytics.pathetic.json-path/paths)

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

(s/def ::parsed-rule
  (s/keys :req-un [::location
                   ]
          :opt-un [::any
                   ::all
                   ::none
                   ::selector
                   ::rules/presence]))

(s/fdef parse-rule
  :args (s/cat :rule ::rules/rule)
  :ret ::parsed-rule)

(defn parse-rule*
  "Parse paths in a rule"
  [{:keys [location selector] :as rule}]
  (cond-> (assoc
           (reduce-kv
            (fn [m k v]
              (if (or (= :presence k)
                      ;; custom key added for extension generation hint
                      ;; -> addition to rule is strictly controlled, see `com.yetanalytics.datasim.xapi.extensions`
                      (= :spec k))
                (assoc m k v)
                (assoc m k (set v))))
            {}
            (select-keys rule [:any :all :none :presence :spec]))
           :location (into []
                           (path/parse-paths location)))
    selector (assoc :selector
                    (into [] (path/parse-paths selector)))))

;; TODO: Memoize in scope
(def parse-rule
  (memo/lru parse-rule* {} :lru/threshold 4096))

(s/fdef match-rule
  :args (s/cat :statement ::xs/statement
               :rule ::parsed-rule)
  :ret (s/every (s/or :json ::j/any
                      :unmatchable #{::unmatchable})))

(defn match-rule
  "The matching logic from https://github.com/adlnet/xapi-profiles/blob/master/xapi-profiles-communication.md#21-statement-template-validation
  returns a tuple, a list of matched values from location, selector, containing the key ::unmatchable if a selector cannot be matched."
  [statement
   {:keys [location selector] :as rule}]
  (let [loc-values (path/get-values* statement location)]
    (into loc-values
          (when selector
            (mapcat
             (fn [lv]
               (let [selection (path/get-values* lv selector)]
                 (if (empty? selection)
                   [::unmatchable]
                   selection)))
             loc-values)))))

(s/fdef follows-rule?
  :args (s/cat :statement ::xs/statement
               :rule ::parsed-rule
               :matches (s/? (s/every (s/or :json ::j/any
                                            :unmatchable #{::unmatchable}))))
  :ret boolean?)

(defn follows-rule?
  "simple predicate check for a rule being satisfied by a statement
  a la https://github.com/adlnet/xapi-profiles/blob/master/xapi-profiles-communication.md#21-statement-template-validation.
  You can pass in matches for efficiency's sake."
  [statement
   {:keys                                                 [location selector
                                                           any all none presence] :as rule}
   & [matches]]
  (let [strict (if (= presence "recommended")
                 false
                 true)
        values (or matches (match-rule statement rule))]
    (and (if presence
           (case presence
             "included"
             (if (or (empty? values)
                     (contains? values ::unmatchable))
               false
               true)
             "excluded"
             (if (and (not-empty values)
                      (not-empty (remove #{::unmatchable} values)))
               false
               true)
             "recommended" true)
           true)
         (if (= presence "excluded")
           true ;; ignore
           (and
            (if (and any
                     (or strict (not-empty values)))
              (not-empty (cset/intersection (set values) any))
              true)
            (if (and all
                     (or strict (not-empty values)))
              (let [values-set* (if (and (coll? values)
                                         (coll? (first values))
                                         (= 1 (count values)))
                                  ;; first and only coll in a coll of colls
                                  (first values)
                                  values)
                    values-set (cond
                                 ;; most cases, gaurd for map to prevent conversion to keypairs
                                 (and (coll? values-set*) (not (map? values-set*)))
                                 (into #{} values-set*)
                                 ;; if `all` specified an object for the location, prevent conversion to keypairs
                                 (map? values-set*)
                                 #{values-set*}
                                 ;; attempt conversion to set, throw on error
                                 (some? values-set*)
                                 (try (set values-set*)
                                      (catch Exception e
                                        (throw (ex-info "Unexpected State!"
                                                        {:type            ::rule-check-error
                                                         :rule            rule
                                                         :statement       statement
                                                         :matched         matches
                                                         :values          values
                                                         :values-set*     values-set*}
                                                        e))))
                                 :else #{})]
                (not (or (contains? values ::unmatchable)
                         (empty? values)
                         (not
                          ;; see `match-all-logic-test` bellow for logic proof
                          (if (empty? (cset/difference all values-set))
                            (cset/superset? all values-set)
                            false)))))
              true)
            (if (and none
                     (or strict (not-empty values)))
              (not (some (partial contains? none)
                         values))
              true))))))

(comment
  ;; -> everything within `all-set` within `target-set`?
  ;;    -> no = failure = (not (not false)) = false
  ;;    -> yes = continue
  ;;       -> anything within `target-set` that's not in `all-set`?
  ;;          -> yes = failure = (not (not false)) = false
  ;;          -> no = success = (not (not true)) = true

  (defn match-all-logic-test
    "Checks `target` set against top and bottom bounds of `all-set`"
    [all-set target-set]
    ;; everything within `all-set` within `target-set`?
    (if (empty? (cset/difference all-set target-set))
      ;; anything within `target-set` that's not in `all-set`?
      (cset/superset? all-set target-set)
      false))

  (def all-set-fixture #{:a :b :c})

  (defn replicate-conditional
    "assuming non-empty matchables which doesn't contain `::unmatchable`"
    [target-set]
    (not (or false false (not (match-all-logic-test all-set-fixture target-set)))))

  (def test-set        #{:c :b :a :A})
  ;; ^ 1 more than `all-set-fixture`
  (def test-set-1      #{:a :b})
  ;; ^ 1 less than `all-set-fixture`
  (def test-set-2      #{:d :e :f})
  ;; ^ same number as `all-set-fixture` but different members
  (def test-set-3      #{:b :c :a})
  ;; ^ matches `all-set-fixture`

  (and
   (false? (replicate-conditional test-set))
   (false? (replicate-conditional test-set-1))
   (false? (replicate-conditional test-set-2))
   (true? (replicate-conditional test-set-3))))

(s/fdef apply-rules-gen
  :args (s/cat :partial-statement ::xs/statement
               :raw-rules (s/every ::rules/rule)
               :options (s/keys* :req-un [::random/seed]))
  :ret ::xs/statement)

(def max-enumerated-paths 10)

(defn- xapi-generator
  [spec parsed-locations statement]
  (s/gen
   (or spec ;; known to be `s/gen` safe
       (try (xp/path->spec
             ::xs/statement
             (first parsed-locations)
             statement)
            (catch java.lang.AssertionError ae
              (throw (ex-info "Couldn't figure out xapi path"
                              {:type     ::undefined-path
                               :key-path (first parsed-locations)}
                              ae)))))))

(defn- join-location-and-selector
  [location selector]
  (vec (for [loc location
             sel selector]
         (vec (concat loc sel)))))

(defn- generate-xapi
  [rng generator {:keys [none] :as rule}]
  (let [?none-filter (when (not-empty none)
                       (partial (complement contains?) none))
        generator*   (cond->> generator
                       ?none-filter (gen/such-that ?none-filter))]
    (try (gen/generate generator* 30 (random/rand-long rng))
         (catch clojure.lang.ExceptionInfo exi
           (throw (ex-info "Generation error!"
                           {:type ::gen-error
                            :gen  generator
                            :rule rule}
                           exi))))))

(defn- excise-rule
  [statement {:keys [location selector]}]
  (let [paths (cond-> location
                selector (join-location-and-selector selector))]
    (path/excise* statement paths {:prune-empty? true})))

(defn- validate-rule-values
  [rule rule-value-set]
  (if (not-empty rule-value-set)
    rule-value-set
    (throw (ex-info "Cannot generate non-empty rule value set from rule!"
                    {:kind ::invalid-rule-values
                     :rule rule}))))

(defn- apply-rule-values*
  "Return a set of values to apply to the Statement from the rule's `any`,
   `all`, and `none` values. Returned values must be part of `all` and not
   include `none`; if `any` is present, then the value space will be
   further restricted to values in `any` (which is stricter than required
   by the xAPI spec). Otherwise use `rng` and `generator` to generate a
   bunch of values."
  [rng generator {:keys [any all none] :as rule}]
  (validate-rule-values
   rule
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
     :else (->> #(generate-xapi rng generator rule)
                (repeatedly max-enumerated-paths)
                set))))

(defn- apply-rule-values
  "Return the collection of values to apply at a rule location, in order."
  [rng generator rule num-values]
  (loop [n-values num-values
         val-set  (apply-rule-values* rng generator rule)
         val-coll []]
    (cond
      (or (empty? val-set)
          (zero? n-values))
      (vec (random/shuffle* rng val-coll))
      ;; n-values is nearly exhausted
      (<= n-values (count val-set))
      (let [x (first val-set)]
        (recur (dec n-values)
               (disj val-set x)
               (conj val-coll x)))
      ;; val-set is nearly exhausted
      (= 1 (count val-set))
      (let [x (first val-set)]
        (recur 0
               (disj val-set x)
               (into val-coll (repeat n-values x))))
      :else
      (let [x (first val-set)
            n (random/rand-int* rng n-values)]
        (recur (- n-values n)
               (disj val-set x)
               (into val-coll (repeat n x)))))))

;; `spec` only in `rule` if previously shown to be `s/gen` safe and more accurate than `::j/any`
(defn- apply-rule
  [statement {:keys [location selector spec] :as rule} rng]
  (let [enum-limit (inc (random/rand-int* rng max-enumerated-paths)) ; [1, 10]
        opt-map    {:multi-value?     true
                    :wildcard-append? false
                    :wildcard-limit   enum-limit}
        paths*     (cond-> location
                     selector (join-location-and-selector selector))
        paths      (path/speculate-paths* statement paths* opt-map)
        generator  (xapi-generator spec paths statement)
        max-paths  (count paths)
        values     (apply-rule-values rng generator rule max-paths)]
    ;; It's rather unoptimized to call pathetic.json-path/speculative-path-seqs
    ;; twice, but profiling shows that this doesn't actually matter.
    (path/apply-value* statement paths* values opt-map)))

;; TODO: We ensure that the rules pass, but we do not ensure that intermediate
;; parts of the statement are valid!
(defn apply-rules-gen
  "Given a partial statement and rules, attempt to make the statement satisfy
  the rules. Additional options like :seed help do this deterministically."
  [partial-statement raw-rules & {:keys [seed]}]
  (let [rng (random/seed-rng seed)]
    (loop [statement partial-statement
           rules     (map parse-rule raw-rules)
           paths     {:applied #{}
                      :excised #{}}]
      (if-let [{:keys [presence] :as rule} (first rules)]
        (let [matches (match-rule statement rule)]
          (if (follows-rule? statement rule matches)
            ;; If the statement follows the rule, continue processing!
            (recur statement (rest rules) paths)
            ;; If not, we've got work to do...
            ;; The simplest case is an exclusion rule, which we can safely
            ;; apply without additional recursion.
            (if (= presence "excluded")
              (let [statement-excised (excise-rule statement rule)
                    statement-paths   (some-> statement-excised meta :paths)]
                (recur statement-excised
                       (rest rules)
                       (update paths :excised into statement-paths)))
              (let [statement-applied (apply-rule statement rule rng)
                    statement-paths   (some-> statement-applied meta :paths)]
                (recur statement-applied
                       (rest rules)
                       (update paths :applied into statement-paths))))))
        ;; Rules have been exhausted
        statement))))
