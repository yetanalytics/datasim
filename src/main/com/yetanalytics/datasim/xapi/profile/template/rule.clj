(ns com.yetanalytics.datasim.xapi.profile.template.rule
  "Apply statement template rules for generation"
  (:require [clojure.core.memoize :as memo]
            [clojure.set :as cset]
            [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.pan.objects.templates.rule :as pan-rules]
            [com.yetanalytics.datasim.json :as j]
            [com.yetanalytics.datasim.json.path :as json-path]
            [com.yetanalytics.datasim.xapi.path :as xp]
            [com.yetanalytics.datasim.random :as random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::location
  ::json-path/json-path)

(s/def ::selector
  ::json-path/json-path)

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
  (s/keys :req-un [::location]
          :opt-un [::selector
                   ::any
                   ::all
                   ::none
                   ::pan-rules/presence]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Parse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef parse-rule
  :args (s/cat :rule ::pan-rules/rule)
  :ret ::parsed-rule)

(defn parse-rule*
  "Parse the `location` and `selector` paths in `rule` and set-ify `any`,
   `all`, and `none` values, so that the rule can be used by other functions
   in this namespace."
  [{:keys [location selector] :as rule}]
  (cond-> (reduce-kv
           (fn [m k v]
             ;; Custom `:spec` key added for extension generation hint.
             ;; Addition to rule is strictly controlled,
             ;; see `com.yetanalytics.datasim.xapi.extensions`.
             (if (#{:presence :spec} k)
               (assoc m k v)
               (assoc m k (set v))))
           {}
           (select-keys rule [:any :all :none :presence :spec]))
    ;; location is always present so it's always re-assoc'd
    location (assoc :location (into [] (json-path/parse location)))
    selector (assoc :selector (into [] (json-path/parse selector)))))

;; TODO: Memoize in scope
(def parse-rule
  (memo/lru parse-rule* {} :lru/threshold 4096))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Match
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef match-rule
  :args (s/cat :statement ::xs/statement
               :rule ::parsed-rule)
  :ret (s/every (s/or :json ::j/any
                      :unmatchable #{::unmatchable})))

;; matching logic from:
;; https://github.com/adlnet/xapi-profiles/blob/master/xapi-profiles-communication.md#21-statement-template-validation
(defn match-rule
  "Return a vector of matchable values. If a selector cannot match any values
   it returns the `::unmatchable` keyword in its place."
  [statement {:keys [location selector]}]
  (vec (cond->> (json-path/select statement location)
         selector
         (mapcat (fn [lv]
                   (let [selection (json-path/select lv selector)]
                     (if (empty? selection)
                       [::unmatchable]
                       selection)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Follow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef follows-rule?
  :args (s/cat :statement ::xs/statement
               :rule ::parsed-rule
               :matches (s/? (s/every (s/or :json ::j/any
                                            :unmatchable #{::unmatchable}))))
  :ret boolean?)

(defn- follows-rule-presence?
  [presence values]
  (case presence
    "included" ; no unmatchable values
    (and (boolean (not-empty values))
         (every? (comp not #{::unmatchable}) values))
    "excluded" ; only unmatchable values
    (or (empty? values)
        (boolean (some #{::unmatchable} values)))
    ;; recommended or no presence
    true))

(comment
  [[1]])

(defn- follows-rule-values?
  [presence any all none values]
  (let [strict? (-> presence (= "recommended") not)
        ignore? (-> presence (= "excluded"))]
    (or ignore?
        (and
         (if (and any
                  (or strict? (not-empty values)))
           (boolean (not-empty (cset/intersection (set values) any)))
           true)
         (if (and all
                  (or strict? (not-empty values)))
           (let [values-set* (if (and (coll? values)
                                      (coll? (first values))
                                      (= 1 (count values)))
                           ;; first and only coll in a coll of colls
                               (first values)
                               values)
                 values-set
                 (cond
                   ;; most cases, guard for map to prevent conversion to keypairs
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
                                          {:type        ::rule-check-error
                                           :values      values
                                           :values-set* values-set*}
                                          e))))
                   :else #{})
                 ;; FIXME: THis is the correct way to do such things
                 #_values-set #_(set values)]
             (not (or
               ;; Unmatchable values?
                   (some #{::unmatchable} values)
                   (empty? values)
               ;; see `match-all-logic-test` bellow for logic proof
                   (not (if (empty? (cset/difference all values-set))
                          (cset/superset? all values-set)
                          false)))))
           true)
         (if (and none
                  (or strict? (not-empty values)))
           (not (some (partial contains? none)
                      values))
           true)))))

(defn follows-rule?
  "Does `statement `follow `rule`? `matches` can be provided to avoid
   re-finding the matched values."
  [statement {:keys [any all none presence] :as rule} & [matches]]
  (let [values (or matches (match-rule statement rule))]
    (and (follows-rule-presence? presence values)
         (follows-rule-values? presence any all none values))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Apply
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef apply-rules-gen
  :args (s/cat :partial-statement ::xs/statement
               :raw-rules (s/every ::pan-rules/rule)
               :options (s/keys* :req-un [::random/seed]))
  :ret ::xs/statement)

(defn- valid-value?
  [{:keys [all none]} v]
  (and (if all
         (contains? all v)
         true)
       (if none
         (not (contains? none v))
         true)))

(defn- excise-rule
  [statement {:keys [location selector]}]
  (cond-> (json-path/excise statement location :prune-empty? true)
    selector
    (json-path/excise (into location selector) :prune-empty? true)))

(defn- xapi-generator
  [statement {:keys [none spec] :as rule} location-enum]
  (cond->> (s/gen
            (or spec ;; known to be `s/gen` safe
                (try (xp/path->spec
                      ::xs/statement
                      (first location-enum)
                      statement)
                     (catch java.lang.AssertionError ae
                       (throw (ex-info "Couldn't figure out xapi path"
                                       {:type      ::undefined-path
                                        :key-path  (first location-enum)
                                        :rule      rule
                                        :statement statement}
                                       ae))))))
    (not-empty none)
    (gen/such-that (partial (complement contains?)
                            none))))

(defn- generate-xapi
  [statement rule location-enum rng]
  (try (gen/generate (xapi-generator statement rule location-enum)
                     30
                     (random/rand-long rng))
       (catch clojure.lang.ExceptionInfo exi
         (throw (ex-info "Generation error!"
                         {:type      ::gen-error
                          :rule      rule
                          :statement statement}
                         exi)))))

;; START EXPERIMENTS
(comment
  {:any [1 2 3]
   :all [1 2]}
  ;; => 1 2
  {:any [1 2]
   :all [1 2 3]}
  ;; => 1 2 OR 1 3 OR 2 3
  {:any [1 2]
   :all [3 4]}
  ;; => No choices
  
  (def any #{1 2 3 4 5})
  (def all #{1 2 3 4 6})
  (def none #{4})

  (->> any
       (filter all)
       (filter (comp not none))))

(defn- combo-with-replacement
  "Select a vector of `n` items from `value-set` with replacement. Each
   particular combination has a `1 / (count value-set)^n` probability of being
   returned."
  [rng value-set n]
  (vec (repeatedly n #(random/choose rng {} value-set))))

(defn- combo-without-replacement
  "Select `n` items from `value-set` without replacement. Each particular
   combination has a `1 / (choose (count value-set) n)` probability of
   being returned."
  [rng value-set n]
  (loop [vset  value-set
         combo (transient [])
         idx   0]
    (if (and (< idx n)
             (not-empty vset))
      (let [x (random/choose rng {} vset)]
        (recur (disj vset x) (conj! combo x) (inc idx)))
      (persistent! combo))))

;; See `clojure.math.combinatorics/index-combinations`
(defn- combinations
  "Return an infinite lazy seq of combinations of `n` values from `value-set`,
   whose entries are randomly selected. `replace?` determines whether the
   values are selected with or without replacement; by default they are
   selected with replacement."
  ([rng value-set n]
   (combinations rng value-set n true))
  ([rng value-set n replace?]
   (let [combo-fn      (if replace?
                         #(combo-with-replacement rng value-set n)
                         #(combo-without-replacement rng value-set n))
         combinations* (fn combinations* [combo-fn]
                         (lazy-seq
                          (cons (combo-fn)
                                (combinations* combo-fn))))]
     (lazy-seq
      (combinations* combo-fn)))))

(defn- rule-values
  "Return a coll of `n` values specified by `any`, `all`, and `none`."
  [{:keys [any all none]} n rng gen-xapi replace?]
  (cond
    ;; Guards
    (and all any (empty? (cset/intersection any all)))
    (throw (ex-info "No `all` values exist in `any` - generation impossible."
                    {:type ::gen-error
                     :any  any
                     :all  all}))
    (and all none (= all none))
    (throw (ex-info "Every `all` value exists in `none` - generation impossible."
                    {:type ::gen-error
                     :all  all
                     :none none}))
    (and any none (= any none))
    (throw (ex-info "Every `any` value exists in `none` - generation impossible"
                    {:type ::gen-error
                     :any  any
                     :none none}))
    ;; Generation
    all
    (first (cond->> (combinations rng all n replace?)
             true (map set)
             any  (filter (fn [aset] (not-empty (cset/intersection any aset))))
             none (filter (fn [aset] (empty? (cset/intersection none aset))))))
    any   ; treat `any` like `all`
    (first (cond->> (combinations rng any n replace?)
             true (map set)
             none (filter (fn [aset] (empty? (cset/intersection none aset))))))
    :else ; `gen-xapi` already incorporates `none` filter
    (set (repeatedly n gen-xapi))))

(comment
  (def the-rng (random/seed-rng 100)) 
   
  (take 10 (combinations the-rng
                         (set (range 1000))
                         3
                         false))

  (rule-values {:any  #{} ; #_{1 2 3 4 5}
                :all  (set (range 100))
                :none #{4}}
               3
               the-rng
               (fn [] (random/rand-int* the-rng 10))
               true)

  (rule-values {:none #{11 12}}
               3
               the-rng
               (fn [] (random/rand-int* the-rng 10))
               false)
  )

(defn- rule-values-2
  [{:keys [any all none]} extant n rng gen-fn]
  (let [choose   (partial random/choose rng {})
        ext-set  (cond-> (set extant)
                   all  (cset/intersection all)
                   none (cset/difference none))
        ?all     (some-> all (cset/difference ext-set))
        ?any     (some-> any (cset/difference ext-set))
        ?any-all (and any all (cset/intersection ?any ?all))]
    (loop [extant   extant
           new      []
           any-sat? (nil? ?any) ; ignore if `any` is not present
           idx      0]
      (cond
        (>= idx n)
        new
        (contains? ext-set (first extant)) ; skip if value already follows rule
        (recur (rest extant)
               (conj new (first extant))
               any-sat?
               (inc idx))
        (not any-sat?) ; add at least one value from `any`
        (recur (rest extant)
               (conj new (choose (or ?any-all ?any)))
               (boolean true) ; avoid weird "recur arg for primitive local" err
               (inc idx))
        ?all
        (recur (rest extant)
               (conj new (choose ?all))
               any-sat?
               (inc idx))
        :else
        (recur (rest extant)
               (conj new (gen-fn))
               any-sat?
               (inc idx))))))

(defn- rule-values-3
  [{:keys [any all none]} extant n rng gen-fn replace?]
  (let [n         (max n (count extant))
        select-fn (if replace?
                    random/select-replace
                    random/select-no-replace)
        ext-set*  (cond-> (set extant)
                    all  (cset/intersection all)
                    none (cset/difference none)
                    true (disj nil))
        extant*   (->> extant (filter ext-set*))
        ext-set   (cond-> ext-set*
                    ;; If none of the pre-existing values are found in `any`,
                    ;; remove a random one so we can insert an `any` value.
                    (and (= n (count extant*))
                         (empty? (cset/intersection ext-set* any)))
                    (disj (random/choose rng {} ext-set*)))
        ?all      (and all (cset/difference all ext-set))
        ?any      (and any (cset/difference any ext-set))
        ?any-all  (and any all (cset/intersection ?any ?all))
        ext-count (count extant*)
        any-count (if (or ?any-all ?any)
                    (->> (- n ext-count) dec (random/rand-int* rng) inc)
                    0)
        any-pool  (when-some [coll (or ?any-all ?any)]
                    (select-fn rng coll any-count))
        all-pool  (when-some [coll (or ?all (set (repeatedly n gen-fn)))]
                    (select-fn rng coll (- n ext-count any-count)))
        val-pool  (random/shuffle* rng (concat any-pool all-pool))]
    (loop [extant extant
           pool   val-pool
           new    []]
      (cond
        ;; skip if value already follows rule
        (and (not-empty extant)
             (contains? ext-set (first extant)))
        (recur (rest extant)
               pool
               (conj new (first extant)))
        ;; otherwise select from the pool of values
        (not-empty pool)
        (recur (rest extant)
               (rest pool)
               (conj new (first pool)))
        :else
        new))))

(comment
  (def the-rng (random/seed-rng 100))

  (random/rand-int* the-rng -1)

  {:any #{:extant-value-a :value-1}
   :all #{:extant-value-a :value-2}
   :none #{:extant-value-b}}

  [:extant-value-a :extant-value-b] ; start
  [:extant-value-a] ; intersect w/ all + difference w/ none
  [:extant-value-a :value-1] ; add items chosen from (any + all - none) - extant
  [:extant-value-a :value-1 :value-2] ; add items chosen from (all - none) - extant

  (rule-values-3 {:any #{:value-1 :value-3}
                  :all #{:extant-value-a :value-1 :value-2 :value-3}
                  :none #{:extant-value-b}}
                 [#_:extant-value-b :extant-value-a]
                 1
                 the-rng
                 (fn [] "foobar")
                 true)
  
  ;; n = 2
  {:any #{:value-1 :value-2}}
  [:extant-value-a :extant-value-b]
  )
;; END EXPERIMENTS

;; `spec` only in `rule` if previously shown to be `s/gen` safe and more accurate than `::j/any`
(defn- apply-rule
  [statement {:keys [location any all] :as rule} rng matches]
  (let [;; In certain situations, we should attempt to make the
        ;; values distinct. This is pretty open ended, but generally
        ;; if the path points at an ID this is sane to do...
        distinct-vals? (= #{"id"} (last location))
        discrete-locs? (json-path/discrete? location)
        ?any-all-coll  (not-empty (concat any all))
        loc-enum-limit (max
                        ;; gotta be at least 1
                        1
                        ;; gotta be at least as many matched values, if we
                        ;; need to override them
                        (count (remove (partial = ::unmatchable) matches))
                        ;; or as many as all provided any or all values
                        (count ?any-all-coll)
                        ;; or maybe up to N
                        #_(if distinct-vals? ;; for distinct, use what we have
                            1
                            (random/rand-int* rng 10)))
        location-enum (json-path/enumerate location :limit loc-enum-limit)
        ;; spec generation is expensive, let's wrap it up for control
        generate-xapi (partial generate-xapi statement rule location-enum rng)
        values
        (if (and discrete-locs?
                 (= 1 (count location-enum))) ;; a single loc that must conform
          (if (not-empty all)
            (into [] all)
            [(if (not-empty any)
               (random/choose rng {} any)
               (generate-xapi))])
          #_[(cond (not-empty any)
                   (random/choose rng {} any)
                   (not-empty all)
                   (random/choose rng {} all)
                   :else (gen-xapi!))]
          ;; multiple discrete locs or infinite locs
          (loop [loc-enum location-enum
                 values   []
                 any-sat? (empty? any)]
            (if-let [path (first loc-enum)]
              (let [?match (get-in statement path)
                    value  (cond
                             (and (some? ?match)
                                  (valid-value? rule ?match))
                             ?match
                             ?any-all-coll
                             ;; try to use each provided val once
                             (if-let [any-all-remaining (not-empty (remove
                                                                    (partial contains? values)
                                                                    ?any-all-coll))]
                               (random/choose rng {} any-all-remaining)
                               ;; but it is better to repeat then gen
                               ;; unless this should be distinct...
                               (if distinct-vals?
                                 (generate-xapi)
                                 (random/choose rng {} ?any-all-coll)))
                             :else
                             (generate-xapi))]
                (recur (rest loc-enum)
                       (conj values value)
                       (or any-sat? (contains? any value))))
              ;; handle possible lack of any...
              (if any-sat?
                values
                ;; if there's no any, just swap one
                (let [swap-idx (random/rand-int* rng (count values))]
                  (assoc values swap-idx (random/choose rng {} any)))))))]
        ;; TODO: remove unmatchable paths
        (json-path/apply-values statement
                                location
                                values
                                :enum-limit 3)))

;; TODO: We ensure that the rules pass, but we do not ensure that intermediate
;; parts of the statement are valid!
(defn apply-rules-gen
  "Given a partial statement and rules, attempt to make the statement satisfy
  the rules. Additional options like :seed help do this deterministically.
  "
  [partial-statement raw-rules & {:keys [seed]}]
  (let [rng (random/seed-rng seed)]
    (loop [statement partial-statement
           rules     (map parse-rule raw-rules)
           paths     {:applied #{}
                      :excised #{}}]
      (if-let [{:keys [presence] :as rule} (first rules)]
        (let [matches (match-rule statement rule)]
          (if (follows-rule? statement rule matches)
            ;; if the statement follows the rule, continue processing!
            (recur statement (rest rules) paths)
            ;; if not, we've got work to do..
            (if (= presence "excluded")
              ;; the simplest case is an exclusion rule, which we can safely
              ;; apply without additional recursion
              (let [statement* (excise-rule statement rule)
                    new-paths  (some-> statement* meta :paths)
                    paths*     (update paths :excised into new-paths)]
                (recur statement* (rest rules) paths*))
              (let [statement* (apply-rule statement rule rng matches)
                    new-paths  (some-> statement* meta :paths)
                    paths*     (update paths :applied into new-paths)]
                (recur statement* (rest rules) paths*)))))
        ;; all rules pass and we're done!
        statement
        ;; check the specs (dev/debug)
        #_(if (s/valid? ::xs/statement statement)
          statement
          (throw (ex-info "Healing not yet implemented"
                          {:type ::not-yet-implemented
                           :statement-error (s/explain-data ::xs/statement statement)})))))))
