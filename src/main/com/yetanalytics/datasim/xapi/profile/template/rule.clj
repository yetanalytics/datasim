(ns com.yetanalytics.datasim.xapi.profile.template.rule
  "Apply statement template rules for generation"
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.pan.objects.templates.rule :as rules]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.json :as j]
            [com.yetanalytics.datasim.json.path :as json-path]
            [com.yetanalytics.datasim.xapi.path :as xp]
            [com.yetanalytics.datasim.random :as random]
            [clojure.set :as cset]
            [clojure.test.check.generators :as gen]
            [clojure.core.memoize :as memo]))

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
          :opt-un [::any
                   ::all
                   ::none
                   ::selector
                   ::rules/presence]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Rule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef parse-rule
  :args (s/cat :rule ::rules/rule)
  :ret ::parsed-rule)

(defn parse-rule*
  "Parse `location` and `selector` paths in a `rule`."
  [{:keys [location selector] :as rule}]
  (let [depathed-rule
        (reduce-kv
         (fn [m k v]
           (if (or (= :presence k)
                   ;; custom key added for extension generation hint
                   ;; -> addition to rule is strictly controlled,
                   ;; see `com.yetanalytics.datasim.xapi.extensions`
                   (= :spec k))
             (assoc m k v)
             (assoc m k (set v))))
         {}
         (select-keys rule [:any :all :none :presence :spec]))
        parsed-location
        (into [] (json-path/parse location))
        parsed-selector
        (when selector (into [] (json-path/parse selector)))]
    (cond-> depathed-rule
      true     (assoc :location parsed-location)
      selector (assoc :selector parsed-selector))))

;; TODO: Memoize in scope
(def parse-rule
  (memo/lru parse-rule* {} :lru/threshold 4096))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Match Rule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See: https://github.com/adlnet/xapi-profiles/blob/master/xapi-profiles-communication.md#21-statement-template-validation
;; for `match-rule` and `follows-rule`

(s/fdef match-rule
  :args (s/cat :statement ::xs/statement
               :rule ::parsed-rule)
  :ret (s/every (s/or :json ::j/any
                      :unmatchable #{::unmatchable})))

(defn match-rule
  "The matching logic from the xAPI Profile specification
   returns a tuple, a list of matched values from `location`, `selector`,
   containing the key `::unmatchable` if a selector cannot be matched."
  [statement
   {:keys [location selector] :as _rule}]
  (let [select-subloc (fn [loc-value]
                        (let [selection (json-path/select loc-value selector)]
                          (if (empty? selection)
                            [::unmatchable]
                            selection)))
        location-vals (json-path/select statement location)
        selector-vals (when selector
                        (mapcat select-subloc location-vals))]
    (into location-vals selector-vals)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Follows Rule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- follows-presence?
  "Do the statement `values` follow the rule `presence`?"
  [presence values]
  (condp = presence
    "included"
    (not (or (empty? values)
             (contains? values ::unmatchable)))
    "excluded"
    (not (and (not-empty values)
              (not-empty (remove #{::unmatchable} values))))
    "recommended"
    true
    ;; else
    true))

(defn- values->set*
  "Take the first item in `values` if it is the first and only coll in
   a collection of colls, return `values` otherwise."
  [values]
  (if (and (coll? values)
           (coll? (first values))
           (= 1 (count values)))
    (first values)
    values))

(defn- values->set
  "Coerce `values` into a set; if `values` is an object, place it in the set."
  [values]
  (let [values-set* (values->set* values)]
    (cond
      ;; most cases, guard for map to prevent conversion to keypairs
      (and (coll? values-set*)
           (not (map? values-set*)))
      (into #{} values-set*)
      ;; if `all` specified an object for the location, prevent conversion to keypairs
      (map? values-set*)
      #{values-set*}
      ;; attempt conversion to set, throw on error
      (some? values-set*)
      (try (set values-set*)
           (catch Exception e
             (throw (ex-info "Unexpected State!"
                             {:type ::rule-check-error
                              :values-set* values-set*}
                             e))))
      :else #{})))

(defn- follows-any?
  [any-set values]
  (->> values set (cset/intersection any-set) not-empty boolean))

;; -> everything within `all-set` within `values-set`?
;;    -> no = failure = (not (not false)) = false
;;    -> yes = continue
;;       -> anything within `target-set` that's not in `all-set`?
;;          -> yes = failure = (not (not false)) = false
;;          -> no = success = (not (not true)) = true 
(defn- follows-all?*
  "Checks `values-set` set against top and bottom bounds of `all-set`"
  [all-set values-set]
  ;; everything within `all-set` within `target-set`?
  (if (empty? (cset/difference all-set values-set))
    ;; anything within `target-set` that's not in `all-set`?
    (cset/superset? all-set values-set)
    false))

(defn- follows-all?
  [all-set values]
  (let [values-set (values->set values)]
    (not (or (contains? values ::unmatchable)
             (empty? values)
             ;; see `match-all-logic-test` below for logic proof
             (not (follows-all?* all-set values-set))))))

(defn- follows-none?
  [none-set values]
  (not (some (partial contains? none-set) values)))

(defn- follows-any-all-none?
  "Do the rule `values` follow `any`, `all`, and `none`? Note that if
   `presence` is `recommended` then these will not be strictly followed."
  [presence any all none values]
  (let [ignore? (-> presence (= "excluded"))
        strict? (-> presence (= "recommended") not)
        follow? (or strict? (not-empty values))]
    (or ignore?
        (and (if (and any follow?)
               (follows-any? any values)
               true)
             (if (and all follow?)
               (follows-all? all values)
               true)
             (if (and none follow?)
               (follows-none? none values)
               true)))))

(s/fdef follows-rule?
  :args (s/cat :statement ::xs/statement
               :rule ::parsed-rule
               :matches (s/? (s/every (s/or :json ::j/any
                                            :unmatchable #{::unmatchable}))))
  :ret boolean?)

(defn follows-rule?
  "Simple predicate check for `rule` being satisfied by a `statement`.
   You can pass in `matches` for efficiency's sake."
  [statement
   {:keys [any all none presence] :as rule}
   & [matches]]
  (let [values (or matches (match-rule statement rule))]
    (try (and (follows-presence? presence values)
              (follows-any-all-none? presence any all none values))
         (catch clojure.lang.ExceptionInfo e
           (if (->> e ex-data :type (= ::rule-check-error))
             (throw (ex-info (ex-message e)
                             (merge (ex-data e)
                                    {:type            ::rule-check-error
                                     :rule            rule
                                     :statement       statement
                                     :matched         matches
                                     :values          values})
                             e))
             (throw e))))))

(comment
  (def all-set-fixture #{:a :b :c})

  (defn replicate-conditional
    "assuming non-empty matchables which doesn't contain `::unmatchable`"
    [target-set]
    (not (or false false (not (follows-all?* all-set-fixture target-set)))))

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
;; Apply Rules Generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef apply-rules-gen
  :args (s/cat :partial-statement ::xs/statement
               :raw-rules (s/every ::rules/rule)
               :options (s/keys* :req-un [::random/seed]))
  :ret ::xs/statement)

(defn- valid-value?
  [{:keys [all none] :as _rule} v]
  (and (if all
         (contains? all v)
         true)
       (if none
         (not (contains? none v))
         true)))

;; TODO: We ensure that the rules pass, but we do not ensure that intermediate
;; parts of the statement are valid!
(defn apply-rules-gen
  "Given a partial statement and rules, attempt to make the statement satisfy
  the rules. Additional options like :seed help do this deterministically.
  "
  [partial-statement
   raw-rules
   & {:keys [seed]}]
  (let [rng (random/seed-rng seed)]
    (loop [statement partial-statement
           rules     (map parse-rule
                          raw-rules)
           paths     {:applied #{}
                      :excised #{}}]
      (if-let [{:keys [location selector
                       presence
                       any all none
                       ;; `spec` only in `rule` if previously shown to be `s/gen` safe and more accurate than `::j/any`
                       spec]
                :as   rule} (first rules)]
        (let [matches (match-rule statement rule)]
          (if (follows-rule? statement rule matches)
            ;; if the statement follows the rule, continue processing!
            (recur statement (rest rules) paths)
            ;; if not, we've got work to do..
            (if ;; the simplest case is an exclusion rule, which we can safely
                ;; apply without additional recursion
                (= presence "excluded")
              (let [statement-excised (cond-> (json-path/excise statement location :prune-empty? true)
                                        selector
                                        (json-path/excise (into location selector) :prune-empty? true))]
                (recur
                 statement-excised
                 (rest rules)
                 (update paths :excised into (some-> statement-excised
                                                     meta
                                                     :paths))))
              (let [discrete? (json-path/discrete? location)
                    location-enum (if discrete?
                                    (json-path/enumerate location)
                                    (json-path/enumerate location :limit 3))
                    ;; spec generation is expensive, let's wrap it up for control
                    gen-xapi! (fn []
                                (try (gen/generate
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
                                        (not-empty none) (gen/such-that (partial (complement contains?)
                                                                                 none)))
                                                   30 (random/rand-long rng))
                                     (catch clojure.lang.ExceptionInfo exi
                                       (throw (ex-info "Generation error!"
                                                       {:type            ::gen-error
                                                        :rule            rule
                                                        :statement       statement
                                                        :matched         matches}
                                                       exi)))))
                    any-all (not-empty (concat any all))

                    ;; In certain situations, we should attempt to make the
                    ;; values distinct. This is pretty open ended, but generally
                    ;; if the path points at an ID this is sane to do...
                    distinct? (= #{"id"} (last location))
                    values
                    (if (and discrete?
                             (= 1 (count location-enum))) ;; a single loc that must conform
                      (if (not-empty all)
                        (into [] all)
                        [(if (not-empty any)
                           (random/choose rng {} any)
                           (gen-xapi!))])
                      #_[(cond (not-empty any)
                             (random/choose rng {} any)
                             (not-empty all)
                             (random/choose rng {} all)
                             :else (gen-xapi!))]
                      ;; multiple discrete locs or infinite locs
                      (loop [loc-enum (cond->> location-enum
                                        ;; only needs limiting if not discrete
                                        (not discrete?) (take (max
                                                               ;; gotta be at least 1
                                                               1
                                                               ;; gotta be at least as many as matched
                                                               (count (remove (partial = ::unmatchable)
                                                                              matches))
                                                               ;; or as many as all provided vals
                                                               (count (concat any all))
                                                               ;; or maybe up to N
                                                               (if distinct? ;; for distinct, use what we have
                                                                 1
                                                                 (random/rand-int* rng 10)))))
                             vs []
                             any-sat? false]
                        (if-let [path (first loc-enum)]
                          (let [?match (get-in statement path)
                                v (cond (and (some? ?match) (valid-value? rule ?match))
                                        ?match
                                        any-all
                                        ;; try to use each provided val once
                                        (if-let [any-all-remaining (not-empty (remove
                                                                               (partial contains? vs)
                                                                               any-all))]
                                          (random/choose rng {} any-all-remaining)
                                          ;; but it is better to repeat then gen
                                          ;; unless this should be distinct...
                                          (if distinct?
                                            (gen-xapi!)
                                            (random/choose rng {} any-all)))
                                        :else
                                        (gen-xapi!))]
                            (recur (rest loc-enum)
                                   (conj vs v)
                                   (or any-sat?
                                       (empty? any)
                                       (and (not-empty any)
                                            (contains? any v))
                                       false)))
                          ;; handle possible lack of any...
                          (if any-sat?
                            vs
                            ;; if there's no any, just swap one
                            (let [swap-idx (random/rand-int* rng (count vs))]
                              (assoc vs swap-idx (random/choose rng {} any)))))))
                    ;; ;; TODO: remove unmatchable paths
                    statement-applied (json-path/apply-values
                                       statement location
                                       values
                                       :enum-limit 3)]
                (recur
                 statement-applied
                 (rest rules)
                 (update paths :applied into (some-> statement-applied
                                                     meta
                                                     :paths)))))))
        ;; all rules pass and we're done!
        statement
        ;; check the specs (dev/debug)

        #_(if (s/valid? ::xs/statement statement)
          statement
          (throw (ex-info "Healing not yet implemented"
                          {:type ::not-yet-implemented
                           :statement-error (s/explain-data ::xs/statement statement)})))))))
