(ns com.yetanalytics.datasim.xapi.profile.template.rule
  "Apply statement template rules for generation"
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.pan.objects.templates.rules :as rules]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.json :as j]
            [com.yetanalytics.datasim.json.path :as json-path]
            [com.yetanalytics.datasim.xapi.path :as xp]
            [com.yetanalytics.datasim.random :as random]
            [clojure.set :as cset]
            [clojure.test.check.generators :as gen]))

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

(defn parse-rule
  "Parse paths in a rule"
  [{:keys [location selector] :as rule}]
  (cond-> (assoc
           (reduce-kv
            (fn [m k v]
              (if (= :presence k)
                (assoc m k v)
                (assoc m k (set v))))
            {}
            (select-keys rule [:any :all :none :presence]))
           :location (into []
                           (json-path/parse location)))
    selector (assoc :selector
                    (into [] (json-path/parse selector)))))

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
  (let [loc-values (into #{}
                         (json-path/select statement location))]
    (into loc-values
          (when selector
            (mapcat
             (fn [lv]
               (let [selection (json-path/select lv selector)]
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
                      (not-empty (remove values #{::unmatchable})))
               false
               true)
             "recommended" true)
           true)
         (if (= presence "excluded")
           true ;; ignore
           (and
            (if (and any
                     (or strict (not-empty values)))
              (not (empty? (cset/intersection (set values) any)))
              true)
            (if (and all
                     (or strict (not-empty values)))
              (not (or (contains? values ::unmatchable)
                       (empty? values)
                       (not (cset/superset? all
                                            (set values)))))
              true)
            (if (and none
                     (or strict (not-empty values)))
              (not (some (partial contains? none)
                         values))
              true))))))

(s/fdef rule-gen
  :args (s/cat :rule ::parsed-rule
               :xapi-gen gen/generator?)
  :ret (s/nilable gen/generator?))

(defn rule-gen
  "Return a generator for values based on a rule, using a provided xapi
  generator for undefined things."
  [{:keys [any all none]} xapi-gen]
  (if all
    (gen/elements all)
    (gen/one-of (if any
                  [(gen/elements any) xapi-gen]
                  [xapi-gen]))))


(s/fdef apply-rules-gen
  :args (s/cat :partial-statement ::xs/statement
               :raw-rules (s/every ::rules/rule)
               :options (s/keys* :req-un [::random/seed]))
  :ret ::xs/statement)

(defn- valid-value?
  [{:keys [all any none] :as rule} v]
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
                       any all none]
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
              ;; for other rules, we'll need to get fancier...
              (let [location-enum (json-path/enumerate location)
                    xapi-spec     (try (xp/path->spec
                                        ::xs/statement
                                        (first location-enum)
                                        statement)
                                       (catch java.lang.AssertionError ae
                                         (throw (ex-info "Couldn't figure out xapi path"
                                                         {:type      ::undefined-path
                                                          :key-path  (first location-enum)
                                                          :rule      rule
                                                          :statement statement}
                                                         ae))))

                    xapi-gen      (s/gen xapi-spec)
                    xapi-rule-gen (rule-gen rule xapi-gen)

                    ;; TODO: remove unmatchable paths

                    valid-values (into #{}
                                       (filter (partial valid-value? rule)
                                               (remove #{::unmatchable} matches)))

                    rule-or-val-gen   (cond->> (gen/one-of (into [xapi-rule-gen]
                                                                 (map gen/return
                                                                      valid-values)))
                                        ;; must be in all if defined
                                        all  (gen/such-that (partial contains? all))
                                        ;; must not be in none if defined
                                        none (gen/such-that (complement (partial contains? none))))
                    values-gen        (cond->> (if (json-path/discrete? location)
                                                 (let [path-count (count location-enum)]
                                                   (if (= 1 path-count)
                                                     (gen/tuple
                                                      (if any
                                                        (gen/elements any)
                                                        rule-or-val-gen))
                                                     (apply gen/tuple (repeat path-count
                                                                              rule-or-val-gen))))
                                                 ;; deal with distinct arrays
                                                 (if (#{:definition/choices
                                                        :definition/scale
                                                        :definition/source
                                                        :definition/target
                                                        :definition/steps} xapi-spec)
                                                   (gen/vector-distinct rule-or-val-gen
                                                                        {:min-elements 1
                                                                         :max-elements (or (and all)
                                                                                           (count all))})
                                                   (gen/vector rule-or-val-gen 1 10)))
                                        ;; at least 1 any
                                        any (gen/such-that (partial some (partial contains? any))))
                    statement-applied (json-path/apply-values
                                       statement location
                                       (try (gen/generate values-gen 30 seed)
                                            (catch clojure.lang.ExceptionInfo exi
                                              (throw (ex-info "Generation error!"
                                                              {:type            ::gen-error
                                                               :rule            rule
                                                               :statement       statement
                                                               :matched         matches
                                                               :rule-or-val-gen rule-or-val-gen
                                                               :values-gen      values-gen
                                                               :seed            seed
                                                               }
                                                              exi)))))]
                (recur
                 statement-applied
                 (rest rules)
                 (update paths :applied into (some-> statement-applied
                                                     meta
                                                     :paths)))))))
        ;; all rules pass and we're done!
        ;; but not quite... we have to walk up removed/changed/added paths and
        ;; check the specs
        (if (s/valid? ::xs/statement statement)
          statement
          (throw (ex-info "Healing not yet implemented"
                          {:type ::not-yet-implemented
                           :statement-error (s/explain-data ::xs/statement statement)})))))))
