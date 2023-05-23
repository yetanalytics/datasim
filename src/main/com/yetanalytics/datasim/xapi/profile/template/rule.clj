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
            [com.yetanalytics.datasim.random    :as random]))

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

(s/def ::parsed-rule
  (s/keys :req-un [::location]
          :opt-un [::any
                   ::all
                   ::none
                   ::selector
                   ::rules/presence]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Parse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(def parse-rule
  (memo/lru parse-rule* {} :lru/threshold 4096))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Follow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- not-empty?
  "Like `not-empty` but returns a boolean."
  [coll]
  (boolean (not-empty coll)))

(defn- match-rule
  "The matching logic from https://github.com/adlnet/xapi-profiles/blob/master/xapi-profiles-communication.md#21-statement-template-validation
  returns a tuple, a list of matched values from location, selector, returning an empty vector if a selector cannot be matched."
  [statement
   {:keys [location selector] :as _rule}]
  (cond-> (path/get-values* statement location)
    selector
    (->> (mapcat #(path/get-values* % selector)) vec)))

(s/fdef follows-rule?
  :args (s/cat :statement ::xs/statement
               :rule ::parsed-rule)
  :ret boolean?)

(defn follows-rule?
  "simple predicate check for a rule being satisfied by a statement
  a la https://github.com/adlnet/xapi-profiles/blob/master/xapi-profiles-communication.md#21-statement-template-validation."
  [statement {:keys [any all none presence] :as rule}]
  (let [strict? (not= presence "recommended")
        ?values (not-empty (match-rule statement rule))]
    (and (case presence
           "included" (some? ?values)
           "excluded" (nil? ?values)
           true)
         (or (= presence "excluded")
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
  [statement {:keys [location selector]}]
  (let [paths (cond-> location
                selector (join-location-and-selector selector))]
    (path/excise* statement paths {:prune-empty? true})))

;; Rule Application

(defn- distinct-values? [parsed-paths]
  (->> parsed-paths
       (map last)
       (filter coll?)
       (map set)
       (some #(not-empty (cset/intersection % distinct-value-properties)))))

(defn- rule-value-set
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
          :else nil)] ; No any, all, or none - must resort to generation
    (if (or (nil? ?value-set)
            (not-empty ?value-set))
      ?value-set
      (throw (ex-info "Cannot generate non-empty rule value set from rule!"
                      {:kind ::invalid-rule-values
                       :rule rule})))))

(defn- rule-value-coll
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
                 (into val-coll (repeat n x))))))))

;; `spec` only in `rule` if previously shown to be `s/gen` safe and more accurate than `::j/any`
(defn- apply-rule
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
               :raw-rules (s/every ::rules/rule)
               :options (s/keys* :req-un [::random/seed]))
  :ret ::xs/statement)

;; TODO: We ensure that the rules pass, but we do not ensure that intermediate
;; parts of the statement are valid!
(defn apply-rules-gen
  "Given a partial statement and rules, attempt to make the statement satisfy
  the rules. Additional options like :seed help do this deterministically."
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
         (= "excluded" presence)
         (excise-rule statement rule)
         ;; Otherwise, we need to apply rule values.
         :else
         (apply-rule statement rule rng)))
     partial-statement
     rules)))
