(ns com.yetanalytics.datasim.xapi.extensions
  (:require [com.yetanalytics.datasim.xapi.profile.template.rule :as rule]
            [com.yetanalytics.datasim.json.schema :as jschema]
            [clojure.spec.alpha :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn containsv?
  "does `coll` contain `query`?"
  [^clojure.lang.PersistentVector coll query]
  (not= -1 (.indexOf coll query)))

;; FIXME: support activity extensions + substatement extensions

;; FIXME: Make it work with the JSONPath pipe ("|") operator
;; Right now it only works for the first entry

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 1, find all rules about extensions whose gen defaults to `:com.yetanalytics.datasim.json/any`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn filter-for-extension-rule-fn
  "parsed `:location` needs to indicate the rule is about an extension"
  [rule]
  (let [{t-loc :location} (rule/parse-rule rule)]
    (->> t-loc
         first ; FIXME: Make it work with JSONPath pipe operator
         (filter (fn [t-entry]
                   (and (coll? t-entry) ; not a wildcard
                        (containsv? t-entry "extensions"))))
         not-empty)))

(defn filter-for-rel-ext-rules-fn
  "only care about rules which provide no additional info to go off of in majority of cases"
  [ext-rule]
  (let [{ext-pres :presence
         ext-any  :any
         ext-all  :all
         ext-none :none} (rule/parse-rule ext-rule)]
    (if (not= :included ext-pres)
      false
      (if (some? (or (not-empty ext-any) (not-empty ext-all) (not-empty ext-none)))
        ;; rule gives us something to go off of
        false
        ;; rule doesn't give us anything to go off of
        true))))

(defn filter-for-extension-rules*
  "filter `template-rules` for rules targeting an extension"
  [template-rules]
  (filterv filter-for-extension-rule-fn template-rules))

(defn filter-for-extension-rules
  "return rules for the current template which specify a context or result extension but don't
   provide any generation hints within the rule itself"
  [template-rules]
  (filterv filter-for-rel-ext-rules-fn (filter-for-extension-rules* template-rules)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 2, find all associated concepts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn search-profile-for-extension
  "filter `:concepts` within `profile` for one with id = `ext-id`"
  [ext-id profile]
  (->> profile
       :concepts
       (filterv (fn [a-concept] (= (:id a-concept) ext-id)))
       first))

(defn extension-concept-from-profiles
  "search `profiles` for a concept with id = `ext-id`"
  [profiles ext-id]
  (->> profiles
       (mapv (partial search-profile-for-extension ext-id))
       (filterv some?)
       first))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 3, create spec from `inlineSchema` and add to rule when appropriate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn in-line-schema->spec
  "When an `inlineSchema` is found for an extension referenced by `raw-rule`:
  
    1) use the JSON Schema to create a `s/spec` via `jschema/schema->spec`
    2) determine if the `s/spec` can be the basis of a `s/gen`
  
  The `s/spec` is only added to `raw-rule` at `:spec` if a generator can be created from it via `s/gen`,
  otherwise return `raw-rule` unaltered and rely on default of `com.yetanalytics.datasim.json/any`"
  [profiles rng raw-rule parsed-loc]
  (let [[_ _ ext-id-set] (first parsed-loc) ; FIXME: Make it work with JSONPath pipe operator
        {json-schema-str :inlineSchema
         :as from-ps} (extension-concept-from-profiles profiles (first ext-id-set))]
    (if (and (string? json-schema-str) (not-empty json-schema-str))
      ;; see `com.yetanalytics.datasim.json.schema` for spec creation logic
      (let [the-spec (jschema/schema->spec rng json-schema-str)]
        (if (try (s/gen the-spec) (catch Exception _ nil))
          ;; able to create an `s/gen` for `the-spec`, add `the-spec` to `raw-rule` at `:spec`
          (assoc raw-rule :spec the-spec)
          ;; unable to create `s/gen` without help, `com.yetanalytics.datasim.json/any` is good enough
          raw-rule))
      ;; can't do better than default without a `:inlineSchema`
      ;; -> fetching JSON Schema via `:schema` IRI not currently supported
      raw-rule)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 4, add spec hint to rule for later pickup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-spec-to-ext-rule-fn
  "When `extension-rule` is describing a context or result extension, continue on to `in-line-schema->spec`,
   otherwise return `extension-rule` unaltered"
  [profiles rng extension-rule]
  (let [{ext-loc :location} (rule/parse-rule extension-rule)
        ext-kind-coll       (ffirst ext-loc)] ; FIXME: Make it work with JSONPath pipe operator
    (if (or (= ext-kind-coll ["context"]) (= ext-kind-coll ["result"]))
      (in-line-schema->spec profiles rng extension-rule ext-loc)
      ;; not supporting any other types of extensions at this time, may be revisited and expanded at future date
      extension-rule)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All steps fn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn derive-generation-hint
  "attempt to determine extension spec that's more accurate than `:com.yetanalytics.datasim.json/any`
   -> if such a spec is derived, will be added to the rule at the key `:spec`
      -> requires an `s/gen` to be created from the derived spec in order to replace `:com.yetanalytics.datasim.json/any`"
  [profiles rng template-rules]
  (let [tmpl-ext-rules-coll (filter-for-extension-rules template-rules)]
    (if (empty? tmpl-ext-rules-coll)
      ;; no relevant rules within current template, return `template-rules` unaltered
      template-rules
      (let [ext-rule-locs     (mapv :location tmpl-ext-rules-coll)
            without-ext-rules (filterv (fn [tmpl-rule] (not (containsv? ext-rule-locs (:location tmpl-rule))))
                                       template-rules)
            updated-rules     (mapv (partial add-spec-to-ext-rule-fn profiles rng)
                                    tmpl-ext-rules-coll)]
        (into without-ext-rules updated-rules)))))

(comment
  (def profiles
    [{:concepts [{:id "https://w3id.org/xapi/video/extensions/volume"
                  :inScheme "https://w3id.org/xapi/video/v1.0.3"
                  :type "ContextExtension"
                  :definition {:en "Used to identify the loudness of sound specified for a media object."}
                  :prefLabel {:en "volume"}
                  :inlineSchema "{ \"type\": \"number\" }"}]}])

  (def template-rules
    [{:location "$.context.extensions['https://w3id.org/xapi/video/extensions/volume']"
      :presence "included"}])

  (def rng 123)

  ;; TODO: Turn into tests
  (and (some? (:spec (first (derive-generation-hint profiles rng template-rules))))
       (s/valid? (:spec (first (derive-generation-hint profiles rng template-rules))) 1)
       (s/valid? (:spec (first (derive-generation-hint profiles rng template-rules))) 1.0)
       (s/valid? (:spec (first (derive-generation-hint profiles rng template-rules))) -1)
       (s/valid? (:spec (first (derive-generation-hint profiles rng template-rules))) -1.0)
       (not (s/valid? (:spec (first (derive-generation-hint profiles rng template-rules))) "1")))
  )




