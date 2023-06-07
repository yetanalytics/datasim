(ns com.yetanalytics.datasim.json.schema
  (:require [clojure.spec.alpha :as s]
            [clojure.walk :as w]
            [cheshire.core :as json]
            [com.yetanalytics.datasim.json :as j]))
(set! *warn-on-reflection* true)
;; FIXME: what does this look like using the new default as-code/as-data protocols?
;; FIXME: what does this look like using spec2?

;; FIXME: figure out priority in edge cases
;; - behavior of minItems/maxItems + positional specs
;;   -> if min items is 2 but there are 3 positional specs, does an array of 2 elements match if both elements match their positional spec?
;; - when exactly is the empty [] a successful match?

;; FIXME: fill in the various validation implementation gaps
;; -> in most cases, whole keys are ignored
;; -> ignored keys are the ones that fall under advanced usage of JSON Schema (IMO)

;; FIXME: test for handling of `type` as array
;; -> random selection from the array needs to be proven to be deterministic

;; FIXME: attach generators to fns (via `s/spec`) when easily built from args

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs for JSON Schema
;; - https://json-schema.org/draft/2019-09/json-schema-core.html#rfc.section.4.2.1
;; - https://json-schema.org/draft/2019-09/json-schema-validation.html#rfc.section.6
;; - https://json-schema.org/draft/2019-09/json-schema-core.html#rfc.section.9.3
;; - https://json-schema.org/draft/2019-09/meta/validation
;;
;; reference material
;; - https://json-schema.org/understanding-json-schema/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::pos
  pos?)

(s/def ::schema
  #{"hacky placeholder"})

(s/def ::double
  double?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::null
  nil?)

(s/def ::boolean
  boolean?)

(s/def ::number
  (s/or :double
        (s/double-in :infinite? false :NaN? false
                     :max 1000.0
                     :min -1000.0)
        :int
        int?))

(s/def ::string
  string?)

(s/def ::integer
  int?)

(s/def ::array
  vector?)

(s/def ::object
  map?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Can be found anywhere
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::valid-types
  #{"null" "boolean" "integer" "number" "string" "array" "object"})

(s/def ::type
  (s/or :array (s/coll-of ::valid-types :kind vector? :into [] :distinct true)
        :scalar ::valid-types))

(s/def ::enum
  (s/coll-of ::j/any :kind vector? :into [] :gen-max 3))

(s/def ::const
  ::j/any)

(s/def ::base-schema
  (s/keys :opt-un [::type
                   ::enum
                   ::const]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numeric validators
;; - can be found when "type" = "number" or "integer"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::pos-int
  (s/and ::pos ::integer))

(s/def ::multipleOf
  (s/and ::pos ::number))

(s/def ::minimum
  ::number)

(s/def ::exclusiveMinimum
  ::number)

(s/def ::maximum
  ::number)

(s/def ::exclusiveMaximum
  ::number)

(s/def ::integer-schema
  (s/keys :opt-un [::type
                   ::enum
                   ::const
                   ::multipleOf
                   ::minimum ::exclusiveMinimum
                   ::maximum ::exclusiveMaximum]))

(s/def ::number-schema
  (s/keys :opt-un [::type
                   ::enum
                   ::const
                   ::multipleOf
                   ::minimum ::exclusiveMinimum
                   ::maximum ::exclusiveMaximum]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String validators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::maxLength
  ::pos-int)

(s/def ::minLength
  (s/or :default #{0}
        :some ::pos-int))

(s/def ::pattern
  ;; specifically a regex but not mandatory
  ::string)

(s/def ::string-schema
  (s/keys :opt-un [::type
                   ::enum
                   ::const
                   ::maxLength
                   ::minLength
                   ::pattern]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Array validators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::maxItems
  ::pos-int)

(s/def ::minItems
  (s/or :default #{0}
        :some ::pos-int))

(s/def ::uniqueItems
  ::boolean)

(s/def ::maxContains
  ::pos-int)

(s/def ::minContains
  (s/or :default #{1}
        :some ::pos-int))

(s/def ::additionalItems
  ::schema)

(s/def ::contains
  ::schema)

(s/def ::items
  (s/or :schema          ::schema
        :position-schema (s/coll-of ::schema :into [] :kind vector? :gen-max 3)))

(s/def ::array-schema
  (s/keys :opt-un [::type
                   ::enum
                   ::const
                   ::maxItems
                   ::minItems
                   ::uniqueItems
                   ::maxContains
                   ::minContains
                   ::items
                   ::contains
                   ::additionalItems]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object validators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::string-array
  (s/coll-of ::string :kind vector? :into [] :distinct true))

(s/def ::maxProperties
  ::pos-int)

(s/def ::minProperties
  (s/or :default #{0}
        :some ::pos-int))

(s/def ::required
  ::string-array)

(s/def ::dependentRequired
  (s/and (s/map-of ::string ::j/any :kind map?)
         (fn [this]
           (if-let [the-props (or (get this "properties")
                                  (get this :properties))]
             (every? #(s/valid? ::string-array %) (vals the-props))
             true))))

(s/def ::properties
  (s/map-of ::string ::schema :kind map?))

(s/def ::patternProperties
  (s/map-of ::string ::schema :kind map?))

(s/def ::additionalProperties
  ::schema)

(s/def ::propertyNames
  ::schema)

(s/def ::object-schema
  (s/keys :opt-un [::type
                   ::enum
                   ::const
                   ::properties
                   ::patternProperties
                   ::additionalProperties
                   ::propertyNames
                   ::maxProperties
                   ::minProperties
                   ::required
                   ::dependentRequired]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::schema
  (s/or :base    ::base-schema
        :integer ::integer-schema
        :number  ::number-schema
        :string  ::string-schema
        :array   ::array-schema
        :object  ::object-schema))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON Schema -> Spec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare array-schema->spec)
(declare object-schema->spec)
(declare schema->spec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Number JSON Schema -> Spec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn number-boundaries->spec
  "return `s/int-in` or `s/or` `s/int-in` `s/double-in` based on `kind`
   -> respects `?exclusiveMin` and `?exclusiveMax` for ints
   -> respects 3 points of percision for double `?exclusiveMin` and `?exclusiveMax`
   -> respects inclusive `?min` and `?max` for both ints and doubles"
  [kind ?min ?exclusiveMin ?max ?exclusiveMax]
  (let [;; Possible Min/Max values
        the-min   (or ?min java.lang.Integer/MIN_VALUE)
        
        the-emin  (or ?exclusiveMin java.lang.Integer/MIN_VALUE)
        
        the-max   (or ?max java.lang.Integer/MAX_VALUE)
        
        the-emax  (or ?exclusiveMax java.lang.Integer/MAX_VALUE)

        ;; determine value which will get passed to spec creation
        ;; -> exclusive values are prefered to simplify exclusive/inclusive logic
        min-used* (cond (= the-min the-emin java.lang.Integer/MIN_VALUE)
                        {:default java.lang.Integer/MIN_VALUE}
                        
                        (and (= the-min java.lang.Integer/MIN_VALUE)
                             (not= the-emin java.lang.Integer/MIN_VALUE))
                        {:emin the-emin}
                        
                        (and (not= the-min java.lang.Integer/MIN_VALUE)
                             (= the-emin java.lang.Integer/MIN_VALUE))
                        {:min the-min}
                        
                        (and (not= the-min java.lang.Integer/MIN_VALUE)
                             (not= the-emin java.lang.Integer/MIN_VALUE)
                             (= the-min ?exclusiveMin))
                        {:emin the-emin}
                        
                        (and (not= the-min java.lang.Integer/MIN_VALUE)
                             (not= the-emin java.lang.Integer/MIN_VALUE)
                             (not= the-min ?exclusiveMin))
                        {:emin the-emin})
        
        max-used* (cond (= the-max the-emax java.lang.Integer/MAX_VALUE)
                        {:default java.lang.Integer/MAX_VALUE}
                        
                        (and (= the-max java.lang.Integer/MAX_VALUE)
                             (not= the-emax java.lang.Integer/MAX_VALUE))
                        {:emax the-emax}
                        
                        (and (not= the-max java.lang.Integer/MAX_VALUE)
                             (= the-emax java.lang.Integer/MAX_VALUE))
                        {:max the-max}
                        
                        (and (not= the-max java.lang.Integer/MAX_VALUE)
                             (not= the-emax java.lang.Integer/MAX_VALUE)
                             (= the-max ?exclusiveMax))
                        {:emax the-emax}
                        
                        (and (not= the-max java.lang.Integer/MAX_VALUE)
                             (not= the-emax java.lang.Integer/MAX_VALUE)
                             (not= the-max ?exclusiveMax))
                        {:emax the-emax})
        
        ;; select int min/max to use + account for exclusive/inclusive
        int-min (if-some [i-emin (:emin min-used*)]
                  (+ 1 i-emin)
                  (if-some [i-min (:min min-used*)]
                    i-min
                    (:default min-used*)))

        int-max (if-some [i-emax (:emax max-used*)]
                  i-emax
                  (if-some [i-max (:max max-used*)]
                    (+ 1 i-max)
                    (:default max-used*)))]
    
    ;; spec based off of `kind`
    (case kind
      "number" (let [;; select double min/max to use + account for exclusive/inclusive
                     ;; -> exclusive accurate up to 3 points of percision
                     double-min (if-some [d-emin (:emin min-used*)]
                                  (+ 0.001 d-emin)
                                  (if-some [d-min (:min min-used*)]
                                    d-min
                                    (:default min-used*)))
                     
                     double-max (if-some [d-emax (:emax max-used*)]
                                  (- d-emax 0.001)
                                  (if-some [d-max (:max max-used*)]
                                    d-max
                                    (:default max-used*)))]
                 
                 ;; spec which applies min/max constraints for both double and integer cases
                 (s/or :double (s/double-in :infinite? false
                                            :NaN? false
                                            :min double-min
                                            :max double-max)
                       :integer (s/int-in int-min int-max)))
      
      ;; spec which applies min/max constraints for the integer case
      "integer" (s/int-in int-min int-max))))

(defn number-schema->spec
  "Creates specs corresponding to JSON Schema Number Instance Validation constraints
   -> see: https://json-schema.org/draft/2019-09/json-schema-validation.html#rfc.section.6.2
   -> see: https://json-schema.org/draft/2019-09/json-schema-core.html#rfc.section.4.2.1"
  [_ number-schema]
  (let [;; destructure `number-schema`
        {?enum         "enum"
         ?const        "const"
         ?multiple-of  "multipleOf"
         ?min          "minimum"
         ?exclusiveMin "exclusiveMinimum"
         ?max          "maximum"
         ?exclusiveMax "exclusiveMaximum"
         schema-type   "type"} number-schema
        
        ;; use `?min`, `?exclusiveMin`, `?max`, and `?exclusiveMax` to determine upper/lower boundaries as spec via `number-boundaries->spec`
        boundaries-spec (number-boundaries->spec
                         schema-type
                         (when (s/valid? ::minimum ?min) ?min)
                         (when (s/valid? ::exclusiveMinimum ?exclusiveMin) ?exclusiveMin)
                         (when (s/valid? ::maximum ?max) ?max)
                         (when (s/valid? ::exclusiveMaximum ?exclusiveMax) ?exclusiveMax))

        ;; when `?enum` present in `number-schema`, use it to create validation fn
        enum-spec-fn (when (s/valid? ::enum ?enum) (fn [to-validate] (or (some #{to-validate} ?enum) false)))

        ;; when `?cons` present in `number-schema`, use it to create validation fn
        const-spec-fn (when (and (s/valid? ::const ?const) (contains? number-schema "const")) (fn [to-validate] (= to-validate ?const)))

        ;; build ontop of `boundaries-spec` as necessary
        seed-spec* (cond (and (fn? enum-spec-fn) (fn? const-spec-fn))
                         (s/and enum-spec-fn const-spec-fn boundaries-spec)

                         (and (nil? enum-spec-fn) (fn? const-spec-fn))
                         (s/and const-spec-fn boundaries-spec)

                         (and (fn? enum-spec-fn) (nil? const-spec-fn))
                         (s/and enum-spec-fn boundaries-spec)

                         (and (nil? enum-spec-fn) (nil? const-spec-fn))
                         boundaries-spec)

        ;; prep before pass to `s/valid?`, contains `pos?` which isn't nil safe, `-1` forces `::multipleOf` to fail
        multiple-of (or ?multiple-of -1)

        ;; when `?multiple-of` present in `number-schema`, use it to create validation fn
        multiples-fn (when (s/valid? ::multipleOf multiple-of)
                       (fn [to-validate]
                         (cond
                           ;; known to be integer so safe to call `mod`
                           (integer? to-validate)
                           (zero? (mod to-validate ?multiple-of))
                           ;; any non-integer number, perform division
                           ;; - double -> int -> double strips decimal from `div-result` for comparison to `div-result`
                           ;; - if `to-validate` is divisible by `?multiple-of`, nothing will be stripped and eq check passes
                           (number? to-validate)
                           (let [div-result (double (/ to-validate ?multiple-of))]
                             (= div-result
                                (-> div-result int double)))
                           ;; `to-validate` was getting passed in as key entry, e.g. [:integer 6]
                           (coll? to-validate)
                           (let [[k v] to-validate
                                 to-test (cond (integer? v)
                                               {:int v}
                                               (number? v)
                                               {:num v}
                                               (integer? k)
                                               {:int k}
                                               (number? k)
                                               {:num k})]
                             (if-some [n-to-test to-test]
                               (if-some [int-to-test (:int n-to-test)]
                                 (zero? (mod int-to-test ?multiple-of))
                                 (if-some [num-to-test (:num n-to-test)]
                                   (let [div-num-res (double (/ num-to-test ?multiple-of))]
                                     (= div-num-res
                                        (-> div-num-res int double)))
                                   false))
                               false))
                           :else (throw (ex-info "Unable to handle validation input"
                                                 {:to-validate   to-validate
                                                  :multiple-of   ?multiple-of})))))]
    (if (fn? multiples-fn)
      (s/and seed-spec*
             multiples-fn)
      seed-spec*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String JSON Schema -> Spec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn string-schema->spec
  "Creates specs corresponding to JSON Schema String Instance Validation constraints
   -> see: https://json-schema.org/draft/2019-09/json-schema-validation.html#rfc.section.6.3
   -> see: https://json-schema.org/draft/2019-09/json-schema-core.html#rfc.section.4.2.1"
  [_ string-schema]
  (let [;; destructure `string-schema`
        {?enum      "enum"
         ?const     "const"
         ?maxLength "maxLength"
         ?minLength "minLength"
         ?pattern   "pattern"} string-schema

        ;; when `?enum` present in `string-schema`, use it to create validation fn
        enum-spec-fn (when (s/valid? ::enum ?enum) (fn [to-validate] (or (some #{to-validate} ?enum) false)))

        ;; when `?cons` present in `string-schema`, use it to create validation fn
        const-spec-fn (when (and (s/valid? ::const ?const) (contains? string-schema "const")) (fn [to-validate] (= to-validate ?const)))

        ;; when `?maxLength` is present in `string-schema`, use it to create validation fn
        max-length  (or ?maxLength -1)
        max-spec-fn (when (s/valid? ::maxLength max-length) (fn [to-validate] (<= (count to-validate) ?maxLength)))

        ;; when `?minLength` is present in `string-schema`, use it to create validation fn
        min-length  (or ?minLength -1)
        min-spec-fn (when (s/valid? ::minLength min-length) (fn [to-validate] (>= (count to-validate) ?minLength)))

        ;; determine contribution of `?enum` and `?cons`
        seed-spec* (cond (and (fn? enum-spec-fn) (fn? const-spec-fn))
                         (s/and enum-spec-fn const-spec-fn ::string)
                         
                         (and (fn? enum-spec-fn) (nil? const-spec-fn))
                         (s/and enum-spec-fn ::string)
                         
                         (and (nil? enum-spec-fn) (fn? const-spec-fn))
                         (s/and const-spec-fn ::string)
                         
                         (and (nil? enum-spec-fn) (nil? const-spec-fn))
                         ::string)

        ;; determine contribution from `?maxLength` and `?minLength`
        seed-spec (cond (and (fn? max-spec-fn) (fn? min-spec-fn))
                        (s/and max-spec-fn min-spec-fn seed-spec*)

                        (and (fn? max-spec-fn) (nil? min-spec-fn))
                        (s/and max-spec-fn seed-spec*)

                        (and (nil? max-spec-fn) (fn? min-spec-fn))
                        (s/and min-spec-fn seed-spec*)

                        (and (nil? max-spec-fn) (nil? min-spec-fn))
                        seed-spec*)

        ;; determine contribution from `?pattern`
        pattern-fn (when (s/valid? ::pattern ?pattern) (fn [to-validate] (re-matches (re-pattern ?pattern) to-validate)))]

    ;; compose `pattern-spec` with `seed-spec` or return `seed-spec` 
    (if (fn? pattern-fn)
      (s/and pattern-fn seed-spec)
      seed-spec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Array JSON Schema -> Spec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn single-item->spec
  "create `s/coll-of` which respects `:min-count`, `:max-count` and `:distinct` via `?max-items`, `?min-items` and `?uni-items` respectively.

   Multiarity to support different sources for pred passed to `s/coll-of`
   -> pred = (`schema->spec` `rng` `item`) | `item-pred`"
  
  ;; pred passed in
  ([item-pred ?max-items ?min-items ?uni-items]
   (cond (and ?max-items ?min-items (true? ?uni-items))
        (s/coll-of item-pred :kind vector? :into [] :min-count ?min-items :max-count ?max-items :distinct ?uni-items)
        
        (and (nil? ?max-items) ?min-items (true? ?uni-items))
        (s/coll-of item-pred :kind vector? :into [] :min-count ?min-items :distinct ?uni-items)
        
        (and ?max-items (nil? ?min-items) (true? ?uni-items))
        (s/coll-of item-pred :kind vector? :into [] :max-count ?max-items :distinct ?uni-items)
        
        (and ?max-items ?min-items (not (true? ?uni-items)))
        (s/coll-of item-pred :kind vector? :into [] :min-count ?min-items :max-count ?max-items)
        
        (and (nil? ?max-items) (nil? ?min-items) (true? ?uni-items))
        (s/coll-of item-pred :kind vector? :into [] :distinct ?uni-items)
        
        (and ?max-items (nil? ?min-items) (not (true? ?uni-items)))
        (s/coll-of item-pred :kind vector? :into [] :max-count ?max-items)
        
        (and (nil? ?max-items) ?min-items (not (true? ?uni-items)))
        (s/coll-of item-pred :kind vector? :into [] :min-count ?min-items)
        
        (and (nil? ?max-items) (nil? ?min-items) (not (true? ?uni-items)))
        (s/coll-of item-pred :kind vector? :into [])))

  ;; pred generated via `schema->spec`
  ([item ?max-items ?min-items ?uni-items rng]
   (cond (and ?max-items ?min-items (true? ?uni-items))
         (s/coll-of (schema->spec rng item) :kind vector? :into [] :min-count ?min-items :max-count ?max-items :distinct ?uni-items)
         
         (and (nil? ?max-items) ?min-items (true? ?uni-items))
         (s/coll-of (schema->spec rng item) :kind vector? :into [] :min-count ?min-items :distinct ?uni-items)
         
         (and ?max-items (nil? ?min-items) (true? ?uni-items))
         (s/coll-of (schema->spec rng item) :kind vector? :into [] :max-count ?max-items :distinct ?uni-items)
         
         (and ?max-items ?min-items (not (true? ?uni-items)))
         (s/coll-of (schema->spec rng item) :kind vector? :into [] :min-count ?min-items :max-count ?max-items)
         
         (and (nil? ?max-items) (nil? ?min-items) (true? ?uni-items))
         (s/coll-of (schema->spec rng item) :kind vector? :into [] :distinct ?uni-items)
         
         (and ?max-items (nil? ?min-items) (not (true? ?uni-items)))
         (s/coll-of (schema->spec rng item) :kind vector? :into [] :max-count ?max-items)
         
         (and (nil? ?max-items) ?min-items (not (true? ?uni-items)))
         (s/coll-of (schema->spec rng item) :kind vector? :into [] :min-count ?min-items)
         
         (and (nil? ?max-items) (nil? ?min-items) (not (true? ?uni-items)))
         (s/coll-of (schema->spec rng item) :kind vector? :into []))))

(defn mk-kw-seq
  "kw seq of n length"
  [n]
  (repeat n (keyword (gensym "item-spec-in-array-spec"))))

(defmacro item-spec-vec->or-spec*
  "prep for creation of `s/or` used as `item-pred` in `single-item->spec`"
  [array-items]
  `(interleave (mk-kw-seq (count ~array-items)) ~array-items))

(defmacro item-spec-vec->or-spec
  "create `item-pred` for `single-item->spec`"
  [array-items]
  `(cons 's/or (item-spec-vec->or-spec* ~array-items)))

(defmacro many-items->spec
  "`s/and` of
     a) `s/tuple` made from specs within `array-items`
     b) `s/coll-of` (`s/or` made from specs within `array-items`) + `?max-items` + `?min-items` + `?uni-items`"
  [array-items ?max-items ?min-items ?uni-items]
  `(s/and
    ;; # and order of items matters
    (s/tuple ~@array-items)
    ;; additional constraints on the parent array itself
    (single-item->spec
     ;; array members must match ONE of the member specs, i.e. `array-items`
     ~(item-spec-vec->or-spec array-items)
     ;; sets `:max-count`, `:min-count` and `:distinct` respectively when non-nil
     ~?max-items ~?min-items ~?uni-items)))

(defmacro items-coll->spec
  "tuple spec based off of ordering in supplied `array-items-coll`"
  [array-items-coll ?max-items ?min-items ?uni-items rng]
  `(many-items->spec
    ;; vector of to-be-specs makes life easier
    [~@(for [each array-items-coll]
         ;; need data not code, code -> obj and cant embed obj in code!
         `(schema->spec ~rng ~each))]
    ;; constraints on parent array
    ~?max-items ~?min-items ~?uni-items))

(defmacro items-kind->spec
  "spec which covers all items in the parent array"
  [array-items ?max-items ?min-items ?uni-items rng]
  `(single-item->spec
    ~array-items
    ;; sets `:max-count`, `:min-count` and `:distinct` respectively when non-nil
    ~?max-items ~?min-items ~?uni-items ~rng))

(defmacro valid-items->spec
  "branch based on type implications"
  [items ?max-items ?min-items ?unique-items rng]
  `(cond (vector? ~items)
         (items-coll->spec ~items ~?max-items ~?min-items ~?unique-items ~rng)
         (map? ~items)
         (items-kind->spec ~items ~?max-items ~?min-items ~?unique-items ~rng)))

(defn- valid-items-spec
  "private fn which forces spec generation from the corresponding code representation
   i.e. escape the macro void and its dark magic with dark magic"
  [items ?max-items ?min-items ?unique-items rng]
  (eval `(valid-items->spec ~items ~?max-items ~?min-items ~?unique-items ~rng)))

(defn array-schema->spec
  "Partial/Minimal coverage of specs corresponding to JSON Schema Array Instance Validation constraints
   -> see: https://json-schema.org/draft/2019-09/json-schema-validation.html#rfc.section.6.4
   -> see: https://json-schema.org/draft/2019-09/json-schema-core.html#rfc.section.9.3.1"
  [rng array-schema]
  (let [;; destructure `array-schema`
        {?items            "items"
         ?enum             "enum"
         ?const            "const"
         ?min-items        "minItems"
         ?max-items        "maxItems"
         ?unique-items     "uniqueItems"} array-schema

        ;; when `?enum` present in `array-schema`, use it to create validation fn
        enum-spec-fn (when (s/valid? ::enum ?enum) (fn [to-validate] (or (some #{to-validate} ?enum) false)))

        ;; when `?cons` present in `array-schema`, use it to create validation fn
        const-spec-fn (when (and (s/valid? ::const ?const) (contains? array-schema "const")) (fn [to-validate] (= to-validate ?const)))
        
        ;; build up spec for enum/const, defaults to `::array` for safe handoff to `s/and`
        seed-spec (cond (and (fn? enum-spec-fn) (fn? const-spec-fn))
                        (s/and enum-spec-fn const-spec-fn)
                        
                        (and (nil? enum-spec-fn) (fn? const-spec-fn))
                        (s/and const-spec-fn ::array)
                        
                        (and (fn? enum-spec-fn) (nil? const-spec-fn))
                        (s/and enum-spec-fn ::array)
                        
                        (and (nil? enum-spec-fn) (nil? const-spec-fn))
                        ::array)]
    
    ;; create spec via `valid-items-spec` when `?items` included in `array-schema`
    (if (s/valid? ::items ?items)
      (s/and seed-spec
             (valid-items-spec ?items ?max-items ?min-items ?unique-items rng))
      (let [;; replace nils with known failure case for associated spec, nil is unsafe failure case
            min-items   (or ?min-items -1)
            max-items   (or ?max-items -1)
            
            ;; when `?min-items` present in `array-schema`, use to create validation fn
            min-itms-fn (when (s/valid? ::minItems min-items) (fn [to-validate] (>= (count to-validate) ?min-items)))

            ;; when `?max-items` present in `array-schema`, use to create validation fn
            max-itms-fn (when (s/valid? ::maxItems max-items) (fn [to-validate] (<= (count to-validate) ?max-items)))

            ;; when `?unique-items` present in `array-schema`, use to create validation fn
            unq-itms-fn (when (s/valid? ::uniqueItems ?unique-items)
                            (fn [to-validate]
                              (if ?unique-items
                                (cond (empty? to-validate)
                                      true
                                      (not-empty to-validate)
                                      (apply distinct? to-validate)
                                      :else false)
                                true)))
            
            ;; build up spec for min/max/unq item constraints
            array-constraints-spec (cond (and (fn? min-itms-fn) (fn? max-itms-fn) (fn? unq-itms-fn))
                                         (s/and min-itms-fn max-itms-fn unq-itms-fn)

                                         (and (nil? min-itms-fn) (fn? max-itms-fn) (fn? unq-itms-fn))
                                         (s/and max-itms-fn unq-itms-fn)

                                         (and (fn? min-itms-fn) (nil? max-itms-fn) (fn? unq-itms-fn))
                                         (s/and min-itms-fn unq-itms-fn)

                                         (and (fn? min-itms-fn) (fn? max-itms-fn) (nil? unq-itms-fn))
                                         (s/and min-itms-fn max-itms-fn)

                                         (and (fn? min-itms-fn) (nil? max-itms-fn) (nil? unq-itms-fn))
                                         (s/spec min-itms-fn)

                                         (and (nil? min-itms-fn) (fn? max-itms-fn) (nil? unq-itms-fn))
                                         (s/spec max-itms-fn)

                                         (and (nil? min-itms-fn) (nil? max-itms-fn) (fn? unq-itms-fn))
                                         (s/spec unq-itms-fn)

                                         (and (nil? min-itms-fn) (nil? max-itms-fn) (nil? unq-itms-fn))
                                         ::array)]
        (s/and seed-spec array-constraints-spec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object JSON Schema -> Spec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn property-schema->spec-fn
  "return a fn which validates a JSON object against the constraints defined in `properties`.
   -> resolves the specs for each kv pair in `properties`
   -> query `to-validate` for each k in `properties`
   -> validate query result against spec for the k"
  [properties rng]
  (fn [to-validate]
    (let [;; for each property, return vector of property name + spec derived from associated schema
          prop-name-to-spec (mapv (fn [[prop-name prop-schema]] [prop-name (schema->spec rng prop-schema)]) (into [] properties))

          ;; reduce over `prop-name-to-spec`, using the name to query `to-validate` and the derived spec for validating the query result
          spec-failures (reduce-kv
                         (fn [ack _ [p-name p-spec]]
                           (if-some [from-to-validate (get to-validate p-name)]
                             (if (s/valid? p-spec from-to-validate)
                               ack
                               (conj ack :failure))
                             ;; leaving out properties is valid
                             ack))
                         [] prop-name-to-spec)]
      (empty? spec-failures))))

(defn object-schema->spec
  "Partial/Minimal coverage of specs corresponding to JSON Schema Object Instance Validation constraints
   -> see: https://json-schema.org/draft/2019-09/json-schema-validation.html#rfc.section.6.5
   -> see: https://json-schema.org/draft/2019-09/json-schema-core.html#rfc.section.9.3.2"
  [rng object-schema]
  (let [;; destructure `object-schema`
        {?enum          "enum"
         ?const         "const"
         ?maxProperties "maxProperties"
         ?minProperties "minProperties"
         ?required      "required"
         ?properties    "properties"} object-schema

        ;; when `?enum` present in `array-schema`, use it to create validation fn
        enum-spec-fn (when (s/valid? ::enum ?enum) (fn [to-validate] (or (some #{to-validate} ?enum) false)))

        ;; when `?cons` present in `array-schema`, use it to create validation fn
        const-spec-fn (when (and (s/valid? ::const ?const) (contains? object-schema "const")) (fn [to-validate] (= to-validate ?const)))

        ;; use `?enum` and `?cons` to derive `seed-spec*`
        seed-spec* (cond (and (fn? enum-spec-fn) (fn? const-spec-fn))
                         (s/and enum-spec-fn const-spec-fn ::object)
                         
                         (and (nil? enum-spec-fn) (fn? const-spec-fn))
                         (s/and const-spec-fn ::object)
                         
                         (and (fn? enum-spec-fn) (nil? const-spec-fn))
                         (s/and enum-spec-fn ::object)
                         
                         (and (nil? enum-spec-fn) (nil? const-spec-fn))
                         ::object)

        ;; handle `?maxProperties` and `?minProperties` prior to spec check, force failure instead of error
        max-props (or ?maxProperties -1)
        min-props (or ?minProperties -1)
        
        ;; when `?maxProperties` is present in `object-schema`, use it to create validation fn
        max-props-fn (when (s/valid? ::maxProperties max-props) (fn [to-validate] (<= (count (keys to-validate)) ?maxProperties)))

        ;; when `?minProperties` is present in `object-schema`, use it to create validation fn
        min-props-fn (when (s/valid? ::minProperties min-props) (fn [to-validate] (>= (count (keys to-validate)) ?minProperties)))

        ;; when `?required` is present in `object-schema`, use it to create validation fn
        required-fn (when (s/valid? ::required ?required) (fn [to-validate] (every? #(contains? to-validate %) ?required)))

        ;; spec of all constraints besides `?properties` derived ones
        seed-spec (cond (and (fn? max-props-fn) (fn? min-props-fn) (fn? required-fn))
                        (s/and max-props-fn min-props-fn required-fn seed-spec*)

                        (and (nil? max-props-fn) (fn? min-props-fn) (fn? required-fn))
                        (s/and min-props-fn required-fn seed-spec*)

                        (and (fn? max-props-fn) (nil? min-props-fn) (fn? required-fn))
                        (s/and max-props-fn required-fn seed-spec*)

                        (and (fn? max-props-fn) (fn? min-props-fn) (nil? required-fn))
                        (s/and max-props-fn min-props-fn seed-spec*)

                        (and (fn? max-props-fn) (nil? min-props-fn) (nil? required-fn))
                        (s/and max-props-fn seed-spec*)

                        (and (nil? max-props-fn) (fn? min-props-fn) (nil? required-fn))
                        (s/and min-props-fn seed-spec*)

                        (and (nil? max-props-fn) (nil? min-props-fn) (fn? required-fn))
                        (s/and required-fn seed-spec*)

                        (and (nil? max-props-fn) (nil? min-props-fn) (nil? required-fn))
                        seed-spec*)]
    
    ;; when `?properties` is present in `object-schema`, create spec for it + comp with `seed-spec` otherwise return `seed-spec`
    (if (s/valid? ::properties ?properties)
      (s/and seed-spec
             (property-schema->spec-fn ?properties rng))
      seed-spec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON Schema -> spec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: `rng` is now unused

(defn- dynamic-or
  "Similar to `s/or`, but accepts a coll of `[:key pred]` pairs. Unlike
   `s/or`, this accepts a dynamic number of such pairs (hence why it
   is a function instead of a macro)."
  [key-pred-pairs]
  (let [keys  (mapv first key-pred-pairs)
        preds (mapv second key-pred-pairs)]
    ;; Yes, spec says not to use `or-spec-impl`, but we need to create
    ;; `s/or` specs at runtime and it is much easier to bypass the macro
    ;; instead of mixing compile-time and run-time code.
    (s/or-spec-impl keys preds preds nil)))

(defn- coll-schema-spec
  [type->spec schema-types]
  (let [pairs (map (fn [schema-type]
                     (case schema-type
                       "null"    [:null ::null]
                       "boolean" [:boolean ::boolean]
                       "integer" [:integer ::integer]
                       "number"  [:number (type->spec "number")]
                       "string"  [:string (type->spec "string")]
                       "array"   [:array (type->spec "array")]
                       "object"  [:object (type->spec "object")]))
                   schema-types)]
    (dynamic-or pairs)))

(defn schema->spec
  ([rng schema]
   (let [parsed-schema (cond (string? schema)
                             (json/parse-string schema)
                             (map? schema)
                             (w/stringify-keys schema))
         {ext-val-type "type"} parsed-schema]
     (letfn [(schema-dispatch [ext-val-t]
               (case ext-val-t
                 "null"    ::null
                 "boolean" ::boolean
                 "integer" ::integer
                 "number"  (schema->spec rng ext-val-type parsed-schema)
                 "string"  (schema->spec rng ext-val-type parsed-schema)
                 "array"   (schema->spec rng ext-val-type parsed-schema)
                 "object"  (schema->spec rng ext-val-type parsed-schema)))]
       (cond (coll? ext-val-type)
             (coll-schema-spec #(schema->spec rng % (assoc parsed-schema "type" %))
                               ext-val-type)
             (string? ext-val-type)
             (schema-dispatch ext-val-type)
             :else ::j/any))))
  ([rng complex-kind complex-schema]
   (case complex-kind
     "array" (array-schema->spec rng complex-schema)
     "object" (object-schema->spec rng complex-schema)
     "number" (number-schema->spec rng complex-schema)
     "string" (string-schema->spec rng complex-schema))))

(comment
  (require '[clojure.spec.gen.alpha :as sgen])
  
  (let [ext-val-type ["number" "string"]]
    (coll-schema-spec #(schema->spec nil % {"type" %}) ext-val-type))

  (sgen/generate (s/gen (schema->spec nil "{\"type\": [\"number\", \"string\"]}")))
  
  (sgen/generate (s/gen (schema->spec nil "{\"type\": \"number\"}")))
  
  (sgen/generate (s/gen (s/or :foo (fn [_] false))))
  )
