(ns com.yetanalytics.datasim.json.schema-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.json.schema :refer :all :as jschema]
            [clojure.spec.alpha :as s]))

(deftest number-boundaries->spec-test
  (testing "creation of number spec based on `type`, `min`, `exclusiveMin`, `max` and/or `exclusiveMax`"
    (testing "doubles and ints, pos, neg and 0"
      (is (s/valid? (number-boundaries->spec "number" nil nil nil nil) 1.0))
      (is (s/valid? (number-boundaries->spec "number" nil nil nil nil) -1.0))
      (is (s/valid? (number-boundaries->spec "number" nil nil nil nil) 1))
      (is (s/valid? (number-boundaries->spec "number" nil nil nil nil) -1))
      (is (s/valid? (number-boundaries->spec "number" nil nil nil nil) 0)))
    (testing "?min (5) to ?max (10), inclusive"
      (is (not (s/valid? (number-boundaries->spec "number" 5 nil 10 nil) 4)))
      (is (s/valid? (number-boundaries->spec "number" 5 nil 10 nil) 5))
      (is (s/valid? (number-boundaries->spec "number" 5 nil 10 nil) 7))
      (is (s/valid? (number-boundaries->spec "number" 5 nil 10 nil) 10))
      (is (not (s/valid? (number-boundaries->spec "number" 5 nil 10 nil) 11))))
    (testing "?min (5.0) to ?max (10.0), inclusive"
      (is (not (s/valid? (number-boundaries->spec "number" 5.0 nil 10.0 nil) 4.9)))
      (is (s/valid? (number-boundaries->spec "number" 5.0 nil 10.0 nil) 5.0))
      (is (s/valid? (number-boundaries->spec "number" 5.0 nil 10.0 nil) 7.0))
      (is (s/valid? (number-boundaries->spec "number" 5.0 nil 10.0 nil) 10.0))
      (is (not (s/valid? (number-boundaries->spec "number" 5.0 nil 10.0 nil) 10.1))))
    (testing "?exclusiveMin (5) to ?exclusiveMax (10)"
      (is (not (s/valid? (number-boundaries->spec "number" nil 5 nil 10) 5)))
      (is (s/valid? (number-boundaries->spec "number" nil 5 nil 10) 6))
      (is (s/valid? (number-boundaries->spec "number" nil 5 nil 10) 9))
      (is (not (s/valid? (number-boundaries->spec "number" nil 5 nil 10) 10))))
    (testing "?exclusiveMin (5.0) to ?exclusiveMax (10.0)"
      (is (not (s/valid? (number-boundaries->spec "number" nil 5.0 nil 10.0) 5.0)))
      (is (s/valid? (number-boundaries->spec "number" nil 5.0 nil 10.0) 5.1))
      (is (s/valid? (number-boundaries->spec "number" nil 5.0 nil 10.0) 7.0))
      (is (s/valid? (number-boundaries->spec "number" nil 5.0 nil 10.0) 9.999))
      (is (not (s/valid? (number-boundaries->spec "number" nil 5.0 nil 10.0) 10.0))))
    (testing "true positives followed by false negatives, i.e. accurate up to 3 points of percision"
      (is (s/valid? (number-boundaries->spec "number" nil 5.0 nil 10.0) 9.999))
      (is (not (s/valid? (number-boundaries->spec "number" nil 5.0 nil 10.0) 9.9999)))
      (is (s/valid? (number-boundaries->spec "number" nil 5.0 nil 10.0) 5.001))
      (is (not (s/valid? (number-boundaries->spec "number" nil 5.0 nil 10.0) 5.0001))))))

(deftest number-schema->spec-test
  (testing "creation of spec for various JSON schemas describing a numeric instance"
    (let [;; ;; all keys comp to only 1 matching number - 6
          number-6        {"type"       "number"
                           "multipleOf" 2.0
                           "enum"       [3 6 9 12]
                           "const"      6}
          ;; `multipleOf` validation derived from remainder, not type
          mul-2-num-6-dbl {"type"       "number"
                           "multipleOf" 2
                           "const"      6.0}
          ;; `enum` places upper bounds on `multipleOf`
          mul-of-2-max-20 {"type"       "number"
                           "multipleOf" 2
                           "enum"       [2 4 6 8 10 12 14 16 18 20]}
          ;; any multiple of 2.1
          mul-of-2-dot-1  {"type"       "number"
                           "multipleOf" 2.1}
          ;; either 1, 2 or 3
          one-or-2-or-3   {"type"       "number"
                           "enum"       [1 2 3]}
          ;; must be 27
          static-27       {"type"       "number"
                           "const"      27}
          ;; any number
          some-number     {"type"       "number"}
          ;; `rng` only comes into play when "type" is array valued, ignoring this case for now
          rng             nil]
      (testing "comp of `type`, `multipleOf`, `enum`, `const`"
        (is (s/valid? (number-schema->spec rng number-6) 6))
        (is (not (s/valid? (number-schema->spec rng number-6) 6.0)))
        (is (not (s/valid? (number-schema->spec rng number-6) 12)))
        (is (not (s/valid? (number-schema->spec rng number-6) 9))))
      (testing "comp of `type`, `multipleOf`, `const`"
        (is (s/valid? (number-schema->spec rng mul-2-num-6-dbl) 6.0))
        (is (not (s/valid? (number-schema->spec rng mul-2-num-6-dbl) 6)))
        (is (not (s/valid? (number-schema->spec rng mul-2-num-6-dbl) 12.0)))
        (is (not (s/valid? (number-schema->spec rng mul-2-num-6-dbl) 12))))
      (testing "comp of `type`, `multipleOf`, `enum`"
        (is (s/valid? (number-schema->spec rng mul-of-2-max-20) 2))
        (is (s/valid? (number-schema->spec rng mul-of-2-max-20) 10))
        (is (s/valid? (number-schema->spec rng mul-of-2-max-20) 20))
        (is (not (s/valid? (number-schema->spec rng mul-of-2-max-20) 22)))
        (is (not (s/valid? (number-schema->spec rng mul-of-2-max-20) 10.0)))
        (is (not (s/valid? (number-schema->spec rng mul-of-2-max-20) -2))))
      (testing "comp of `type`, `multipleOf`"
        (is (s/valid? (number-schema->spec rng mul-of-2-dot-1) 6.3))
        (is (s/valid? (number-schema->spec rng mul-of-2-dot-1) -4.2))
        (is (not (s/valid? (number-schema->spec rng mul-of-2-dot-1) 7.5))))
      (testing "comp of `type`, `enum`"
        (is (s/valid? (number-schema->spec rng one-or-2-or-3) 1))
        (is (s/valid? (number-schema->spec rng one-or-2-or-3) 2))
        (is (s/valid? (number-schema->spec rng one-or-2-or-3) 3))
        (is (not (s/valid? (number-schema->spec rng one-or-2-or-3) 4)))
        (is (not (s/valid? (number-schema->spec rng one-or-2-or-3) 1.0)))
        (is (not (s/valid? (number-schema->spec rng one-or-2-or-3) 2.0)))
        (is (not (s/valid? (number-schema->spec rng one-or-2-or-3) 3.0))))
      (testing "comp of `type`, `const`"
        (is (s/valid? (number-schema->spec rng static-27) 27))
        (is (not (s/valid? (number-schema->spec rng static-27) 27.0)))
        (is (not (s/valid? (number-schema->spec rng static-27) "27")))
        (is (not (s/valid? (number-schema->spec rng static-27) 11))))
      (testing "only given `type`"
        (is (s/valid? (number-schema->spec rng some-number) 0))
        (is (s/valid? (number-schema->spec rng some-number) 27))
        (is (s/valid? (number-schema->spec rng some-number) -27))
        (is (s/valid? (number-schema->spec rng some-number) 27.0))
        (is (s/valid? (number-schema->spec rng some-number) -27.0))
        (is (not (s/valid? (number-schema->spec rng some-number) "27")))))))

(deftest string-schema->spec-test
  (testing "creation of spec for various JSON schemas describing a string instance"
    (testing "its a string"
      (is (s/valid? (string-schema->spec nil {"type" "string"}) "foo"))
      (is (not (s/valid? (string-schema->spec nil {"type" "string"}) 1))))
    (testing "max length"
      (is (s/valid? (string-schema->spec nil {"type" "string" "maxLength" 4}) "fooo"))
      (is (not (s/valid? (string-schema->spec nil {"type" "string" "maxLength" 4}) "foooo"))))
    (testing "min length"
      (is (s/valid? (string-schema->spec nil {"type" "string" "minLength" 2}) "fo"))
      (is (not (s/valid? (string-schema->spec nil {"type" "string" "minLength" 2}) "f"))))
    (testing "min + max length"
      (is (s/valid? (string-schema->spec nil {"type" "string" "minLength" 2 "maxLength" 4}) "foo"))
      (is (not (s/valid? (string-schema->spec nil {"type" "string" "minLength" 2 "maxLength" 4}) "foooo")))
      (is (not (s/valid? (string-schema->spec nil {"type" "string" "minLength" 2 "maxLength" 4}) "f"))))
    (testing "const"
      (is (s/valid? (string-schema->spec nil {"type" "string" "const" "baz"}) "baz"))
      (is (not (s/valid? (string-schema->spec nil {"type" "string" "const" "baz"}) "foo"))))
    (testing "enum"
      (is (s/valid? (string-schema->spec nil {"type" "string" "enum" ["foo" "baz"]}) "baz"))
      (is (s/valid? (string-schema->spec nil {"type" "string" "enum" ["foo" "baz"]}) "foo"))
      (is (not (s/valid? (string-schema->spec nil {"type" "string" "enum" ["foo" "baz"]}) "bar"))))
    (testing "pattern"
      (is (s/valid? (string-schema->spec nil {"type" "string" "pattern" "^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$"}) "555-1212"))
      (is (s/valid? (string-schema->spec nil {"type" "string" "pattern" "^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$"}) "(888)555-1212"))
      (is (not (s/valid? (string-schema->spec nil {"type" "string" "pattern" "^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$"}) "(888)555-1212 ext. 532")))
      (is (not (s/valid? (string-schema->spec nil {"type" "string" "pattern" "^(\\([0-9]{3}\\))?[0-9]{3}-[0-9]{4}$"}) "(800)FLOWERS"))))))

(deftest array-schema->spec-test
  (testing "creation of spec for various JSON schemas describing an Array instance"
    (testing "helper fns"
      (testing "`item-pred` arity of `single-item->spec`"
        (is (s/valid?
             ;; at min limit of 1
             (single-item->spec string? 3 1 true) ["foo"]))
        (is (s/valid?
             ;; over min limit, bellow max limit
             (single-item->spec string? 3 1 true) ["foo" "baz"]))
        (is (s/valid?
             ;; over min limit, bellow max limit, distinct = false = nil
             (single-item->spec string? 3 1 false) ["foo" "foo"]))
        (is (s/valid?
             ;; at max limit of 3
             (single-item->spec string? 3 1 true) ["foo" "baz" "bar"]))
        (is (s/valid?
             ;; at max limit of 3, distinct = false = nil
             (single-item->spec string? 3 1 nil) ["foo" "foo" "foo"]))
        (is (s/valid?
             ;; at max limit of 3, distinct = false = nil
             (single-item->spec string? 3 1 nil) ["foo" "foo" "baz"]))
        (is (not
             ;; over max of 3
             (s/valid? (single-item->spec string? 3 1 true) ["foo" "baz" "bar" "buzz"])))
        (is (not
             ;; breaks distinct constraint
             (s/valid? (single-item->spec string? 3 1 true) ["foo" "baz" "baz"]))))
      (testing "`item` arity of `single-item->spec`"
        (is (s/valid?
             ;; at min limit of 1 + of right type
             (single-item->spec {"type" "integer"} 2 1 true nil) [1]))
        (is (s/valid?
             ;; at max limit of 2 + of right type + distinct
             (single-item->spec {"type" "integer"} 2 1 true nil) [1 2]))
        (is (s/valid?
             ;; at max limit of 2 + of right type
             (single-item->spec {"type" "integer"} 2 1 nil nil) [1 1]))
        (is (not
             ;; right type but over max
             (s/valid? (single-item->spec {"type" "integer"} 2 1 nil nil) [1 1 1])))
        (is (not
             ;; at max limit of 2 + distinct but not right type
             (s/valid? (single-item->spec {"type" "integer"} 2 1 true nil) [1 2.0])))
        (is (not
             ;; at max limit of 2 but not right type
             (s/valid? (single-item->spec {"type" "integer"} 2 1 false nil) [2.0 2.0]))))
      (testing "positional specs from `many-items->spec`"
        (is (s/valid? (many-items->spec [::jschema/string neg? (schema->spec nil {"type" "integer"})] nil nil true)
                      ;; distinct and each item follows positional spec
                      ["foo" -1 1]))
        (is (s/valid? (many-items->spec [::jschema/string neg? neg?] nil nil false)
                      ;; each item follows positional spec, distinct set to false
                      ["foo" -1 -1]))
        (is (s/valid? (many-items->spec [::jschema/string neg? neg?] nil nil nil)
                      ;; each item follows positional spec, distinct not specified
                      ["foo" -1 -1]))
        (is (not
             ;; items should be unique
             (s/valid? (many-items->spec [::jschema/string neg? (schema->spec nil {"type" "integer"})] nil nil true)
                       ["foo" -1 -1])))
        (is (not
             ;; first item should be a string
             (s/valid? (many-items->spec [::jschema/string neg? (schema->spec nil {"type" "integer"})] nil nil true)
                       [true -1 1])))
        (is (not
             ;; second item should be negative
             (s/valid? (many-items->spec [::jschema/string neg? (schema->spec nil {"type" "integer"})] nil nil true)
                       ["foo" 1 1])))
        (is (not
             ;; last item should be an int
             (s/valid? (many-items->spec [::jschema/string neg? (schema->spec nil {"type" "integer"})] nil nil true)
                       ["foo" -1 1.0])))
        (is (not
             ;; extra item in the vector
             (s/valid? (many-items->spec [::jschema/string neg? (schema->spec nil {"type" "integer"})] nil nil true)
                       ["foo" -1 1 "invalid-extra"])))
        (is (not
             ;; missing third item
             (s/valid? (many-items->spec [::jschema/string neg? (schema->spec nil {"type" "integer"})] nil nil true)
                       ["missing one of the things thing" -1])))
        (is (not
             ;; missing second and third item
             (s/valid? (many-items->spec [::jschema/string neg? (schema->spec nil {"type" "integer"})] nil nil true)
                       ["missing all other things"])))))
    (testing "main fn"
      (let [simplish-schema {"type" "array"
                             "minItems" 1
                             "maxItems" 2
                             "uniqueItems" true
                             "items" {"type" "integer"}}
            pos-schema-uniq {"type" "array"
                             "uniqueItems" true
                             "items" [{"type" "integer"} {"type" "integer"}]}
            pos-schema      {"type" "array"
                             "items" [{"type" "integer"} {"type" "integer"}]}
            ;; `rng` only comes into play when "type" is array valued, ignoring this case for now
            rng             nil
            ;; fixtures to test against
            ;; -> `pos-schema`
            pos-schema-valid                 [1 1]
            pos-schema-invalid-too-many      [1 1 1]
            pos-schema-invalid-too-few       [1]
            pos-schema-invalid-type          [1 1.0]
            ;; -> `pos-schema-uniq`
            pos-schema-uniq-valid            [1 2]
            pos-schema-uniq-invalid-distinct [1 1]
            pos-schema-uniq-invalid-too-many [1 2 3]
            pos-schema-uniq-invalid-too-few  [1]
            pos-schema-uniq-invalid-type     [1.0 2]
            ;; -> `simplish-schema`
            simplish-schema-valid            [1 2]
            simplish-schema-invalid-distinct [1 1]
            simplish-schema-invalid-too-many [1 2 3]
            simplish-schema-invalid-type     [1 2.0]]
        (testing "`pos-schema`"
          (is (s/valid? (array-schema->spec rng pos-schema) pos-schema-valid))
          (is (not (s/valid? (array-schema->spec rng pos-schema) pos-schema-invalid-too-many)))
          (is (not (s/valid? (array-schema->spec rng pos-schema) pos-schema-invalid-too-few)))
          (is (not (s/valid? (array-schema->spec rng pos-schema) pos-schema-invalid-type))))
        (testing "`pos-schema-uniq`"
          (is (s/valid? (array-schema->spec rng pos-schema-uniq) pos-schema-uniq-valid))
          (is (not (s/valid? (array-schema->spec rng pos-schema-uniq) pos-schema-uniq-invalid-distinct)))
          (is (not (s/valid? (array-schema->spec rng pos-schema-uniq) pos-schema-uniq-invalid-too-many)))
          (is (not (s/valid? (array-schema->spec rng pos-schema-uniq) pos-schema-uniq-invalid-too-few)))
          (is (not (s/valid? (array-schema->spec rng pos-schema-uniq) pos-schema-uniq-invalid-type))))
        (testing "`simplish-schema`"
          (is (s/valid? (array-schema->spec rng simplish-schema) simplish-schema-valid))
          (is (not (s/valid? (array-schema->spec rng simplish-schema) simplish-schema-invalid-distinct)))
          (is (not (s/valid? (array-schema->spec rng simplish-schema) simplish-schema-invalid-too-many)))
          (is (not (s/valid? (array-schema->spec rng simplish-schema) simplish-schema-invalid-type))))))))

(deftest property-schema->spec-fn-test
  (testing "spec based validation fn for an Object Instance based on kv pairs found in `properties`"
    (let [property-spec (property-schema->spec-fn
                         {"number"      {"type" "number"}
                          "street_name" {"type" "string"}
                          "street_type" {"type" "string"
                                         "enum" ["Street" "Avenue" "Boulevard"]}}
                         nil)]
      (is (s/valid? property-spec      {"number" 1600
                                        "street_name" "Pennsylvania"
                                        "street_type" "Avenue"}))
      (is (s/valid? property-spec      {"number" 1600
                                        "street_name" "Pennsylvania"}))
      (is (s/valid? property-spec      {"number" 1600
                                        "street_name" "Pennsylvania"
                                        "street_type" "Avenue"
                                        "direction" "NW"}))
      (is (s/valid? property-spec      {}))
      (is (not (s/valid? property-spec {"number" "1600"
                                        "street_name" "Pennsylvania"
                                        "street_type" "Avenue"}))))))

(deftest object-schema->spec-test
  (testing "creation of validation spec from Object Instance JSON Schema"
    (testing "`?enum`"
      (is (s/valid? (object-schema->spec nil {"type" "object"
                                              "enum" [{"foo" "baz"} {"foo" "bar"}]})
                    {"foo" "baz"}))
      (is (s/valid? (object-schema->spec nil {"type" "object"
                                              "enum" [{"foo" "baz"} {"foo" "bar"}]})
                    {"foo" "bar"}))
      (is (not (s/valid? (object-schema->spec nil {"type" "object"
                                                   "enum" [{"foo" "baz"} {"foo" "bar"}]})
                         {"foo" "buz"}))))
    (testing "`?const`"
      (is (s/valid? (object-schema->spec nil {"type" "object"
                                              "const" {"foo" "baz"}})
                    {"foo" "baz"}))
      (is (not (s/valid? (object-schema->spec nil {"type" "object"
                                                   "const" {"foo" "baz"}})
                         {"foo" "buz"}))))
    (testing "`?maxProperties`"
      (is (s/valid? (object-schema->spec nil {"type" "object"
                                              "maxProperties" 2})
                    {"foo" "baz"
                     "buz" "bar"}))
      (is (not (s/valid? (object-schema->spec nil {"type" "object"
                                                   "maxProperties" 2})
                         {"foo" "baz"
                          "buz" "bar"
                          "bar" "qux"}))))
    (testing "`?minProperties`"
      (is (s/valid? (object-schema->spec nil {"type" "object"
                                              "minProperties" 2})
                    {"foo" "baz"
                     "buz" "bar"}))
      (is (s/valid? (object-schema->spec nil {"type" "object"
                                              "minProperties" 2})
                    {"foo" "baz"
                     "buz" "bar"
                     "bar" "qux"}))
      (is (not (s/valid? (object-schema->spec nil {"type" "object"
                                                   "minProperties" 2})
                         {"foo" "baz"}))))
    (testing "`?required`"
      (is (s/valid? (object-schema->spec nil {"type" "object"
                                              "required" ["foo" "buz"]})
                    {"foo" "baz"
                     "buz" "bar"}))
      (is (s/valid? (object-schema->spec nil {"type" "object"
                                              "required" ["foo" "buz"]})
                    {"foo" "baz"
                     "buz" "bar"
                     "bar" "qux"}))
      (is (not (s/valid? (object-schema->spec nil {"type" "object"
                                                   "required" ["foo" "buz"]})
                         {"oof" "baz"
                          "buz" "bar"})))
      (is (not (s/valid? (object-schema->spec nil {"type" "object"
                                                   "required" ["foo" "buz"]})
                         {"foo" "baz"
                          "bzz" "bar"})))
      (is (not (s/valid? (object-schema->spec nil {"type" "object"
                                                   "required" ["foo" "buz"]})
                         {"foo" "baz"})))
      (is (not (s/valid? (object-schema->spec nil {"type" "object"
                                                   "required" ["foo" "buz"]})
                         {"buz" "baz"})))
      (is (not (s/valid? (object-schema->spec nil {"type" "object"
                                                   "required" ["foo" "buz"]})
                         {}))))
    (testing "`?properties`"
      (is (s/valid? (object-schema->spec nil {"type" "object"
                                              "properties" {"number"      {"type" "number"}
                                                            "street_name" {"type" "string"}
                                                            "street_type" {"type" "string"
                                                                           "enum" ["Street" "Avenue" "Boulevard"]}}})
                    {"number" 1600
                     "street_name" "Pennsylvania"
                     "street_type" "Avenue"}))
      (is (s/valid? (object-schema->spec nil {"type" "object"
                                              "properties" {"number"      {"type" "number"}
                                                            "street_name" {"type" "string"}
                                                            "street_type" {"type" "string"
                                                                           "enum" ["Street" "Avenue" "Boulevard"]}}})
                    {"number" 1600
                     "street_name" "Pennsylvania"}))
      (is (s/valid? (object-schema->spec nil {"type" "object"
                                              "properties" {"number"      {"type" "number"}
                                                            "street_name" {"type" "string"}
                                                            "street_type" {"type" "string"
                                                                           "enum" ["Street" "Avenue" "Boulevard"]}}})
                    {"number" 1600
                     "street_name" "Pennsylvania"
                     "street_type" "Avenue"
                     "direction" "NW"}))
      (is (s/valid? (object-schema->spec nil {"type" "object"
                                              "properties" {"number"      {"type" "number"}
                                                            "street_name" {"type" "string"}
                                                            "street_type" {"type" "string"
                                                                           "enum" ["Street" "Avenue" "Boulevard"]}}})
                    {}))
      (is (not (s/valid? (object-schema->spec nil {"type" "object"
                                                   "properties" {"number"      {"type" "number"}
                                                                 "street_name" {"type" "string"}
                                                                 "street_type" {"type" "string"
                                                                                "enum" ["Street" "Avenue" "Boulevard"]}}})
                         {"number" "1600"
                          "street_name" "Pennsylvania"
                          "street_type" "Avenue"}))))))
