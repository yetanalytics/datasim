(ns com.yetanalytics.datasim.json.path-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.json.path :refer :all]
            [blancas.kern.core :as k]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.test.check :as tc]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

(def long-statement
  (with-open
    [r (io/reader (io/resource "xapi/statements/long.json"))]
    (json/read r)))

(deftest parse-test
  (are [path v] (= v
                   (parse path))
    "$.store.book[*].author" [#{"store"} #{"book"} '* #{"author"}]
    "$..author"              ['* #{"author"}]
    "$.store.*"              [#{"store"} '*]
    "$.store..price"         [#{"store"} '* #{"price"}]
    "$..book[2]"             ['* #{"book"} #{2}]
    "$..book[-1:]"           ['* #{"book"} (->RangeSpec -1 9223372036854775807 1)]
    "$..book[0,1]"           ['* #{"book"} #{0 1}]
    "$..book[:2]"            ['* #{"book"} (->RangeSpec 0 2 1)]
    "$..*"                   '[* *]
    ;; selections from cmi5
    "$.context.contextActivities.grouping[*]"
    [#{"context"} #{"contextActivities"} #{"grouping"} '*]

    "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/sessionid']"
    [#{"context"} #{"extensions"} #{"https://w3id.org/xapi/cmi5/context/extensions/sessionid"}]
    ))

(deftest satisfied-test
  (let [json-path [#{"foo"} #{"bar"} '* #{"quxx"} #{0 1}]
        key-path ["foo" "bar" "baz" "quxx" 0]]
    (testing "when json-path and key path match"
      (testing "returns the json path"
        (is (= json-path (satisfied json-path key-path)))))
    (testing "when json-path and key path match partially"
      (testing "returns the json path"
        (is (= (take 3 json-path) (take 3 (satisfied json-path key-path))))))
    (testing "when json-path and key path diverge"
      (testing "returns nil"
        (is (nil? (satisfied json-path (assoc key-path 3 "blork"))))))))

(deftest select-deep-test
  (are [path selection]
      (= (select-deep long-statement (parse path))
         selection)
    "$.id" {"id" "6690e6c9-3ef0-4ed3-8b37-7f3964730bee"}
    "$.timestamp" {"timestamp" "2013-05-18T05:32:34.804Z"}

    "$.context.contextActivities.grouping[*]"
    {"context" {"contextActivities" {}}}

    "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/sessionid']"
    {"context" {}}

    "$.result.score"
    {"result" {}}

    "$.result.success"
    {"result" {"success" true}}

    "$.result.completion"
    {"result" {"completion" true}}

    "$.context.contextActivities.category[*].id"
    {"context"
     {"contextActivities"
      {"category"
       [{"id" "http://www.example.com/meetings/categories/teammeeting"}]}}}

    "$.context.contextActivities.other[*].id"
    {"context"
     {"contextActivities"
      {"other"
       [{"id" "http://www.example.com/meetings/occurances/34257"}
        {"id" "http://www.example.com/meetings/occurances/3425567"}]}}}

    "$.result.duration"
    {"result" {"duration" "PT1H0M0S"}}

    "$.result.extensions['http://example.com/profiles/meetings/resultextensions/minuteslocation']"
    {"result"
     {"extensions"
      {"http://example.com/profiles/meetings/resultextensions/minuteslocation"
       "X:\\meetings\\minutes\\examplemeeting.one"}}}

    "$.object.definition.type"
    {"object"
     {"definition" {"type" "http://adlnet.gov/expapi/activities/meeting"}}}))


(comment
  (select long-statement (parse "$.id"))
  (clojure.pprint/pprint long-statement
                         )


  )

(deftest select-test
  (are [path selection]
      (= (select long-statement (parse path))
         selection)
    "$.id" ["6690e6c9-3ef0-4ed3-8b37-7f3964730bee"]
    "$.timestamp" ["2013-05-18T05:32:34.804Z"]

    "$.result.success"
    [true]

    "$.result.completion"
    [true]

    "$.context.contextActivities.category[*].id"
    ["http://www.example.com/meetings/categories/teammeeting"]

    "$.context.contextActivities.other[*].id"
    ["http://www.example.com/meetings/occurances/34257"
     "http://www.example.com/meetings/occurances/3425567"]

    "$.result.duration"
    ["PT1H0M0S"]

    "$.result.extensions['http://example.com/profiles/meetings/resultextensions/minuteslocation']"
    ["X:\\meetings\\minutes\\examplemeeting.one"]

    "$.object.definition.type"
    ["http://adlnet.gov/expapi/activities/meeting"]

    ))

(deftest excise-test
  (are [path s-after]
      (= (excise long-statement (parse path)
                 :prune-empty? true)
         s-after)
    "$.id" (dissoc long-statement "id")
    "$.timestamp" (dissoc long-statement "timestamp")

    "$.result.success"
    (update long-statement "result" dissoc "success")

    "$.result.completion"
    (update long-statement "result" dissoc "completion")

    "$.context.contextActivities.category[*].id"
    (update-in long-statement ["context"
                               "contextActivities"
                               "category"]
               (partial mapv #(dissoc % "id")))

    "$.context.contextActivities.other[*].id"
    (update-in long-statement ["context"
                               "contextActivities"
                               "other"]
               (partial mapv #(dissoc % "id")))
    "$.context.contextActivities.other[*]"
    (update-in long-statement ["context"
                               "contextActivities"
                               ]
               dissoc "other")

    "$.result.duration"
    (update long-statement "result" dissoc "duration")

    "$.result.extensions['http://example.com/profiles/meetings/resultextensions/minuteslocation']"
    (update  long-statement "result" dissoc "extensions")

    "$.object.definition.type"
    (update-in  long-statement ["object" "definition"] dissoc "type")))


(deftest enumerate-test
  (are [path result-count]
      (= result-count
         (count (enumerate path)))
    [#{"store"} #{"book"} '* #{"author"}]                  10
    ['* #{"author"}]                                       10
    [#{"store"} '*]                                        10
    [#{"store"} '* #{"price"}]                             10
    ['* #{"book"} #{2}]                                    10
    ['* #{"book"} (->RangeSpec -1 9223372036854775807 1)]  100
    ['* #{"book"} #{0 1}]                                  20
    ['* #{"book"} (->RangeSpec 0 2 1)]                     20
    '[* *]                                                 100
    ;; selections from cmi5
    [#{"context"} #{"contextActivities"} #{"grouping"} '*] 10

    [#{"context"} #{"extensions"} #{"https://w3id.org/xapi/cmi5/context/extensions/sessionid"}] 1
    ))
