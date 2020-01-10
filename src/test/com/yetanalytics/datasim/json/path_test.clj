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
    "$..book[-1:]"           ['* #{"book"} (->RangeSpec -1 Long/MAX_VALUE 1 false)]
    "$..book[0,1]"           ['* #{"book"} #{0 1}]
    "$..book[:2]"            ['* #{"book"} (->RangeSpec 0 2 1 true)]
    "$..*"                   '[* *]
    ;; selections from cmi5
    "$.context.contextActivities.grouping[*]"
    [#{"context"} #{"contextActivities"} #{"grouping"} '*]

    "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/sessionid']"
    [#{"context"} #{"extensions"} #{"https://w3id.org/xapi/cmi5/context/extensions/sessionid"}]
    ))

(deftest select-test
  (are [path selection]
      (= (select long-statement (parse path))
         selection)
    "$.id"        ["6690e6c9-3ef0-4ed3-8b37-7f3964730bee"]
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
      (= s-after
         (excise long-statement (parse path)
                 :prune-empty? true))
    "$.id"        (dissoc long-statement "id")
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
    [#{"store"} #{"book"} '* #{"author"}]                       10
    ['* #{"author"}]                                            10
    [#{"store"} '*]                                             10
    [#{"store"} '* #{"price"}]                                  10
    ['* #{"book"} #{2}]                                         10
    ['* #{"book"} (->RangeSpec -1 9223372036854775807 1 false)] 100
    ['* #{"book"} #{0 1}]                                       20
    ['* #{"book"} (->RangeSpec 0 2 1 true)]                     20
    '[* *]                                                      100
    ;; selections from cmi5
    [#{"context"} #{"contextActivities"} #{"grouping"} '*]      10

    [#{"context"} #{"extensions"} #{"https://w3id.org/xapi/cmi5/context/extensions/sessionid"}] 1
    ))

(deftest apply-values-test
  (are [json parsed-path values expected]
      (= expected
         (apply-values json parsed-path values))

    long-statement [#{"result"} #{"success"}] [false]
    (assoc-in long-statement ["result" "success"] false)))
