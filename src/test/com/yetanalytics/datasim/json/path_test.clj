(ns com.yetanalytics.datasim.json.path-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.json.path :refer :all]
            [blancas.kern.core :as k]))

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