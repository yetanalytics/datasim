(ns com.yetanalytics.datasim.xapi.statement.location.inference
  (:require [com.yetanalytics.datasim.xapi.statement.location.determining-property
             :refer [path-to-determining-property?]]
            [com.yetanalytics.datasim.xapi.statement.location.nav-by-expectation
             :as nav]))

(defn path-to-non-determining-property?
  "determining properties are static given the spec so everything else
   must not be a determining property"
  [stmt-path]
  (not (path-to-determining-property? stmt-path)))

(comment
  ;; this is why `nav/step-back-expected?` exists.
  (path-to-non-determining-property? ["foo" "baz"]))

(defn path-terminate-at-array?
  "does the `stmt-path` indicate the target will be an array?"
  [stmt-path]
  (let [terminate-at (peek stmt-path)]
    (case terminate-at
      "correctResponsePattern" true
      "attachments"            true
      "category"               true
      "grouping"               true
      "member"                 true
      "parent"                 true
      "other"                  true
      "choices"                true
      "scale"                  true
      "source"                 true
      "target"                 true
      "steps"                  true
      false)))

(comment
  (= true
     (path-terminate-at-array? ["foo" "baz" "other"])
     (not (path-terminate-at-array? ["foo" "baz" "verb" "id"]))))
