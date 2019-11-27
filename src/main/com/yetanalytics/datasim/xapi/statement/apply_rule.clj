(ns com.yetanalytics.datasim.xapi.statement.apply-rule
  (:require [com.yetanalytics.datasim.xapi.statement.location.inference :as inf]))

(defn update-stmt-fn
  ;; FIXME: more than just stub
  [stmt rule-result]
  (let [{str-ks :stmt/path
         v      :stmt/val} rule-result]
    rule-result))
