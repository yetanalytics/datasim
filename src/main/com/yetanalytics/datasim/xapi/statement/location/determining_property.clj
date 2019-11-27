(ns com.yetanalytics.datasim.xapi.statement.location.determining-property
  (:require [com.yetanalytics.datasim.xapi.statement.location.nav-by-expectation :as nav]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; static paths to determining properties
;; - `:placeholder` is a stand in for some array value within the src json-path
;; -- see `nav/handle-placeholder` for more info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def verb-iri-stmt-path
  ["verb" "id"])

(def obj-at-stmt-path
  ["object" "definition" "type"])

(def attach-ut-stmt-path
  ["attachments" :placeholder "usageType"])

(def grouping-at-stmt-path
  ["context" "contextActivities" "grouping" :placeholder "definition" "type"])

(def parent-at-stmt-path
  ["context" "contextActivities" "parent" :placeholder "definition" "type"])

(def other-at-stmt-path
  ["context" "contextActivities" "other" :placeholder "definition" "type"])

(def category-at-stmt-path
  ["context" "contextActivities" "category" :placeholder "definition" "type"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scalar determining properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- path-to-scalar-determining-property?
  "fn which determines if path is going to a
   determining property that is NOT array valued."
  [stmt-path]
  (let [terminate-at (peek stmt-path)]
    (case terminate-at
      "id"   (nav/step-back-expected? "verb" stmt-path)
      "type" (if-let [stmt-path' (nav/step-back-expected?
                                  "definition"
                                  stmt-path
                                  :return-poped? true
                                  :expected-pop ["object" "definition"])]
               (nav/step-back-expected? "object" stmt-path')
               false)
      false)))

(comment
  (= true
     ;; scalars
     (path-to-scalar-determining-property?
      verb-iri-stmt-path)
     (path-to-scalar-determining-property?
      obj-at-stmt-path)
     ;; vectors
     (not (path-to-scalar-determining-property?
           attach-ut-stmt-path))
     (not (path-to-scalar-determining-property?
           grouping-at-stmt-path))
     (not (path-to-scalar-determining-property?
           parent-at-stmt-path))
     (not (path-to-scalar-determining-property?
           other-at-stmt-path))
     (not (path-to-scalar-determining-property?
           category-at-stmt-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vector determining properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- path-to-vector-determining-property?
  [stmt-path]
  (let [terminate-at (peek stmt-path)]
    (case terminate-at
      "usageType" (nav/step-back-expected? :placeholder stmt-path :next-key "attachments")
      "type"      (if-let [in-array (nav/step-back-expected?
                                     "definition"
                                     stmt-path
                                     :next-key :placeholder
                                     :return-poped? true)]
                    (if-let [at-ctx-a (or (nav/step-back-expected?
                                           :placeholder in-array
                                           :next-key "parent"
                                           :return-poped? true)
                                          (nav/step-back-expected?
                                           :placeholder in-array
                                           :next-key "grouping"
                                           :return-poped? true)
                                          (nav/step-back-expected?
                                           :placeholder in-array
                                           :next-key "other"
                                           :return-poped? true)
                                          (nav/step-back-expected?
                                           :placeholder in-array
                                           :next-key "category"
                                           :return-poped? true))]
                      (or (nav/step-back-expected?
                           "category" at-ctx-a
                           :expected-pop ["context" "contextActivities" "category"])
                          (nav/step-back-expected?
                           "grouping" at-ctx-a
                           :expected-pop ["context" "contextActivities" "grouping"])
                          (nav/step-back-expected?
                           "other" at-ctx-a
                           :expected-pop ["context" "contextActivities" "other"])
                          (nav/step-back-expected?
                           "parent" at-ctx-a
                           :expected-pop ["context" "contextActivities" "parent"]))
                      false)
                    false)
      false)))

(comment
  (= true
     ;; scalars
     (not (path-to-vector-determining-property?
           verb-iri-stmt-path))
     (not (path-to-vector-determining-property?
           obj-at-stmt-path))
     ;; vectors
     (path-to-vector-determining-property?
      attach-ut-stmt-path)
     (path-to-vector-determining-property?
      grouping-at-stmt-path)
     (path-to-vector-determining-property?
      parent-at-stmt-path)
     (path-to-vector-determining-property?
      other-at-stmt-path)
     (path-to-vector-determining-property?
      category-at-stmt-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exposed fn which will check for both scalar and array before returning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn path-to-determining-property?
  "does `stmt-path` point to a determining property?
    - Verb id
    - Object Activity Type
    - Attachment Usage Type
    - Parent Activity Type
    - Grouping Activity Type
    - Category Activity Type
    - Other Activity Type"
  [stmt-path]
  (or (path-to-scalar-determining-property? stmt-path)
      (path-to-vector-determining-property? stmt-path)))
