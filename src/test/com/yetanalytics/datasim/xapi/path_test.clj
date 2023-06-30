(ns com.yetanalytics.datasim.xapi.path-test
  (:require [clojure.test       :refer [deftest testing is are]]
            [clojure.java.io    :as io]
            [clojure.spec.alpha :as s]
            [cheshire.core      :as json]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.datasim.xapi.path      :as path]
            [com.yetanalytics.datasim.test-constants :as const]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def long-statement
  (with-open
   [r (io/reader const/long-statement-filepath)]
    (json/parse-stream r)))

;; Taken from the xAPI spec example
(def sample-attachments
  [{"usageType"   "http://adlnet.gov/expapi/attachments/signature"
    "display"     {"en-us" "Signature"}
    "description" {"en-us" "A test signature"}
    "contentType" "application/octet-stream"
    "length"      4235
    "sha2"        "672fa5fa658017f1b72d65036f13379c6ab05d4ab3b6664908d8acf0b6a0c634"}])

(def object-types
  {["object"] #{"activity"}
   ["context" "instructor"] #{"agent"}
   ["actor"] #{"group"}
   ["authority"] #{"agent"}})

;; See: https://dnaeon.github.io/clojure-map-ks-paths/
(defn- json->path-map
  [json-value]
  (letfn [(children [node]
            (let [v (get-in json-value node)]
              (cond
                (map? v)  (map (fn [k] (conj node k)) (keys v))
                (coll? v) (map (fn [i] (conj node i)) (range (count v)))
                :else     [])))
          (branch? [node]
            (-> node children seq boolean))]
    (->> (cond
           (map? json-value) (keys json-value)
           (coll? json-value) (range (count json-value)))
         (map vector)
         (mapcat (partial tree-seq branch? children))
         (reduce (fn [m path]
                   (assoc m
                          (mapv #(if (int? %) '* %) path)
                          (get-in json-value path)))
                 {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path to Spec Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tests are chosen to ensure nearly-complete test coverage of
;; path-spec multimethod

(deftest path->spec-test
  (testing "works for lots of paths"
    ;; Explode a statement using a helper from our zipperoo to get a bunch of
    ;; paths and leaf values
    (is (every?
         (fn [[path v]]
           (let [spec (path/path->spec ::xs/statement
                                       path
                                       object-types)]
             (and spec
                  (s/valid? spec v))))
         (json->path-map long-statement)))
    (is (every?
         (fn [[path v]]
           (let [spec (path/path->spec :statement/attachments
                                       path
                                       object-types)]
             (and spec
                  (s/valid? spec v))))
         (json->path-map sample-attachments)))
    (is (every?
         (fn [[path v]]
           (let [spec (path/path->spec ::xs/sub-statement
                                       (into path)
                                       object-types)]
             (and spec
                  (s/valid? spec v))))
         (json->path-map
          (-> long-statement
              (assoc "objectType" "SubStatement"
                     "attachments" sample-attachments)
              (dissoc "id" "stored" "version" "authority"))))))
  (testing "works for arbitrary and relative paths"
    (is (= :score/max
           (path/path->spec ::xs/result
                            ["score" "max"]
                            {})))
    (is (= :activity/id
           (path/path->spec ::xs/context
                            ["contextActivities" "grouping" '* "id"]
                            {})))
    (is (= ::xs/account
           (path/path->spec ::xs/group
                            ["account"]
                            {})))
    (is (= :account/homePage
           (path/path->spec ::xs/group
                            ["account" "homePage"]
                            {})))
    (is (= ::xs/interaction-component
           (path/path->spec :activity/definition
                            ["choices" '*]
                            {})))
    (is (= ::xs/interaction-component
           (path/path->spec :definition/scale
                            ['*]
                            {})))
    (is (= :interaction-component/id
           (path/path->spec :definition/source
                            ['* "id"]
                            {})))
    (is (= ::xs/language-map
           (path/path->spec :definition/target
                            ['* "description"]
                            {})))
    (is (= ::xs/language-map-text
           (path/path->spec :definition/steps
                            ['* "description" "en-US"]
                            {}))))
  (testing "can return custom specs"
    (is (= :actor/objectType
           (path/path->spec ::xs/statement
                            ["actor" "objectType"]
                            {["actor"] #{"agent" "group"}})))
    (is (= :correctResponsesPattern/string
           (path/path->spec ::xs/statement
                            ["object" "definition" "correctResponsesPattern" '*]
                            {["object"] #{"activity"}}))))
  (testing "can work with different object types"
    ;; Actors
    (is (= :agent/name
           (path/path->spec ::xs/statement
                            ["actor" "name"]
                            {["actor"] #{"agent"}})))
    (is (= :agent/name
           (path/path->spec ::xs/statement
                            ["actor" "name"]
                            {["actor"] #{"agent" "group"}})))
    (is (= :group/name
           (path/path->spec ::xs/statement
                            ["actor" "name"]
                            {["actor"] #{"group"}})))
    (is (= :group/member ; shouldn't happen in actual use...
           (path/path->spec ::xs/statement
                            ["actor" "member"]
                            {["actor"] #{"agent" "group"}})))
    ;; Statement objects
    (is (= :agent/name
           (path/path->spec ::xs/statement
                            ["object" "name"]
                            {["object"] #{"agent" "group"}})))
    (is (= :agent/name
           (path/path->spec ::xs/statement
                            ["object" "name"]
                            {["object"] #{"agent"}})))
    (is (= :group/name
           (path/path->spec ::xs/statement
                            ["object" "name"]
                            {["object"] #{"group"}})))
    (is (= :statement-ref/id
           (path/path->spec ::xs/statement
                            ["object" "id"]
                            {["object"] #{"statement-ref"}})))
    (is (= ::xs/verb
           (path/path->spec ::xs/statement
                            ["object" "verb"]
                            {["object"] #{"sub-statement"}})))
    (is (= :activity/id
           (path/path->spec ::xs/statement
                            ["object" "id"]
                            {["object"] #{"activity" "statement-ref"}})))
    ;; Sub-statement objects
    (is (= :agent/name
           (path/path->spec ::xs/statement
                            ["object" "object" "name"]
                            {["object"] #{"sub-statement"}
                             ["object" "object"] #{"agent" "group"}})))
    (is (= :agent/name
           (path/path->spec ::xs/statement
                            ["object" "object" "name"]
                            {["object"] #{"sub-statement"}
                             ["object" "object"] #{"agent"}})))
    (is (= :group/name
           (path/path->spec ::xs/statement
                            ["object" "object" "name"]
                            {["object"] #{"sub-statement"}
                             ["object" "object"] #{"group"}})))
    (is (= :activity/id
           (path/path->spec ::xs/statement
                            ["object" "object" "id"]
                            {["object"] #{"sub-statement"}
                             ["object" "object"] #{"activity"}})))
    (is (= :statement-ref/id
           (path/path->spec ::xs/statement
                            ["object" "object" "id"]
                            {["object"] #{"sub-statement"}
                             ["object" "object"] #{"statement-ref"}})))
    (is (= :activity/id
           (path/path->spec ::xs/statement
                            ["object" "object" "id"]
                            {["object"] #{"sub-statement"}
                             ["object" "object"] #{"activity" "statement-ref"}})))
    ;; Bad type combos
    (is (= ::path/unsuppored-object-types
           (try (path/path->spec ::xs/statement
                                 ["object" "id"]
                                 {["object"] #{"sub-statement" "statement-ref"}})
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::path/unsuppored-object-types
           (try (path/path->spec ::xs/statement
                                 ["object" "object" "id"]
                                 {["object"] #{"sub-statement"}
                                  ["object" "object"] #{"sub-statement"}})
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::path/unsuppored-object-types
           (try (path/path->spec ::xs/statement
                                 ["actor" "id"]
                                 {["actor"] #{"sub-statement"}})
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::path/unsuppored-object-types
           (try (path/path->spec ::xs/statement
                                 ["object"]
                                 {["object"] #{"sub-statement" "statement-ref"}})
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::path/unsuppored-object-types
           (try (path/path->spec ::xs/statement
                                 ["object" "object"]
                                 {["object"] #{"sub-statement"}
                                  ["object" "object"] #{"sub-statement"}})
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::path/unsuppored-object-types
           (try (path/path->spec ::xs/statement
                                 ["actor"]
                                 {["actor"] #{"sub-statement"}})
                (catch Exception e (-> e ex-data :type))))))
  (testing "bad paths or specs"
    (is (= ::path/invalid-spec
           (try (path/path->spec ::xs/statement
                                 ["actor" "badSpec"]
                                 {["actor"] #{"agent"}})
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::path/unknown-path-spec
           (try (path/path->spec ::xs/statement
                                 ["actor" "zoo" "wee" "mama"]
                                 {["actor"] #{"agent"}})
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::path/invalid-path-map-key
           (try (path/path->spec ::xs/statement
                                 ["object" '*]
                                 {["object"] #{"activity"}})
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::path/invalid-path-array-key
           (try (path/path->spec ::xs/statement
                                 ["context" "contextActivities" "grouping" "foo"]
                                 {})
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::path/invalid-path-spec-key
           (try (path/path->spec ::xs/statement
                                 ["verb" "display" '*]
                                 {})
                (catch Exception e (-> e ex-data :type))))))
  (testing "extensions get special treatment"
    (is (= :com.yetanalytics.datasim.json/any
           (path/path->spec
            :statement/context
            ["extensions" "http://foo.org/extension"]
            {})))
    (is (= :com.yetanalytics.datasim.json/any
           (path/path->spec
            :statement/context
            ["extensions" "http://foo.org/extension"]
            {}))))
  (testing "are correct for paths that have valuesets"
    (are [spec path] (path/path->spec ::xs/statement
                                      path
                                      {["object"] #{"activity"}})
      ::xs/verb        ["verb"]
      :verb/id         ["verb" "id"]
      ::xs/activity    ["object"]
      ::xs/activity    ["context" "contextActivities" "category" '*]
      ::xs/activity    ["context" "contextActivities" "grouping" '*]
      ::xs/activity    ["context" "contextActivities" "parent" '*]
      ::xs/activity    ["context" "contextActivities" "other" '*]
      :activity/id     ["object" "id"]
      :activity/id     ["context" "contextActivities" "category" '* "id"]
      :activity/id     ["context" "contextActivities" "grouping" '* "id"]
      :activity/id     ["context" "contextActivities" "parent" '* "id"]
      :activity/id     ["context" "contextActivities" "other" '* "id"]
      :definition/type ["object" "definition" "type"]
      :definition/type ["context" "contextActivities" "category" '* "definition" "type"]
      :definition/type ["context" "contextActivities" "grouping" '* "definition" "type"]
      :definition/type ["context" "contextActivities" "parent" '* "definition" "type"]
      :definition/type ["context" "contextActivities" "other" '* "definition" "type"])
    (are [spec path] (path/path->spec ::xs/statement
                                      path
                                      {["object"] #{"sub-statement"}
                                       ["object" "object"] #{"activity"}})
      ::xs/verb        ["object" "verb"]
      :verb/id         ["object" "verb" "id"]
      ::xs/activity    ["object" "object"]
      ::xs/activity    ["object" "context" "contextActivities" "category" '*]
      ::xs/activity    ["object" "context" "contextActivities" "grouping" '*]
      ::xs/activity    ["object" "context" "contextActivities" "parent" '*]
      ::xs/activity    ["object" "context" "contextActivities" "other" '*]
      :activity/id     ["object" "object" "id"]
      :activity/id     ["object" "context" "contextActivities" "category" '* "id"]
      :activity/id     ["object" "context" "contextActivities" "grouping" '* "id"]
      :activity/id     ["object" "context" "contextActivities" "parent" '* "id"]
      :activity/id     ["object" "context" "contextActivities" "other" '* "id"]
      :definition/type ["object" "object" "definition" "type"]
      :definition/type ["object" "context" "contextActivities" "category" '* "definition" "type"]
      :definition/type ["object" "context" "contextActivities" "grouping" '* "definition" "type"]
      :definition/type ["object" "context" "contextActivities" "parent" '* "definition" "type"]
      :definition/type ["object" "context" "contextActivities" "other" '* "definition" "type"])))
