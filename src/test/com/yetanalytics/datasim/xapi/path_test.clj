(ns com.yetanalytics.datasim.xapi.path-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [cheshire.core :as json]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.json.zip :as pzip]
            [com.yetanalytics.datasim.xapi.path :as path]
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

(defn- kv-map [statement]
  (->> statement
       pzip/json->path-map
       (reduce-kv (fn [m k v] (assoc m (mapv #(if (int? %) '* %) k) v))
                  {})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path to Spec Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def object-types
  {["object"] #{"activity"}
   ["context" "instructor"] #{"agent"}
   ["actor"] #{"group"}
   ["authority"] #{"agent"}})

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
         (kv-map long-statement)))
    (is (every?
         (fn [[path v]]
           (let [spec (path/path->spec :statement/attachments
                                       path
                                       object-types)]
             (and spec
                  (s/valid? spec v))))
         (kv-map sample-attachments)))
    (is (every?
         (fn [[path v]]
           (let [spec (path/path->spec ::xs/sub-statement
                                       (into path)
                                       object-types)]
             (and spec
                  (s/valid? spec v))))
         (kv-map (-> long-statement
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
            {})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path to Valueset Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def valuesets
  {:verbs          #{{:id   "http://foo.org/verb"
                      :type "Verb"}}
   :verb-ids       "http://foo.org/verb"
   :activities     #{{:id         "http://foo.org/activity"
                      :definition {:type "http://foo.org/activity-type"}}}
   :activity-ids   #{"http://foo.org/activity"}
   :activity-types #{"http://foo.org/activity-type"}})

(defn- path->valueset [p]
  (path/path->valueset {["object"] #{"activity"}}
                       valuesets
                       p))

(defn- path->valueset-sub [p]
  (path/path->valueset {["object"] #{"sub-statement"}
                        ["object" "object"] #{"activity"}}
                       valuesets
                       p))

(deftest valueset-test
  (testing "statement valueset"
    (is (= (:verbs valuesets)
           (path->valueset
            ["verb"])))
    (is (= (:verb-ids valuesets)
           (path->valueset
            ["verb" "id"])))
    (is (= (:activities valuesets)
           (path->valueset
            ["object"])))
    (is (= (:activity-ids valuesets)
           (path->valueset
            ["object" "id"])))
    (is (= (:activity-types valuesets)
           (path->valueset
            ["object" "definition" "type"])))
    (is (= (:activities valuesets)
           (path->valueset
            ["context" "contextActivities" "category" '*])))
    (is (= (:activity-ids valuesets)
           (path->valueset
            ["context" "contextActivities" "category" '* "id"])))
    (is (= (:activity-types valuesets)
           (path->valueset
            ["context" "contextActivities" "category" '* "definition" "type"])))
    (is (= (:activities valuesets)
           (path->valueset
            ["context" "contextActivities" "grouping" '*])))
    (is (= (:activity-ids valuesets)
           (path->valueset
            ["context" "contextActivities" "grouping" '* "id"])))
    (is (= (:activity-types valuesets)
           (path->valueset
            ["context" "contextActivities" "grouping" '* "definition" "type"])))
    (is (= (:activities valuesets)
           (path->valueset
            ["context" "contextActivities" "parent" '*])))
    (is (= (:activity-ids valuesets)
           (path->valueset
            ["context" "contextActivities" "parent" '* "id"])))
    (is (= (:activity-types valuesets)
           (path->valueset
            ["context" "contextActivities" "parent" '* "definition" "type"])))
    (is (= (:activities valuesets)
           (path->valueset
            ["context" "contextActivities" "other" '*])))
    (is (= (:activity-ids valuesets)
           (path->valueset
            ["context" "contextActivities" "other" '* "id"])))
    (is (= (:activity-types valuesets)
           (path->valueset
            ["context" "contextActivities" "other" '* "definition" "type"])))
    ;; These paths do not return valuesets
    (is (nil? (path->valueset ["actor"])))
    (is (nil? (path->valueset ["context" "contextActivities" "category"])))
    (is (nil? (path->valueset ["context" "contextActivities" "grouping"])))
    (is (nil? (path->valueset ["context" "contextActivities" "parent"])))
    (is (nil? (path->valueset ["context" "contextActivities" "other"]))))
  (testing "sub-statement valueset"
    (is (= (:verbs valuesets)
           (path->valueset-sub
            ["object" "verb"])))
    (is (= (:verb-ids valuesets)
           (path->valueset-sub
            ["object" "verb" "id"])))
    (is (= (:activities valuesets)
           (path->valueset-sub
            ["object" "object"])))
    (is (= (:activity-ids valuesets)
           (path->valueset-sub
            ["object" "object" "id"])))
    (is (= (:activity-types valuesets)
           (path->valueset-sub
            ["object" "object" "definition" "type"])))
    (is (= (:activities valuesets)
           (path->valueset-sub
            ["object" "context" "contextActivities" "category" '*])))
    (is (= (:activity-ids valuesets)
           (path->valueset-sub
            ["object" "context" "contextActivities" "category" '* "id"])))
    (is (= (:activity-types valuesets)
           (path->valueset-sub
            ["object" "context" "contextActivities" "category" '* "definition" "type"])))
    (is (= (:activities valuesets)
           (path->valueset-sub
            ["object" "context" "contextActivities" "grouping" '*])))
    (is (= (:activity-ids valuesets)
           (path->valueset-sub
            ["object" "context" "contextActivities" "grouping" '* "id"])))
    (is (= (:activity-types valuesets)
           (path->valueset-sub
            ["object" "context" "contextActivities" "grouping" '* "definition" "type"])))
    (is (= (:activities valuesets)
           (path->valueset-sub
            ["object" "context" "contextActivities" "parent" '*])))
    (is (= (:activity-ids valuesets)
           (path->valueset-sub
            ["object" "context" "contextActivities" "parent" '* "id"])))
    (is (= (:activity-types valuesets)
           (path->valueset-sub
            ["object" "context" "contextActivities" "parent" '* "definition" "type"])))
    (is (= (:activities valuesets)
           (path->valueset-sub
            ["object" "context" "contextActivities" "other" '*])))
    (is (= (:activity-ids valuesets)
           (path->valueset-sub
            ["object" "context" "contextActivities" "other" '* "id"])))
    (is (= (:activity-types valuesets)
           (path->valueset-sub
            ["object" "context" "contextActivities" "other" '* "definition" "type"])))
    ;; These paths do not return valuesets
    (is (nil? (path->valueset-sub
               ["object" "actor"])))
    (is (nil? (path->valueset-sub
               ["object" "context" "contextActivities" "category"])))
    (is (nil? (path->valueset-sub
               ["object" "context" "contextActivities" "grouping"])))
    (is (nil? (path->valueset-sub
               ["object" "context" "contextActivities" "parent"])))
    (is (nil? (path->valueset-sub
               ["object" "context" "contextActivities" "other"])))))
