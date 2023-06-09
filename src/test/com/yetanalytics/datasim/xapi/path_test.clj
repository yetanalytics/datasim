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
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest spec-map-test
  (is (s/valid? ::path/spec-map path/spec-map)))

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
           (let [spec (path/path->spec-3 ::xs/statement
                                         path
                                         {:object-types object-types})]
             (and spec
                  (s/valid? spec v))))
         (kv-map long-statement)))
    (is (every?
         (fn [[path v]]
           (let [spec (path/path->spec-3 :statement/attachments
                                         path
                                         {:object-types object-types})]
             (and spec
                  (s/valid? spec v))))
         (kv-map sample-attachments)))
    (is (every?
         (fn [[path v]]
           (let [spec (path/path->spec-3 ::xs/sub-statement
                                         (into path)
                                         {:object-types object-types})]
             (and spec
                  (s/valid? spec v))))
         (kv-map (-> long-statement
                     (assoc "objectType" "SubStatement"
                            "attachments" sample-attachments)
                     (dissoc "id" "stored" "version" "authority"))))))
  (testing "works for arbitrary and relative paths"
    (is (= :score/max
           (path/path->spec-3 ::xs/result
                              ["score" "max"]
                              {})))
    (is (= :activity/id
           (path/path->spec-3 ::xs/context
                              ["contextActivities" "grouping" '* "id"]
                              {})))
    (is (= :group/account
           (path/path->spec-3 ::xs/group
                              ["account"]
                              {})))
    (is (= :account/homePage
           (path/path->spec-3 ::xs/group
                              ["account" "homePage"]
                              {})))
    (is (= ::xs/interaction-component
           (path/path->spec-3 :activity/definition
                              ["choices" '*]
                              {})))
    (is (= ::xs/interaction-component
           (path/path->spec-3 :definition/scale
                              ['*]
                              {})))
    (is (= :interaction-component/id
           (path/path->spec-3 :definition/source
                              ['* "id"]
                              {})))
    (is (= :interaction-component/description
           (path/path->spec-3 :definition/target
                              ['* "description"]
                              {})))
    (is (= ::xs/language-map-text
           (path/path->spec-3 :definition/steps
                              ['* "description" "en-US"]
                              {}))))
  (testing "can return custom specs"
    (is (= :actor/objectType
           (path/path->spec-3 ::xs/statement
                              ["actor" "objectType"]
                              {:object-types {["actor"] #{"agent" "group"}}})))
    (is (= :correctResponsesPattern/string
           (path/path->spec-3 ::xs/statement
                              ["object" "definition" "correctResponsesPattern" '*]
                              {:object-types {["object"] #{"activity"}}}))))
  (testing "can work with different object types"
    ;; Actors
    (is (= :agent/name
           (path/path->spec-3 ::xs/statement
                              ["actor" "name"]
                              {:object-types
                               {["actor"] #{"agent"}}})))
    (is (= :agent/name
           (path/path->spec-3 ::xs/statement
                              ["actor" "name"]
                              {:object-types
                               {["actor"] #{"agent" "group"}}})))
    (is (= :group/name
           (path/path->spec-3 ::xs/statement
                              ["actor" "name"]
                              {:object-types
                               {["actor"] #{"group"}}})))
    (is (= :group/member
           (path/path->spec-3 ::xs/statement
                              ["actor" "member"]
                              {:object-types
                               {["actor"] #{"agent" "group"}}})))
    ;; Statement objects
    (is (= :agent/name
           (path/path->spec-3 ::xs/statement
                              ["object" "name"]
                              {:object-types
                               {["object"] #{"agent" "group"}}})))
    (is (= :agent/name
           (path/path->spec-3 ::xs/statement
                              ["object" "name"]
                              {:object-types
                               {["object"] #{"agent"}}})))
    (is (= :group/name
           (path/path->spec-3 ::xs/statement
                              ["object" "name"]
                              {:object-types
                               {["object"] #{"group"}}})))
    (is (= :statement-ref/id
           (path/path->spec-3 ::xs/statement
                              ["object" "id"]
                              {:object-types
                               {["object"] #{"statement-ref"}}})))
    (is (= :sub-statement/verb
           (path/path->spec-3 ::xs/statement
                              ["object" "verb"]
                              {:object-types
                               {["object"] #{"sub-statement"}}})))
    (is (= :activity/id
           (path/path->spec-3 ::xs/statement
                              ["object" "id"]
                              {:object-types
                               {["object"] #{"activity" "statement-ref"}}})))
    ;; Sub-statement objects
    (is (= :agent/name
           (path/path->spec-3 ::xs/statement
                              ["object" "object" "name"]
                              {:object-types
                               {["object"] #{"sub-statement"}
                                ["object" "object"] #{"agent" "group"}}})))
    (is (= :agent/name
           (path/path->spec-3 ::xs/statement
                              ["object" "object" "name"]
                              {:object-types
                               {["object"] #{"sub-statement"}
                                ["object" "object"] #{"agent"}}})))
    (is (= :group/name
           (path/path->spec-3 ::xs/statement
                              ["object" "object" "name"]
                              {:object-types
                               {["object"] #{"sub-statement"}
                                ["object" "object"] #{"group"}}})))
    (is (= :activity/id
           (path/path->spec-3 ::xs/statement
                              ["object" "object" "id"]
                              {:object-types
                               {["object"] #{"sub-statement"}
                                ["object" "object"] #{"activity"}}})))
    (is (= :statement-ref/id
           (path/path->spec-3 ::xs/statement
                              ["object" "object" "id"]
                              {:object-types
                               {["object"] #{"sub-statement"}
                                ["object" "object"] #{"statement-ref"}}})))
    (is (= :activity/id
           (path/path->spec-3 ::xs/statement
                              ["object" "object" "id"]
                              {:object-types
                               {["object"] #{"sub-statement"}
                                ["object" "object"] #{"activity" "statement-ref"}}})))
    ;; Bad type combos
    (is (= ::path/unsuppored-object-types
           (try (path/path->spec-3 ::xs/statement
                                   ["object" "id"]
                                   {:object-types
                                    {["object"] #{"sub-statement" "statement-ref"}}})
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::path/unsuppored-object-types
           (try (path/path->spec-3 ::xs/statement
                                   ["object" "object" "id"]
                                   {:object-types
                                    {["object"] #{"sub-statement"}
                                     ["object" "object"] #{"sub-statement"}}})
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::path/unsuppored-object-types
           (try (path/path->spec-3 ::xs/statement
                                   ["actor" "id"]
                                   {:object-types {["actor"] #{"sub-statement"}}})
                (catch Exception e (-> e ex-data :type)))))
    ;; if we go one level up we don't care about bad object types
    (is (= :statement/object
           (path/path->spec-3 ::xs/statement
                              ["object"]
                              {:object-types
                               {["object"] #{"sub-statement" "statement-ref"}}})))
    (is (= :sub-statement/object
           (path/path->spec-3 ::xs/statement
                              ["object" "object"]
                              {:object-types
                               {["object"] #{"sub-statement"}
                                ["object" "object"] #{"sub-statement"}}})))
    (is (= :statement/actor
           (path/path->spec-3 ::xs/statement
                              ["actor"]
                              {:object-types {["actor"] #{"sub-statement"}}}))))
  (testing "bad paths or specs"
    (is (= ::path/invalid-spec
           (try (path/path->spec-3 ::xs/statement
                                   ["actor" "badSpec"]
                                   {:object-types {["actor"] #{"agent"}}})
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::path/unknown-path-spec
           (try (path/path->spec-3 ::xs/statement
                                   ["actor" "zoo" "wee" "mama"]
                                   {:object-types {["actor"] #{"agent"}}})
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::path/invalid-path-map-key
           (try (path/path->spec-3 ::xs/statement
                                   ["object" '*]
                                   {:object-types {["object"] #{"activity"}}})
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::path/invalid-path-array-key
           (try (path/path->spec-3 ::xs/statement
                                   ["context" "contextActivities" "grouping" "foo"]
                                   {})
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::path/invalid-path-spec-key
           (try (path/path->spec-3 ::xs/statement
                                   ["verb" "display" '*]
                                   {})
                (catch Exception e (-> e ex-data :type))))))
  (testing "extensions get special treatment"
    (is (= :com.yetanalytics.datasim.json/any
           (path/path->spec-3 :statement/context
                              ["extensions" "http://foo.org/extension"]
                              {:iri-map {}})))
    (is (= :com.yetanalytics.datasim.json.schema/integer
           (path/path->spec-3 :statement/context
                              ["extensions" "http://foo.org/extension"]
                              {:iri-map {"http://foo.org/extension"
                                         {:id "http://foo.org/extension"
                                          :type "ContextExtension"
                                          :inlineSchema "{\"type\": \"integer\"}"}}})))))
