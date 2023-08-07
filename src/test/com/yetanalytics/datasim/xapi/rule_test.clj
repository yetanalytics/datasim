(ns com.yetanalytics.datasim.xapi.rule-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.generators :as stest]
            [clojure.java.io    :as io]
            [cheshire.core      :as json]
            [clojure.spec.alpha :as s]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.schemer             :as schemer]
            [com.yetanalytics.datasim.math.random :as random]
            [com.yetanalytics.datasim.xapi.path   :as xp]
            [com.yetanalytics.datasim.xapi.rule   :as r]
            [com.yetanalytics.datasim.xapi.profile :as profile]
            [com.yetanalytics.datasim.test-constants :as const])
  (:import [clojure.lang ExceptionInfo]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def short-statement
  {"id"        "59de1b06-bb6c-4708-a51a-b3d403c491db"
   "actor"     {"name" "Alice Faux"
                "mbox" "mailto:alice@example.org"}
   "verb"      {"id" "https://adlnet.gov/expapi/verbs/launched"}
   "object"    {"id"         "https://example.org/career/1054719918"
                "definition" {"type" "https://w3id.org/xapi/tla/activity-types/career"}
                "objectType" "Activity"}
   "context"   {"registration" "d7acfddb-f4c2-49f4-a081-ad1fb8490448"}
   "timestamp" "2021-03-18T17:36:22.131Z"})

(def long-statement
  (with-open
   [r (io/reader const/long-statement-filepath)]
    (json/parse-stream r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Parse/Follow/Match Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro is-parsed [parsed-rules rules]
  `(is (= ~parsed-rules
          (r/parse-rules ~rules))))

(deftest parse-rules-test
  (testing "included presence, no value requirement"
    (is-parsed [{:location [[["id"]]]
                 :presence :included
                 :path     ["id"]
                 :spec     :statement/id}]
               [{:location  "$.id"
                 :presence  "included"}]))
  (testing "included presence, no value requirement, with scope note"
    (is-parsed [{:location [[["timestamp"]]]
                 :presence :included
                 :path     ["timestamp"]
                 :spec     :statement/timestamp}]
               [{:location  "$.timestamp"
                 :presence  "included"
                 :scopeNote {:en-US "This is a scope note"}}]))
  (testing "included presence, any values"
    (is-parsed [{:location [[["verb"] ["id"]]]
                 :presence :included
                 :valueset #{"http://adlnet.gov/expapi/verbs/launched"
                             "http://adlnet.gov/expapi/verbs/attended"}
                 :any      #{"http://adlnet.gov/expapi/verbs/launched"
                             "http://adlnet.gov/expapi/verbs/attended"}
                 :path     ["verb" "id"]
                 :spec     :verb/id}]
               [{:location  "$.verb.id"
                 :presence  "included"
                 :any       ["http://adlnet.gov/expapi/verbs/launched"
                             "http://adlnet.gov/expapi/verbs/attended"]}]))
  (testing "included presence, all values"
    (is-parsed [{:location [[["verb"] ["id"]]]
                 :presence :included
                 :valueset #{"http://adlnet.gov/expapi/verbs/launched"
                             "http://adlnet.gov/expapi/verbs/attended"}
                 :all      #{"http://adlnet.gov/expapi/verbs/launched"
                             "http://adlnet.gov/expapi/verbs/attended"}
                 :path     ["verb" "id"]
                 :spec     :verb/id}]
               [{:location  "$.verb.id"
                 :presence  "included"
                 :all       ["http://adlnet.gov/expapi/verbs/launched"
                             "http://adlnet.gov/expapi/verbs/attended"]}]))
  (testing "included presence, none values"
    (is-parsed [{:location [[["verb"] ["id"]]]
                 :presence :included
                 :none     #{"http://adlnet.gov/expapi/verbs/launched"}
                 :path     ["verb" "id"]
                 :spec     :verb/id}]
               [{:location  "$.verb.id"
                 :presence  "included"
                 :none      ["http://adlnet.gov/expapi/verbs/launched"]}]))
  (testing "included presence, any and all values"
    (is-parsed [{:location [[["verb"] ["id"]]]
                 :presence :included
                 :valueset  #{"http://adlnet.gov/expapi/verbs/launched"}
                 :any       #{"http://adlnet.gov/expapi/verbs/launched"
                              "http://adlnet.gov/expapi/verbs/attended"}
                 :all       #{"http://adlnet.gov/expapi/verbs/launched"}
                 :path      ["verb" "id"]
                 :spec      :verb/id}]
               [{:location  "$.verb.id"
                 :presence  "included"
                 :any       ["http://adlnet.gov/expapi/verbs/launched"
                             "http://adlnet.gov/expapi/verbs/attended"]
                 :all       ["http://adlnet.gov/expapi/verbs/launched"]}]))
  (testing "included presence, any and none values"
    (is-parsed [{:location [[["verb"] ["id"]]]
                 :presence :included
                 :valueset  #{"http://adlnet.gov/expapi/verbs/launched"}
                 :any       #{"http://adlnet.gov/expapi/verbs/launched"
                              "http://adlnet.gov/expapi/verbs/attended"}
                 :none      #{"http://adlnet.gov/expapi/verbs/attended"}
                 :path      ["verb" "id"]
                 :spec      :verb/id}]
               [{:location  "$.verb.id"
                 :presence  "included"
                 :any       ["http://adlnet.gov/expapi/verbs/launched"
                             "http://adlnet.gov/expapi/verbs/attended"]
                 :none      ["http://adlnet.gov/expapi/verbs/attended"]}]))
  (testing "included presence, all and none values"
    (is-parsed [{:location [[["verb"] ["id"]]]
                 :presence :included
                 :valueset  #{"http://adlnet.gov/expapi/verbs/launched"}
                 :all       #{"http://adlnet.gov/expapi/verbs/launched"
                              "http://adlnet.gov/expapi/verbs/attended"}
                 :none      #{"http://adlnet.gov/expapi/verbs/attended"}
                 :path      ["verb" "id"]
                 :spec      :verb/id}]
               [{:location  "$.verb.id"
                 :presence  "included"
                 :all       ["http://adlnet.gov/expapi/verbs/launched"
                             "http://adlnet.gov/expapi/verbs/attended"]
                 :none      ["http://adlnet.gov/expapi/verbs/attended"]}]))
  (testing "included presence, any, all and none values"
    (is-parsed [{:location [[["verb"] ["id"]]]
                 :presence :included
                 :valueset  #{"http://adlnet.gov/expapi/verbs/launched"}
                 :any       #{"http://adlnet.gov/expapi/verbs/launched"
                              "http://adlnet.gov/expapi/verbs/initialized"
                              "http://adlnet.gov/expapi/verbs/passed"}
                 :all       #{"http://adlnet.gov/expapi/verbs/launched"
                              "http://adlnet.gov/expapi/verbs/initialized"
                              "http://adlnet.gov/expapi/verbs/completed"}
                 :none      #{"http://adlnet.gov/expapi/verbs/initialized"}
                 :path      ["verb" "id"]
                 :spec      :verb/id}]
               [{:location  "$.verb.id"
                 :presence  "included"
                 :any       ["http://adlnet.gov/expapi/verbs/launched"
                             "http://adlnet.gov/expapi/verbs/initialized"
                             "http://adlnet.gov/expapi/verbs/passed"]
                 :all       ["http://adlnet.gov/expapi/verbs/launched"
                             "http://adlnet.gov/expapi/verbs/initialized"
                             "http://adlnet.gov/expapi/verbs/completed"]
                 :none      ["http://adlnet.gov/expapi/verbs/initialized"]}]))
  (testing "included presence, path with wildcards"
    (is-parsed [{:location [[["actor"] ["member"] '* ["objectType"]]]
                 :presence :included
                 :all      #{"Agent" "Group"}
                 :valueset #{"Agent" "Group"}
                 :path     ["actor" "member" '* "objectType"]
                 :spec     :agent/objectType}]
               [{:location  "$.actor.member[*].objectType"
                 :presence  "included"
                 :all       ["Agent" "Group"]}]))
  (testing "excluded presence, no value requirement"
    (is-parsed [{:location [[["context"] ["contextActivities"] ["grouping"]]]
                 :presence :excluded
                 :path     ["context" "contextActivities" "grouping"]
                 :spec     ::xs/context-activities-array}]
               [{:location  "$.context.contextActivities.grouping"
                 :presence  "excluded"}]))
  (testing "recommended presence, no value req, exists in long statement"
    (is-parsed [{:location [[["context"] ["contextActivities"] ["parent"]]]
                 :presence :recommended
                 :path     ["context" "contextActivities" "parent"]
                 :spec     ::xs/context-activities-array}]
               [{:location  "$.context.contextActivities.parent"
                 :presence  "recommended"}]))
  ;; Rule separation now applies
  (testing "selector path with array indices"
    (is-parsed
     [{:location [[["context"] ["contextActivities"] ["other"] [0 1] ["id"]]]
       :presence :included
       :path     ["context" "contextActivities" "other" '* "id"]
       :spec     :activity/id}]
     [{:location  "$.context.contextActivities.other[0,1]"
       :selector  "$.id"
       :presence  "included"}]))
  (testing "selector path with multiple string keys"
    (is-parsed
     [{:location  [[["context"] ["contextActivities"] ["category"] [0 1]]]
       :presence  :included
       :path      ["context" "contextActivities" "category" '*]
       :spec      ::xs/activity}
      {:location  [[["context"] ["contextActivities"] ["grouping"] [0 1]]]
       :presence  :included
       :path      ["context" "contextActivities" "grouping" '*]
       :spec      ::xs/activity}]
     [{:location  "$.context.contextActivities['category','grouping']"
       :selector  "$[0,1]"
       :presence  "included"}]))
  (testing "selector path with and multiple keys and wildcard"
    (is-parsed
     [{:location  [[["context"] ["contextActivities"] ["parent"] '*]]
       :presence  :included
       :path      ["context" "contextActivities" "parent" '*]
       :spec      ::xs/activity}
      {:location  [[["context"] ["contextActivities"] ["other"] '*]]
       :presence  :included
       :path      ["context" "contextActivities" "other" '*]
       :spec      ::xs/activity}]
     [{:location  "$.context.contextActivities['parent','other']"
       :selector  "$.*"
       :presence  "included"}]))
  (testing "path with pipe operator"
    (is-parsed
     [{:location [[["object"] ["objectType"]]]
       :presence :included
       :path     ["object" "objectType"]
       :spec     :sub-statement/objectType}
      {:location     [[["object"] ["object"] ["objectType"]]]
       :presence     :included
       :path         ["object" "object" "objectType"]
       :spec         :activity/objectType}] ; defaults to Activity
     [{:location  "$.object.objectType | $.object.object.objectType"
       :presence  "included"}]))
  ;; Errors
  (testing "excluded presence, path with wildcards"
    ;; FIXME: Figure out how to apply rule separation in these cases of
    ;; wildcards on JSON objects (as opposed to arrays).
    (is (= ::xp/invalid-path-map-key
           (try (r/parse-rules
                 [{:location "$.actor.*"
                   :presence "excluded"}])
                (catch ExceptionInfo e (-> e ex-data :type))))))
  (testing "mixing integer and string keys"
    (is (= ::r/invalid-rule-location
           (try (r/parse-rules
                 [{:location "$.actor[0,'name']"
                   :presence "included"}])
                (catch ExceptionInfo e (-> e ex-data :type)))))
    (is (= ::r/invalid-rule-location
           (try (r/parse-rules
                 [{:location "$.actor"
                   :selector "$[0,'name']"
                   :presence "included"}])
                (catch ExceptionInfo e (-> e ex-data :type))))))
  (testing "non-existent verb property"
    (is (= ::xp/unknown-path-spec
           (try (r/parse-rules
                 [{:location "$.verb.foo.bar.baz"
                   :presence "included"}])
                (catch ExceptionInfo e (-> e ex-data :type)))))
    (is (= ::xp/invalid-spec
           (try (r/parse-rules
                 [{:location "$.verb.zooWeeMama"
                   :presence "included"}])
                (catch ExceptionInfo e (-> e ex-data :type)))))
    (is (= ::xp/invalid-spec
           (try (r/parse-rules
                 [{:location "$.verb.name"
                   :presence "included"}])
                (catch ExceptionInfo e (-> e ex-data :type))))))
  (testing "non-existent object property"
    (is (= ::r/invalid-object-types
           (try (r/parse-rules
                 [{:location "$.object.zooWeeMama"
                   :presence "included"}])
                (catch ExceptionInfo e (-> e ex-data :type)))))
    (is (= ::r/invalid-object-types
           (try (r/parse-rules
                 [{:location "$.object.authority"
                   :presence "included"}])
                (catch ExceptionInfo e (-> e ex-data :type))))))
  (testing "empty valueset"
    (is (= ::r/invalid-rule-values
           (try (r/parse-rules
                 [{:location "$.actor.name"
                   :any      ["Alice Faux"]
                   :all      ["Bob Fakename"]}])
                (catch ExceptionInfo e (-> e ex-data :type)))))
    (is (= ::r/invalid-rule-values
           (try (r/parse-rules
                 [{:location "$.actor.name"
                   :any      ["Alice Faux"]
                   :none     ["Alice Faux"]}]) 
                (catch ExceptionInfo e (-> e ex-data :type)))))
    (is (= ::r/invalid-rule-values
           (try (r/parse-rules
                 [{:location "$.actor.name"
                   :all      ["Bob Fakename"]
                   :none     ["Bob Fakename"]}])
                (catch ExceptionInfo e (-> e ex-data :type)))))))

(defmacro is-obj-types [object-types rules]
  `(is (= (merge xp/default-object-type-m ~object-types)
          (->> (r/parse-rules ~rules) r/rules->object-types))))

(deftest spec-object-types-test
  (testing "Statement object based on objectType"
    (is-obj-types {["object"] #{"activity"}}
                  [{:location "$.object.objectType"
                    :all      ["Activity"]}])
    (is-obj-types {["object"] #{"agent"}}
                  [{:location "$.object.objectType"
                    :all      ["Agent"]}])
    (is-obj-types {["object"] #{"group"}}
                  [{:location "$.object.objectType"
                    :all      ["Group"]}])
    (is-obj-types {["object"] #{"statement-ref"}}
                  [{:location "$.object.objectType"
                    :all      ["StatementRef"]}])
    (is-obj-types {["object"] #{"sub-statement"}}
                  [{:location "$.object.objectType"
                    :all      ["SubStatement"]}])
    (is-obj-types {["object"] #{"agent" "group"}}
                  [{:location "$.object.objectType"
                    :all      ["Agent" "Group"]}]))
  (testing "SubStatement object based on objectType"
    (is-obj-types {["object"] #{"sub-statement"}
                   ["object" "object"] #{"activity"}}
                  [{:location "$.object.object.objectType"
                    :all      ["Activity"]}])
    (is-obj-types {["object"] #{"sub-statement"}
                   ["object" "object"] #{"activity"}}
                  [{:location "$.object.object.objectType"
                    :all      ["Activity"]}])
    (is-obj-types {["object"] #{"sub-statement"}
                   ["object" "object"] #{"agent"}}
                  [{:location "$.object.object.objectType"
                    :all      ["Agent"]}])
    (is-obj-types {["object"] #{"sub-statement"}
                   ["object" "object"] #{"group"}}
                  [{:location "$.object.object.objectType"
                    :all      ["Group"]}])
    (is-obj-types {["object"] #{"sub-statement"}
                   ["object" "object"] #{"statement-ref"}}
                  [{:location "$.object.object.objectType"
                    :all      ["StatementRef"]}])
    (is-obj-types {["object"] #{"sub-statement"}
                   ["object" "object"] #{"agent" "group"}}
                  [{:location "$.object.object.objectType"
                    :all      ["Agent" "Group"]}]))
  (testing "Actors based on objectType"
    (is-obj-types {["actor"] #{"agent"}
                   ["authority"] #{"agent"}
                   ["context" "instructor"] #{"agent"}}
                  [{:location "$.actor.objectType"
                    :all      ["Agent"]}
                   {:location "$.authority.objectType"
                    :all      ["Agent"]}
                   {:location "$.context.instructor.objectType"
                    :all      ["Agent"]}])
    (is-obj-types {["actor"] #{"group"}
                   ["authority"] #{"group"}
                   ["context" "instructor"] #{"group"}}
                  [{:location "$.actor.objectType"
                    :all      ["Group"]}
                   {:location "$.authority.objectType"
                    :all      ["Group"]}
                   {:location "$.context.instructor.objectType"
                    :all      ["Group"]}])
    (is-obj-types {["actor"] #{"agent" "group"}
                   ["authority"] #{"agent" "group"}
                   ["context" "instructor"] #{"agent" "group"}}
                  [{:location "$.actor.objectType"
                    :all      ["Agent" "Group"]}
                   {:location "$.authority.objectType"
                    :all      ["Agent" "Group"]}
                   {:location "$.context.instructor.objectType"
                    :all      ["Agent" "Group"]}])
    (is-obj-types {["object"] #{"sub-statement"}
                   ["object" "actor"] #{"agent" "group"}
                   ["object" "context" "instructor"] #{"agent" "group"}}
                  [{:location "$.object.actor.objectType"
                    :all      ["Agent" "Group"]}
                   {:location "$.object.context.instructor.objectType"
                    :all      ["Agent" "Group"]}]))
  (testing "Objects based on properties"
    (is-obj-types {["object"] #{"activity" "agent" "group" "statement-ref" "sub-statement"}}
                  [{:location "$.object"
                    :presence "included"}])
    (is-obj-types {["object"] #{"activity" "agent" "group" "statement-ref" "sub-statement"}}
                  [{:location "$.object.objectType"
                    :presence "included"}])
    (is-obj-types {["object"] #{"activity" "statement-ref"}}
                  [{:location "$.object.id"
                    :presence "included"}])
    (is-obj-types {["object"] #{"activity"}}
                  [{:location "$.object.definition"
                    :presence "included"}])
    (is-obj-types {["object"] #{"activity"}}
                  [{:location "$.object.id"
                    :presence "included"}
                   {:location "$.object.definition"
                    :presence "included"}])
    (is-obj-types {["object"] #{"agent" "group"}}
                  [{:location "$.object.name"
                    :presence "included"}])
    (is-obj-types {["object"] #{"agent" "group"}}
                  [{:location "$.object.mbox"
                    :presence "included"}])
    (is-obj-types {["object"] #{"agent" "group"}}
                  [{:location "$.object.mbox_sha1sum"
                    :presence "included"}])
    (is-obj-types {["object"] #{"agent" "group"}}
                  [{:location "$.object.openid"
                    :presence "included"}])
    (is-obj-types {["object"] #{"agent" "group"}}
                  [{:location "$.object.account"
                    :presence "included"}])
    (is-obj-types {["object"] #{"agent" "group"}}
                  [{:location "$.object.account.name"
                    :presence "included"}])
    (is-obj-types {["object"] #{"agent" "group"}}
                  [{:location "$.object.account.homePage"
                    :presence "included"}])
    (is-obj-types {["object"] #{"group"}}
                  [{:location "$.object.member"
                    :presence "included"}])
    (is-obj-types {["object"] #{"group"}}
                  [{:location "$.object.name"
                    :presence "included"}
                   {:location "$.object.member"
                    :presence "included"}])
    (is-obj-types {["object"] #{"sub-statement"}
                   ["object" "actor"] #{"agent" "group"}}
                  [{:location "$.object.actor"
                    :presence "included"}])
    (is-obj-types {["object"] #{"sub-statement"}
                   ["object" "object"] #{"activity" "agent" "group" "statement-ref"}}
                  [{:location "$.object.object"
                    :presence "included"}])
    (is-obj-types {["object"] #{"sub-statement"}}
                  [{:location "$.object.verb"
                    :presence "included"}])
    (is-obj-types {["object"] #{"sub-statement"}}
                  [{:location "$.object.context"
                    :presence "included"}])
    (is-obj-types {["object"] #{"sub-statement"}}
                  [{:location "$.object.result"
                    :presence "included"}])
    (is-obj-types {["object"] #{"sub-statement"}}
                  [{:location "$.object.timestamp"
                    :presence "included"}]))
  (testing "Contradictory types"
    (is (= ::r/invalid-object-types
           (try (-> [{:location "$.object.objectType"
                      :any      ["BadType"]}]
                    r/parse-rules
                    r/rules->object-types)
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::r/invalid-object-types
           (try (-> [{:location "$.object.id"
                      :presence "included"}
                     {:location "$.object.objectType"
                      :any      ["Agent" "Group"]}]
                    r/parse-rules
                    r/rules->object-types)
                (catch Exception e (-> e ex-data :type)))))
    (is (= ::r/invalid-object-types
           (try (-> [{:location "$.object.id"
                      :presence "included"}
                     {:location "$.object.name"
                      :presence "included"}]
                    r/parse-rules
                    r/rules->object-types)
                (catch Exception e (-> e ex-data :type)))))))

(def valuegen-extension-specs
  {:activity {"http://foo.org/activity-extension"
              (schemer/schema->spec nil "{\"type\":\"string\"}")}
   :context  {"http://foo.org/context-extension"
              (schemer/schema->spec nil "{\"type\":\"integer\"}")}
   :result   {"http://foo.org/result-extension"
              (schemer/schema->spec nil "{\"type\":\"boolean\"}")}})

(def valuegen-valuesets
  {:verbs          #{{"id""http://foo.org/verb"}}
   :verb-ids       #{"http://foo.org/verb"}
   :activities     #{{"id"         "http://foo.org/activity"
                      "objectType" "Activity"
                      "definition" {"type" "http://foo.org/activity-type"}}}
   :activity-ids   #{"http://foo.org/activity"}
   :activity-types #{"http://foo.org/activity-type"}})

(defn- parse-rule-valuegen [rule]
  (r/add-rule-valuegen valuegen-extension-specs
                       valuegen-valuesets
                       (first (r/parse-rules [rule]))))

(defn- parse-rule-sub-valuegen [rule]
  (r/add-rule-valuegen valuegen-extension-specs
                       valuegen-valuesets
                       (first (r/parse-rules [rule]))))

(deftest valuegen-test
  (testing "Ignore if valueset is already present"
    (is (= {:location [[["actor"] ["name"]]]
            :valueset #{"Andrew Downes" "Toby Nichols" "Ena Hills"}
            :all      #{"Andrew Downes" "Toby Nichols" "Ena Hills"}
            :path     ["actor" "name"]
            :spec     :agent/name}
           (parse-rule-valuegen
            {:location "$.actor.name"
             :all      ["Andrew Downes" "Toby Nichols" "Ena Hills"]})))
    (is (= {:location [[["verb"] ["id"]]]
            :valueset #{"http://example.org/verb" "http://example.org/verb-2"}
            :any      #{"http://example.org/verb" "http://example.org/verb-2"}
            :path     ["verb" "id"]
            :spec     :verb/id}
           (parse-rule-valuegen
            {:location "$.verb.id"
             :any      ["http://example.org/verb"
                        "http://example.org/verb-2"]})))
    (is (= {:location [[["object"] ["verb"] ["id"]]]
            :valueset #{"http://example.org/verb" "http://example.org/verb-2"}
            :any      #{"http://example.org/verb" "http://example.org/verb-2"}
            :path     ["object" "verb" "id"]
            :spec     :verb/id}
           (parse-rule-sub-valuegen
            {:location "$.object.verb.id"
             :any      ["http://example.org/verb"
                        "http://example.org/verb-2"]}))))
  (testing "Add valuesets"
    (is (= {:location [[["verb"]]]
            :presence :included
            :path     ["verb"]
            :spec     ::xs/verb
            :valueset #{{"id" "http://foo.org/verb"}}}
           (parse-rule-valuegen
            {:location "$.verb"
             :presence "included"})))
    (is (= {:location [[["verb"] ["id"]]]
            :presence :included
            :path     ["verb" "id"]
            :spec     :verb/id
            :valueset #{"http://foo.org/verb"}}
           (parse-rule-valuegen
            {:location "$.verb.id"
             :presence "included"})))
    (is (= {:location [[["object"]]]
            :presence :included
            :path     ["object"]
            :spec     ::xs/activity 
            :valueset #{{"id"         "http://foo.org/activity"
                         "objectType" "Activity"
                         "definition" {"type" "http://foo.org/activity-type"}}}}
           (parse-rule-valuegen
            {:location "$.object"
             :presence "included"})))
    (is (= {:location [[["object"] ["id"]]]
            :presence :included
            :path     ["object" "id"]
            :spec     :activity/id
            :valueset #{"http://foo.org/activity"}}
           (parse-rule-valuegen
            {:location "$.object.id"
             :presence "included"})))
    (is (= {:location [[["object"] ["definition"] ["type"]]]
            :presence :included
            :path     ["object" "definition" "type"]
            :spec     :definition/type
            :valueset #{"http://foo.org/activity-type"}}
           (parse-rule-valuegen
            {:location "$.object.definition.type"
             :presence "included"})))
    (is (= {:location [[["context"] ["contextActivities"] ["category"] [0]]]
            :presence :included
            :path     ["context" "contextActivities" "category" '*]
            :spec     ::xs/activity
            :valueset #{{"id"         "http://foo.org/activity"
                         "objectType" "Activity"
                         "definition" {"type" "http://foo.org/activity-type"}}}}
           (parse-rule-valuegen
            {:location "$.context.contextActivities.category[0]"
             :presence "included"}))))
  (testing "Add valuesets (substatements)"
    (is (= {:location [[["object"] ["verb"]]]
            :presence :included
            :path     ["object" "verb"]
            :spec     ::xs/verb
            :valueset #{{"id" "http://foo.org/verb"}}}
           (parse-rule-sub-valuegen
            {:location "$.object.verb"
             :presence "included"})))
    (is (= {:location [[["object"] ["verb"] ["id"]]]
            :presence :included
            :path     ["object" "verb" "id"]
            :spec     :verb/id
            :valueset #{"http://foo.org/verb"}}
           (parse-rule-sub-valuegen
            {:location "$.object.verb.id"
             :presence "included"})))
    (is (= {:location [[["object"] ["object"]]]
            :presence :included
            :path     ["object" "object"]
            :spec     ::xs/activity
            :valueset #{{"id"         "http://foo.org/activity"
                         "objectType" "Activity"
                         "definition" {"type" "http://foo.org/activity-type"}}}}
           (parse-rule-sub-valuegen
            {:location "$.object.object"
             :presence "included"})))
    (is (= {:location [[["object"] ["object"] ["id"]]]
            :presence :included
            :path     ["object" "object" "id"]
            :spec     :activity/id
            :valueset #{"http://foo.org/activity"}}
           (parse-rule-sub-valuegen
            {:location "$.object.object.id"
             :presence "included"})))
    (is (= {:location [[["object"] ["object"] ["definition"] ["type"]]]
            :presence :included
            :path     ["object" "object" "definition" "type"]
            :spec     :definition/type
            :valueset #{"http://foo.org/activity-type"}}
           (parse-rule-sub-valuegen
            {:location "$.object.object.definition.type"
             :presence "included"}))))
  (testing "Add spec and generator"
    (is (= ::xs/result
           (:spec (parse-rule-valuegen {:location "$.result"
                                        :presence "included"}))))
    (is (= ::xs/result
           (:spec (parse-rule-sub-valuegen {:location "$.object.result"
                                          :presence "included"}))))
    (is (string?
         (stest/generate
          (:generator (parse-rule-valuegen {:location "$.actor.name"
                                            :presence "included"})))))
    (is (string?
         (stest/generate
          (:generator (parse-rule-sub-valuegen {:location "$.object.actor.name"
                                              :presence "included"}))))))
  (testing "Add spec and generator (extensions)"
    (is (= ::schemer/string
           (:spec (parse-rule-valuegen
                   {:location "$.object.definition.extensions['http://foo.org/activity-extension']"
                    :presence "included"}))))
    (is (= ::schemer/integer
           (:spec (parse-rule-valuegen
                   {:location "$.context.extensions['http://foo.org/context-extension']"
                    :presence "included"}))))
    (is (= ::schemer/boolean
           (:spec (parse-rule-valuegen
                   {:location "$.result.extensions['http://foo.org/result-extension']"
                    :presence "included"}))))
    (is (string?
         (stest/generate
          (:generator (parse-rule-valuegen
                       {:location "$.object.definition.extensions['http://foo.org/activity-extension']"
                        :presence "included"})))))
    (is (int?
         (stest/generate
          (:generator (parse-rule-valuegen
                       {:location "$.context.extensions['http://foo.org/context-extension']"
                        :presence "included"})))))
    (is (boolean?
         (stest/generate
          (:generator (parse-rule-valuegen
                       {:location "$.result.extensions['http://foo.org/result-extension']"
                        :presence "included"})))))))

(deftest follows-rule-test
  (testing "Follows rules -"
    (testing "statement has ID"
      (is (->> [{:location  "$.id"
                 :presence  "included"}]
               r/parse-rules
               first
               (r/follows-rule? long-statement))))
    (testing "statement has specified verb IDs"
      ;; long-statement has "attended" verb
      (is (->> [{:location  "$.verb.id"
                 :presence  "included"
                 :all       ["http://adlnet.gov/expapi/verbs/attended"
                             "http://adlnet.gov/expapi/verbs/launched"
                             "http://adlnet.gov/expapi/verbs/initialized"]}]
               r/parse-rules
               first
               (r/follows-rule? long-statement)))
      (is (->> [{:location  "$.verb.id"
                 :presence  "included"
                 :any       ["http://adlnet.gov/expapi/verbs/attended"
                             "http://adlnet.gov/expapi/verbs/initialized"]}]
               r/parse-rules
               first
               (r/follows-rule? long-statement)))
      (is (->> [{:location  "$.verb.id"
                 :presence  "included"
                 :none      ["http://adlnet.gov/expapi/verbs/initialized"]}]
               r/parse-rules
               first
               (r/follows-rule? long-statement)))
      (is (->> [{:location  "$.verb.id"
                 :presence  "included" ; only "attended" is left in valueset
                 :all       ["http://adlnet.gov/expapi/verbs/attended"
                             "http://adlnet.gov/expapi/verbs/launched"
                             "http://adlnet.gov/expapi/verbs/initialized"]
                 :any       ["http://adlnet.gov/expapi/verbs/attended"
                             "http://adlnet.gov/expapi/verbs/initialized"]
                 :none      ["http://adlnet.gov/expapi/verbs/initialized"]}]
               r/parse-rules
               first
               (r/follows-rule? long-statement))))
    (testing "statement has specified verb IDs (recommended presence)"
      (is (->> [{:location  "$.verb.id"
                 :presence  "recommended"
                 :all       ["http://adlnet.gov/expapi/verbs/attended"
                             "http://adlnet.gov/expapi/verbs/launched"
                             "http://adlnet.gov/expapi/verbs/initialized"]}]
               r/parse-rules
               first
               (r/follows-rule? long-statement)))
      (is (->> [{:location  "$.verb.id"
                 :presence  "recommended"
                 :any       ["http://adlnet.gov/expapi/verbs/attended"
                             "http://adlnet.gov/expapi/verbs/initialized"]}]
               r/parse-rules
               first
               (r/follows-rule? long-statement)))
      (is (->> [{:location  "$.verb.id"
                 :presence  "recommended"
                 :none      ["http://adlnet.gov/expapi/verbs/initialized"]}]
               r/parse-rules
               first
               (r/follows-rule? long-statement))))
    (testing "statement does not have grouping context activities"
      (is (->> [{:location  "$.context.contextActivities.grouping.*"
                 :presence  "excluded"}]
               r/parse-rules
               first
               (r/follows-rule? long-statement)))))
  (testing "Does not follow rule -"
    (testing "statement does not have ID"
      (is (->> [{:location  "$.id"
                 :presence  "included"}]
               r/parse-rules
               first
               (r/follows-rule? (dissoc short-statement "id"))
               not)))
    (testing "statement has bad verb ID"
      (is (->> [{:location  "$.verb.id"
                 :presence  "included"
                 :any       ["http://adlnet.gov/expapi/verbs/initialized"]}]
               r/parse-rules
               first
               (r/follows-rule? long-statement)
               not)))
    (testing "statement has bad verb ID (recommended presence)"
      (is (->> [{:location  "$.verb.id"
                 :presence  "recommended" ; fails due to presence of all value
                 :any       ["http://adlnet.gov/expapi/verbs/initialized"]}]
               r/parse-rules
               first
               (r/follows-rule? long-statement)
               not)))
    (testing "statement has excluded property (category context activity)"
      (is (->> [{:location "$.context.contextActivities.category.*"
                 :presence "excluded"}]
               r/parse-rules
               first
               (r/follows-rule? long-statement)
               not)))
    (testing "statement does not have grouping context activities (included presence)"
      (is (->> [{:location  "$.context.contextActivities.grouping.*"
                 :presence  "included"}]
               r/parse-rules
               first
               (r/follows-rule? long-statement)
               not)))
    ;; This will normally pass validation under the xAPI spec, but we are
    ;; having `recommended` behave exactly as `included`
    (testing "statement does not have grouping context activities (recommended presence)"
      (is (->> [{:location  "$.context.contextActivities.grouping.*"
                 :presence  "recommended"}]
               r/parse-rules
               first
               (r/follows-rule? long-statement)
               not)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Application Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- apply-rules
  ([statement rule]
   (apply-rules statement rule (random/seed-rng 100)))
  ([statement rules rng]
   (let [parsed-rules (r/parse-rules rules)
         parsed-rules (map (partial r/add-rule-valuegen
                                    valuegen-extension-specs
                                    valuegen-valuesets)
                           parsed-rules)]
     (r/apply-rules statement parsed-rules rng))))

(defmacro is-actor [expected & rules]
  `(is (= ~expected
          (-> (apply-rules ~short-statement ~(vec rules)) (get "actor")))))

(defmacro is-verb [expected & rules]
  `(is (= ~expected
          (-> (apply-rules ~short-statement ~(vec rules)) (get "verb")))))

(defmacro is-ctx-activities [activity-property expected & rules]
  `(is (= ~expected
          (-> (apply-rules ~long-statement ~(vec rules))
              (get-in ["context" "contextActivities" ~activity-property])))))

(deftest apply-actor-rules-test
  (testing "Actor rule:"
    (testing "replace actor"
      (is-actor {"name" "Bob Fakename"
                 "mbox" "mailto:bob@example.org"}
                {:location "$.actor"
                 :all      [{"name" "Bob Fakename"
                             "mbox" "mailto:bob@example.org"}]}))
    (testing "replace actor objectType"
      (is-actor {"name" "Alice Faux"
                 "mbox" "mailto:alice@example.org"
                 "objectType" "Group"}
                {:location "$.actor.objectType"
                 :presence "included"
                 :all      ["Group"]}))
    (testing "replace actor objectType and complete transformation to Group"
      (is-actor {"name" "Alice Faux"
                 "mbox" "mailto:alice@example.org"
                 "objectType" "Group"
                 "member" [{"mbox" "mailto:milt@yetanalytics.com"
                            "name" "milt"
                            "objectType" "Agent"}]}
                {:location "$.actor.objectType"
                 :presence "included"
                 :all      ["Group"]}
                {:location "$.actor.member[0]"
                 :presence "included"
                 :all      [{"mbox"       "mailto:milt@yetanalytics.com"
                             "name"       "milt"
                             "objectType" "Agent"}]}))
    (testing "replace actor name"
      (is-actor {"name" "Bob Fakename"
                 "mbox" "mailto:alice@example.org"}
                {:location "$.actor.name"
                 :all      ["Bob Fakename"]}))
    (testing "replace actor name via `any`"
      (is-actor {"name" "Bob Fakename"
                 "mbox" "mailto:alice@example.org"}
                {:location "$.actor.name"
                 :any      ["Bob Fakename"]}))
    (testing "replace actor name is not applied as name is already included"
      (is-actor {"name" "Alice Faux"
                 "mbox" "mailto:alice@example.org"}
                {:location "$.actor.name"
                 :any      ["Alice Faux" "Bob Fakename"]}))
    (testing "replace actor name is not applied due to already matching"
      (is-actor {"name" "Alice Faux"
                 "mbox" "mailto:alice@example.org"}
                {:location "$.actor.name"
                 :all      ["Alice Faux" "Bob Fakename"]}))
    (testing "remove actor name via `none`"
      (is-actor {"name" "jntEzRjtBxBJ9lS0OY1vzukO9rbS" ; randomly generated
                 "mbox" "mailto:alice@example.org"}
                {:location "$.actor.name"
                 :none     ["Alice Faux" "Bob Fakename"]}))
    (testing "remove actor name via `excluded` (any)"
      (is-actor {"mbox" "mailto:alice@example.org"}
                {:location "$.actor.name"
                 :presence "excluded"
                 :any     ["Alice Faux" "Bob Fakename"]}))
    (testing "remove actor name via `excluded` (all)"
      (is-actor {"mbox" "mailto:alice@example.org"}
                {:location "$.actor.name"
                 :presence "excluded"
                 :none     ["Alice Faux" "Bob Fakename"]}))
    (testing "remove actor name via `excluded` (none)"
      (is-actor {"mbox" "mailto:alice@example.org"}
                {:location "$.actor.name"
                 :presence "excluded"
                 :none     ["Alice Faux" "Bob Fakename"]}))
    (testing "remove actor mbox_sha1sum via `excluded` does nothing"
      (is-actor {"name" "Alice Faux"
                 "mbox" "mailto:alice@example.org"}
                {:location "$.actor.mbox_sha1sum"
                 :presence "excluded"}))
    (testing "remove actor name using both location and selector"
      (is-actor {"mbox" "mailto:alice@example.org"}
                {:location "$.actor"
                 :selector "$.name"
                 :presence "excluded"}))
    (testing "remove actor via `excluded`"
      (is-actor nil
                {:location "$.actor"
                 :presence "excluded"}))
    (testing "remove actor properties"
      (is-actor nil
                {:location "$.actor"
                 :presence "excluded"}))
    (testing "both any + all; former is a superset of the latter"
      (is-actor {"name" "Bob Fakename"
                 "mbox" "mailto:alice@example.org"}
                {:location "$.actor.name"
                 :any      ["Alice Faux" "Bob Fakename"]
                 :all      ["Bob Fakename"]}))
    (testing "both any + all; former is a subset of the latter"
      (is-actor {"name" "Bob Fakename"
                 "mbox" "mailto:alice@example.org"}
                {:location "$.actor.name"
                 :any      ["Bob Fakename"]
                 :all      ["Alice Faux" "Bob Fakename"]}))
    (testing "both any + none"
      (is-actor {"name" "Bob Fakename"
                 "mbox" "mailto:alice@example.org"}
                {:location "$.actor.name"
                 :any      ["Bob Fakename"]
                 :none     ["Alice Faux"]}))
    (testing "both all + none"
      (is-actor {"name" "Bob Fakename"
                 "mbox" "mailto:alice@example.org"}
                {:location "$.actor.name"
                 :all      ["Bob Fakename"]
                 :none     ["Alice Faux"]}))
    (testing "any, all + none"
      (is-actor {"name" "Bob Fakename"
                 "mbox" "mailto:alice@example.org"}
                {:location "$.actor.name"
                 :all      ["Alice Faux" "Bob Fakename" "Fred Ersatz"]
                 :any      ["Alice Faux" "Bob Fakename"]
                 :none     ["Alice Faux"]}))))

(deftest apply-verb-rule
  (testing "Verb rules:"
    (testing "replace verb ID using `any`"
      (is-verb {"id" "https://adlnet.gov/expapi/verbs/launched"}
               {:location "$.verb.id"
                :presence "included"
                :any      ["https://adlnet.gov/expapi/verbs/launched"
                           "https://adlnet.gov/expapi/verbs/initialized"]}))
    (testing "replace verb ID using `all`"
      (is-verb {"id" "https://adlnet.gov/expapi/verbs/launched"}
               {:location "$.verb.id"
                :presence "included"
                :all      ["https://adlnet.gov/expapi/verbs/launched"]}))
    (testing "remove verb ID using `none`"
      (is-verb {"id" "http://foo.org/verb"} ; taken from valuegen-valueset
               {:location "$.verb.id"
                :presence "included"
                :none     ["https://adlnet.gov/expapi/verbs/launched"]}))
    (testing "remove verb id using `excluded deletes verb"
      (is-verb nil
               {:location "$.verb.id"
                :presence "excluded"}))
    (testing "remove verb display using `excluded` does nothing"
      (is-verb {"id" "https://adlnet.gov/expapi/verbs/launched"}
               {:location "$.verb.display"
                :presence "excluded"}))
    (testing "insert verb description (`any`)"
      (is-verb {"id" "https://adlnet.gov/expapi/verbs/launched"
                "display" {"en-US" "Launched"}}
               {:location "$.verb.display"
                :any      [{"en-US" "Launched"}]}))
    (testing "insert verb description (`all`)"
      (is-verb {"id" "https://adlnet.gov/expapi/verbs/launched"
                "display" {"en-US" "Launched"}}
               {:location "$.verb.display.en-US"
                :all      ["Launched"]}))
    (testing "insert verb description (spec gen)"
      (is-verb {"id" "https://adlnet.gov/expapi/verbs/launched"
                "display" {"en-US" "F0b8eo9FvLZI3Mf1V3gTneDFUC4s3"}}
               {:location "$.verb.display.en-US"
                :presence "included"}))
    (testing "insert verb description (recommended presence, spec gen)"
      (is-verb {"id" "https://adlnet.gov/expapi/verbs/launched"
                "display" {"en-US" "F0b8eo9FvLZI3Mf1V3gTneDFUC4s3"}}
               {:location "$.verb.display.en-US"
                :presence "recommended"}))
    (testing "include then exclude verb description, resulting in no net change"
      (is-verb {"id" "https://adlnet.gov/expapi/verbs/launched"}
               {:location "$.verb.display.en-US"
                :presence "included"}
               {:location "$.verb.display.en-US"
                :presence "excluded"}))
    (testing "add two lang map entires w/ two rules"
      (is-verb {"id" "https://adlnet.gov/expapi/verbs/launched"
                "display" {"en-US" "Launched"
                           "zh-CN" "展开"}}
               {:location "$.verb.display.en-US"
                :all      ["Launched"]}
               {:location "$.verb.display.zh-CN"
                :all      ["展开"]}))
    (testing "add two lang map entries w/ three rules"
      (is-verb {"id" "https://adlnet.gov/expapi/verbs/launched"
                "display" {"en-US" "Launched"
                           "zh-CN" "hm74DN7tV2jq3S59O5TUQUV5l"}} ; randomly generated
               {:location "$.verb.display.en-US"
                :all      ["Launched"]}
               {:location "$.verb.display.zh-CN"
                :all      ["展开"]}
               {:location "$.verb.display.zh-CN" ; overwrites previous rule
                :none     ["展开"]}))
    (testing "don't replace verb not in profile cosmos with one that is"
      (is-verb {"id" "http://example.org/nonexistent"}
               {:location "$.verb.id"
                :all      ["http://example.org/nonexistent"]}
               {:location "$.verb.id"
                :presence "included"}))))

(deftest apply-context-rules
  (testing "Context rules:"
    (testing "increment one single activity ID"
      (is-ctx-activities
       "parent"
       [{"id" "http://www.example.com/meetings/series/268"
         "objectType" "Activity"}]
       {:location "$.context.contextActivities.parent[0].id"
        :all      ["http://www.example.com/meetings/series/268"]}))
    (testing "increment potentially multiple IDs using wildcard"
      (is-ctx-activities
       "parent"
       [{"id" "http://www.example.com/meetings/series/268" ; randomly chosen
         "objectType" "Activity"}]
       {:location "$.context.contextActivities.parent[*].id"
        :all      ["http://www.example.com/meetings/series/268"]}))
    (testing "replace one activity ID"
      (is-ctx-activities
       "category"
       [{"id" "http://foo.org/activity" ; chosen from valuegen-valueset
         "objectType" "Activity"
         "definition" {"name" {"en" "team meeting"}
                       "description" {"en" "A category of meeting used for regular team meetings."}
                       "type" "http://example.com/expapi/activities/meetingcategory"}}]
       {:location "$.context.contextActivities.category[0].id"
        :none     ["http://www.example.com/meetings/categories/teammeeting"]}))
    (testing "replace two different activity IDs"
      (is-ctx-activities
       "other"
       [{"id" "http://www.example.com/meetings/occurances/foo"
         "objectType" "Activity"}
        {"id" "http://www.example.com/meetings/occurances/bar"
         "objectType" "Activity"}]
       {:location "$.context.contextActivities.other[0,1].id"
        :all      ["http://www.example.com/meetings/occurances/foo"
                   "http://www.example.com/meetings/occurances/bar"]}))
    (testing "replace and insert activity IDs using wildcard"
      (is-ctx-activities
       "other"
       [{"id" "http://www.example.com/meetings/occurances/baz" ; randomly chosen
         "objectType" "Activity"}
        {"id" "http://www.example.com/meetings/occurances/3425567"
         "objectType" "Activity"}]
       {:location "$.context.contextActivities.other[*].id"
        :all      ["http://www.example.com/meetings/occurances/foo"
                   "http://www.example.com/meetings/occurances/bar"
                   "http://www.example.com/meetings/occurances/baz"
                   "http://www.example.com/meetings/occurances/qux"]}))
    (testing "replace and insert multiple activity types"
      (is-ctx-activities
       "other"
       [{"id" "http://www.example.com/meetings/occurances/34257"
         "objectType" "Activity"
         "definition" {"type" "http://www.example.com/activity-type-3"}} ; randomly chosen
        {"id" "http://www.example.com/meetings/occurances/3425567"
         "objectType" "Activity"
         "definition" {"type" "http://www.example.com/activity-type-3"}}
        {"definition" {"type" "http://www.example.com/activity-type-3"}}
        {"definition" {"type" "http://www.example.com/activity-type-2"}}
        {"definition" {"type" "http://www.example.com/activity-type-1"}}]
       {:location "$.context.contextActivities.other[0,1,2,3,4].definition.type"
        :all      ["http://www.example.com/activity-type-1"
                   "http://www.example.com/activity-type-2"
                   "http://www.example.com/activity-type-3"]}))
    (testing "replace one activity ID using any"
      (is-ctx-activities
       "other"
       [{"id" "http://www.example.com/meetings/occurances/34257"
         "objectType" "Activity"}
        {"id" "http://www.example.com/meetings/occurances/foo" ; randomly chosen
         "objectType" "Activity"}]
       {:location "$.context.contextActivities.other[1].id"
        :any      ["http://www.example.com/meetings/occurances/foo"
                   "http://www.example.com/meetings/occurances/bar"]}))
    (testing "replace activity definitions"
      (is-ctx-activities
       "category"
       [{"id"         "http://www.example.com/meetings/categories/teammeeting"
         "objectType" "Activity"
         "definition" {"name"        {"en" "team meeting"}
                       "description" {"en" "foo"}
                       "type"        "http://example.com/expapi/activities/meetingcategory"}}
        {"definition" {"description" {"en" "foo"}}}]
       {:location "$.context.contextActivities.category[0,1].definition.description"
        :any      [{"en" "foo"}]}))
    (testing "try creating multiple IDs"
      (is-ctx-activities
       "grouping"
       [{"id" "http://www.example.com/id-1"} ; randomly shuffled
        {"id" "http://www.example.com/id-2"}
        {"id" "http://www.example.com/id-3"}]
       {:location "$.context.contextActivities.grouping[0,1,2].id"
        :presence "included"
        :all      ["http://www.example.com/id-1"
                   "http://www.example.com/id-2"
                   "http://www.example.com/id-3"]}))
    (testing "try creating multiple IDs, but only one ID is available"
      (is-ctx-activities
       "grouping"
       [{"id" "http://www.example.com/only-id"}
        {"id" "http://www.example.com/only-id"}
        {"id" "http://www.example.com/only-id"}]
       {:location "$.context.contextActivities.grouping[0,1,2].id"
        :presence "included"
        :all      ["http://www.example.com/only-id"]}))
    (testing "try creating multiple IDs, skipping an array entry"
      (is-ctx-activities
       "grouping"
       [{"id" "http://www.example.com/only-id"}
        nil
        {"id" "http://www.example.com/only-id"}]
       {:location "$.context.contextActivities.grouping[0,2].id"
        :presence "included"
        :all      ["http://www.example.com/only-id"]}))
    (testing "assoc an entry out of bounds"
      ;; This was the error Cliff encountered when trying to craft a Profile
      (is-ctx-activities
       "grouping"
       [nil
        {"definition" {"type" "https://xapinet.com/xapi/blooms/activitytypes/cognitive-process-dimension"}}]
       {:location "$.context.contextActivities.grouping[1].definition.type"
        :presence "included"
        :all      ["https://xapinet.com/xapi/blooms/activitytypes/cognitive-process-dimension"]}))
    (testing "use both location and selector to assoc new activity types"
      (is-ctx-activities
       "other"
       [{"id" "http://www.example.com/meetings/occurances/34257"
         "objectType" "Activity"
         "definition" {"type" "http://www.example.com/activity-type-3"}} ; randomly chosen
        {"id" "http://www.example.com/meetings/occurances/3425567"
         "objectType" "Activity"
         "definition" {"type" "http://www.example.com/activity-type-3"}}
        {"definition" {"type" "http://www.example.com/activity-type-3"}}
        {"definition" {"type" "http://www.example.com/activity-type-2"}}
        {"definition" {"type" "http://www.example.com/activity-type-1"}}]
       {:location "$.context.contextActivities.other[0,1,2,3,4]"
        :selector "$.definition.type"
        :all      ["http://www.example.com/activity-type-1"
                   "http://www.example.com/activity-type-2"
                   "http://www.example.com/activity-type-3"]}))
    (testing "use the pipe operator to assoc new activity types"
      ;; IDs are no longer considered distinct if they are used across
      ;; a JSONPath pipe operator
      (is-ctx-activities
       "other"
       [{"id" "http://www.example.com/meetings/occurances/34257"
         "objectType" "Activity"
         "definition" {"type" "http://www.example.com/activity-type-3"}} ; randomly chosen
        {"id" "http://www.example.com/meetings/occurances/3425567"
         "objectType" "Activity"
         "definition" {"type" "http://www.example.com/activity-type-1"}}]
       {:location "$.context.contextActivities.other[0] | $.context.contextActivities.other[1]"
        :selector "$.definition.type"
        :all      ["http://www.example.com/activity-type-1"
                   "http://www.example.com/activity-type-2"
                   "http://www.example.com/activity-type-3"]}))
    (testing "use both pipe operator and selector to assoc new activity types"
      (is-ctx-activities
       "other"
       [{"id" "http://www.example.com/meetings/occurances/34257"
         "objectType" "Activity"
         "definition" {"type" "http://www.example.com/activity-type-3"}} ; randomly chosen
        {"id" "http://www.example.com/meetings/occurances/3425567"
         "objectType" "Activity"
         "definition" {"type" "http://www.example.com/activity-type-1"}}]
       {:location "$.context.contextActivities"
        :selector "$.other[0].definition.type | $.other[1].definition.type"
        :all      ["http://www.example.com/activity-type-1"
                   "http://www.example.com/activity-type-2"
                   "http://www.example.com/activity-type-3"]}))
    ;; Note for `any` rules that values get appended instead of overwriting
    ;; previous values
    (testing "two `any` rules (wildcard)"
      (is-ctx-activities
       "other"
       [{"id" "http://www.example.com/meetings/occurances/34257"
         "objectType" "Activity"}
        {"id" "http://www.example.com/meetings/occurances/3425567"
         "objectType" "Activity"}
        {"id" "http://www.example.com/meetings/occurances/foo"}
        {"id" "http://www.example.com/meetings/occurances/bar"}]
       {:location "$.context.contextActivities.other[*].id"
        :any ["http://www.example.com/meetings/occurances/foo"]}
       {:location "$.context.contextActivities.other[*].id"
        :any ["http://www.example.com/meetings/occurances/bar"]}))
    (testing "two `any` rules (indexes)"
      (is-ctx-activities
       "other"
       [{"id"         "http://www.example.com/meetings/occurances/bar"
         "objectType" "Activity"}
        {"id" "http://www.example.com/meetings/occurances/3425567"
         "objectType" "Activity"}]
       {:location "$.context.contextActivities.other[0].id"
        :any ["http://www.example.com/meetings/occurances/foo"]}
       {:location "$.context.contextActivities.other[0].id"
        :any ["http://www.example.com/meetings/occurances/bar"]}))
    ;; FIXME: Should have replaced entirety of other coll, not just first entry
    ;; TODO: ERROR on blatantly contradictory rules
    (testing "`all` followed by `any`" ; any overwrites all
      (is-ctx-activities
       "other"
       [{"id" "http://www.example.com/meetings/occurances/foo"
         "objectType" "Activity"}
        {"id" "http://www.example.com/meetings/occurances/3425567"
         "objectType" "Activity"}
        {"id" "http://www.example.com/meetings/occurances/bar"}]
       {:location "$.context.contextActivities.other[*].id"
        :all ["http://www.example.com/meetings/occurances/foo"]}
       {:location "$.context.contextActivities.other[*].id"
        :any ["http://www.example.com/meetings/occurances/bar"]}))
    ;; TODO: ERROR on blatantly contradictory rules
    (testing "`any` followed by `all`" ; all overwrites any
      (is-ctx-activities
       "other"
       [{"id" "http://www.example.com/meetings/occurances/bar"
         "objectType" "Activity"}
        {"id" "http://www.example.com/meetings/occurances/3425567"
         "objectType" "Activity"}
        {"id" "http://www.example.com/meetings/occurances/foo"}]
       {:location "$.context.contextActivities.other[*].id"
        :any ["http://www.example.com/meetings/occurances/foo"]}
       {:location "$.context.contextActivities.other[*].id"
        :all ["http://www.example.com/meetings/occurances/bar"]}))
    ;; TODO: ERROR on blatantly contradictory rules
    (testing "two `all` rules"
      (is-ctx-activities
       "other"
       [{"id" "http://www.example.com/meetings/occurances/bar"
         "objectType" "Activity"}
        {"id" "http://www.example.com/meetings/occurances/3425567"
         "objectType" "Activity"}]
       {:location "$.context.contextActivities.other[*].id"
        :all ["http://www.example.com/meetings/occurances/foo"]}
       {:location "$.context.contextActivities.other[*].id"
        :all ["http://www.example.com/meetings/occurances/bar"]}))
    (testing "assoc 1th entry before 0th entry"
      (is-ctx-activities
       "grouping"
       [{"definition"
         {"type" "https://xapinet.com/xapi/blooms/activities/objectives/procedural"}}
        {"definition"
         {"type" "https://xapinet.com/xapi/blooms/activitytypes/cognitive-process-dimension"}}]
       {:location "$.context.contextActivities.grouping[1].definition.type"
        :presence "included"
        :all      ["https://xapinet.com/xapi/blooms/activitytypes/cognitive-process-dimension"]}
       {:location "$.context.contextActivities.grouping[0].definition.type"
        :presence "included"
        :all      ["https://xapinet.com/xapi/blooms/activities/objectives/procedural"]}))
    (testing "don't replace activity not in profile cosmos with one that is"
      ;; This case particularly applies to adding the profile ID
      ;; as a category context activity
      (is-ctx-activities
       "category"
       [{"id"         "http://foo.org/my-profile-id"
         "objectType" "Activity"
         "definition" {"name"        {"en" "team meeting"}
                       "description" {"en" "A category of meeting used for regular team meetings."}
                       "type"        "http://example.com/expapi/activities/meetingcategory"}}]
       {:location "$.context.contextActivities.category[*].id"
        :all      ["http://foo.org/my-profile-id"]}
       {:location "$.context.contextActivities.category[*].id"
        :presence "included"}))))

(deftest apply-rules-distinct-test
  (testing "uses all 3 distinct `all` values for 3 locations"
    (let [rng      (random/seed-rng 120)
          rule     {:location "$.context.contextActivities.grouping[0,1,2].id"
                    :presence "included"
                    :all      ["http://www.example.com/id-1"
                               "http://www.example.com/id-2"
                               "http://www.example.com/id-3"]}
          expected #{{"id" "http://www.example.com/id-1"}
                     {"id" "http://www.example.com/id-2"}
                     {"id" "http://www.example.com/id-3"}}
          apply-fn #(-> long-statement
                        (r/apply-inclusion-rules (r/parse-rules [rule]) rng)
                        (get-in ["context" "contextActivities" "grouping"])
                        set)
          actuals  (repeatedly 30 apply-fn)]
      (is (every? #(= expected %) actuals))))
  (testing "uses 2 of 3 distinct `all` values for 2 locations"
    (let [rng      (random/seed-rng 120)
          rule     {:location "$.context.contextActivities.grouping[0,1].id"
                    :presence "included"
                    :all      ["http://www.example.com/id-1"
                               "http://www.example.com/id-2"
                               "http://www.example.com/id-3"]}
          expect-1 #{{"id" "http://www.example.com/id-1"}
                     {"id" "http://www.example.com/id-2"}}
          expect-2 #{{"id" "http://www.example.com/id-1"}
                     {"id" "http://www.example.com/id-3"}}
          expect-3 #{{"id" "http://www.example.com/id-2"}
                     {"id" "http://www.example.com/id-3"}}
          apply-fn #(-> long-statement
                        (r/apply-inclusion-rules (r/parse-rules [rule]) rng)
                        (get-in ["context" "contextActivities" "grouping"])
                        set)
          actuals  (repeatedly 30 apply-fn)]
      (is (every? #(#{expect-1 expect-2 expect-3} %) actuals)))))

(deftest apply-rules-repeatedly
  (testing "Apply the same rule repeatedly with the same seed -"
    (testing "any values only"
      (let [the-rng  (random/seed-rng 200)
            any-rule {:location "$.context.extensions['http://example.com/extension']"
                      :presence "included"
                      :any      [1 2 3 4 5]}]
        (is (= [2 1 2 3 1 1 4 4 1 3]
               (->> (repeatedly 10 #(apply-rules {} [any-rule] the-rng))
                    (map #(get-in % ["context" "extensions" "http://example.com/extension"])))))))
    (testing "all values only"
      (let [the-rng  (random/seed-rng 300)
            all-rule {:location "$.result.extensions['http://example.com/extension']"
                      :presence "included"
                      :all      ["A" "B" "C" "D" "E"]}]
        (is (= ["A" "B" "D" "A" "E" "E" "B" "A" "B" "A"]
               (->> (repeatedly 10 #(apply-rules {} [all-rule] the-rng))
                    (map #(get-in % ["result" "extensions" "http://example.com/extension"])))))))
    (testing "both any and all values"
      (let [the-rng   (random/seed-rng 400)
            both-rule {:location "$.object.definition.extensions['http://example.com/extension']"
                       :presence "included"
                       :any      [true false 1 2 3]
                       :all      [true false "A" "B" "C"]}]
        (is (= [false true false false false false true false false true]
               (->> (repeatedly 10 #(apply-rules {} [both-rule] the-rng))
                    (map #(get-in % ["object" "definition" "extensions" "http://example.com/extension"])))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CMI5 Rule Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def cmi5-profile-map
  (profile/profiles->profile-map [const/cmi5-profile] {} 100))

(def cmi5-statement ; simple statement used for cmi5 tests
  (with-open [r (io/reader const/simple-statement-filepath)]
    (json/parse-stream r)))

(deftest cmi5-rule-tests
  (testing "cmi5 profile rule"
    (doseq [[id template-rules] (:parsed-rules-map cmi5-profile-map)]
      (testing id
        (let [rng  (random/seed-rng 100)
              stmt (r/apply-rules cmi5-statement template-rules rng)]
          (is (every? #(s/valid? ::r/parsed-rule %) template-rules))
          (is (every? #(r/follows-rule? stmt %) template-rules))
          (is (s/valid? ::xs/statement stmt)))))))
