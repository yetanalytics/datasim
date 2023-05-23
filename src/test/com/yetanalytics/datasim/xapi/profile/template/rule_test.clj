(ns com.yetanalytics.datasim.xapi.profile.template.rule-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [clojure.spec.alpha :as s]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.xapi.profile.template.rule :as r]
            [com.yetanalytics.datasim.test-constants :as const])
  (:import [clojure.lang ExceptionInfo]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def gen-seed 100)

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

(def example-rules
  [;; included presence
   {:location  "$.id"
    :presence  "included"
    :scopeNote {:en "included presence, no value requirement"}}
   {:location  "$.timestamp"
    :presence  "included"
    :scopeNote {:en "included presence, no value requirement, non-ID value"}}
   {:location  "$.verb.id"
    :presence  "included"
    :any       ["http://adlnet.gov/expapi/verbs/launched"
                "http://adlnet.gov/expapi/verbs/attended"]
    :scopeNote {:en "included presence, any values"}}
   {:location  "$.actor.member[*].objectType"
    :presence  "included"
    :any       ["Agent" "Group"]
    :scopeNote {:en "included presence, any values, non-ID value"}}
   {:location  "$.verb.id"
    :presence  "included"
    :all       ["http://adlnet.gov/expapi/verbs/attended"]
    :scopeNote {:en "included presence, any values"}}
   {:location  "$.actor.member[*].name"
    :presence  "included"
    :all       ["Andrew Downes" "Toby Nichols" "Ena Hills"]
    :scopeNote {:en "included presence, all values, non-ID value"}}
   {:location  "$.verb.id"
    :presence  "included"
    :none      ["http://adlnet.gov/expapi/verbs/launched"]
    :scopeNote {:en "included presence, none values"}}
   {:location  "$.version"
    :presence  "included"
    :none      ["1.0.1" "1.0.2" "1.0.3"]
    :scopeNote {:en "included presence, none values, non-ID value"}}
   ;; excluded presence
   {:location  "$.context.contextActivities.grouping"
    :presence  "excluded"
    :scopeNote {:en "excluded presence, no value requirement"}}
   {:location  "$.context.contextActivities.grouping[*].id"
    :presence  "excluded"
    :any       ["http://www.example.com/non-existent-activity/1"
                "http://www.example.com/non-existent-activity/2"
                "http://www.example.com/non-existent-activity/3"]
    :scopeNote {:en "excluded presence, any values (values don't matter)"}}
   {:location  "$.context.contextActivities.grouping[*].objectType"
    :presence  "excluded"
    :all       ["Actor" "Group"]
    :scopeNote {:en "excluded presence, all values (values don't matter)"}}
   {:location  "$.context.contextActivities.group[*].definition.name"
    :presence  "excluded"
    :none      ["bad activity name 1" "bad activity name 2"]
    :scopeNote {:en "excluded presence, none values (values don't matter)"}}
   ;; recommended presence
   {:location  "$.context.contextActivities.parent"
    :presence  "recommended"
    :scopeNote {:en "recommended presence, no value req, exists in long statement"}}
   {:location  "$.context.contextActivities.grouping"
    :presence  "recommended"
    :scopeNote {:en "recommended presence, no value req, does not exist in long statement"}}
   {:location  "$.verb.id"
    :presence  "recommended"
    :any       ["http://adlnet.gov/expapi/verbs/launched"
                "http://adlnet.gov/expapi/verbs/attended"]
    :scopeNote {:en "recommended presence, any values"}}
   {:location  "$.actor.member[*].name"
    :presence  "recommended"
    :all       ["Andrew Downes" "Toby Nichols" "Ena Hills"]
    :scopeNote {:en "recommended presence, all values"}}
   {:location  "$.version"
    :presence  "recommended"
    :none      ["1.0.1" "1.0.2" "1.0.3"]
    :scopeNote {:en "recommended presence, none values"}}
   ;; no presence
   {:location  "$.verb.id"
    :any       ["http://adlnet.gov/expapi/verbs/launched"
                "http://adlnet.gov/expapi/verbs/attended"]
    :scopeNote {:en "no presence, any values"}}
   {:location  "$.actor.member[*].name"
    :all       ["Andrew Downes" "Toby Nichols" "Ena Hills"]
    :scopeNote {:en "no presence, all values"}}
   {:location  "$.version"
    :none      ["1.0.1" "1.0.2" "1.0.3"]
    :scopeNote {:en "no presence, none values"}}
   ;; selector JSONPath
   ;; FIXME: selector is currently bugged due to being appended, not
   ;; replacing, the location values
   {:location  "$.context.contextActivities.parent.*"
    :selector  "$.id"
    :any       ["http://www.example.com/meetings/series/266"
                "http://www.example.com/meetings/series/267"
                "http://www.example.com/meetings/series/268"]
    :scopeNote {:en "selector path, any values"}}
   #_{:location  "$.context.contextActivities.category.*"
      :selector  "$.id"
      :all       ["http://www.example.com/meetings/categories/teammeeting"]
      :scopeNote {:en "selector path, all values"}}
   {:location  "$.context.contextActivities.other.*"
    :selector  "$.id"
    :none      ["http://www.example.com/meetings/occurances/0"
                "http://www.example.com/meetings/occurances/1"
                "http://www.example.com/meetings/occurances/2"]
    :scopeNote {:en "selector path, none values"}}
   {:location  "$.context.contextActivities.other[0,1]"
    :selector  "$.id"
    :none      ["http://www.example.com/meetings/occurances/0"
                "http://www.example.com/meetings/occurances/1"
                "http://www.example.com/meetings/occurances/2"]
    :scopeNote {:en "selector path, none values, array"}}
   {:location  "$.context.contextActivities.other[0:1]"
    :selector  "$.id"
    :none      ["http://www.example.com/meetings/occurances/0"
                "http://www.example.com/meetings/occurances/1"
                "http://www.example.com/meetings/occurances/2"]
    :scopeNote {:en "selector path, none values, splice"}}
   ;; object values
   {:location  "$.result.extensions"
    :any       [{"http://example.com/profiles/meetings/resultextensions/minuteslocation" "X:\\meetings\\minutes\\examplemeeting.one"}
                {"http://example.com/profiles/meetings/resultextensions/minuteslocation" "X:\\meetings\\minutes\\examplemeeting.two"}]
    :scopeNote {:en "any value that is a JSON object"}}
   {:location  "$.result.extensions"
    :all       [{"http://example.com/profiles/meetings/resultextensions/minuteslocation" "X:\\meetings\\minutes\\examplemeeting.one"}]
    :scopeNote {:en "all value that is a JSON object"}}
   {:location  "$.result.extensions"
    :none      [{"http://example.com/profiles/meetings/resultextensions/minuteslocation" "X:\\meetings\\minutes\\examplemeeting.two"}]
    :scopeNote {:en "none value that is a JSON object"}}])

(comment
  (r/match-rule long-statement
                (r/parse-rule {:location "$.id"
                               :presence "included"})))

(deftest example-rules-test
  (let [rule-tuples (map (fn [{:keys [scopeNote] :as rule}]
                           [scopeNote (r/parse-rule rule)])
                         example-rules)]
    (testing "Parse rule test:"
      (doseq [[rule-name parsed-rule] rule-tuples]
        (testing rule-name
          (is (nil? (s/explain-data ::r/parsed-rule parsed-rule))))))
    (testing "Matches rule test:"
      (doseq [[rule-name parsed-rule] rule-tuples]
        (testing rule-name
          (is (not= ::r/unmatchable
                    (r/match-rule long-statement parsed-rule))))))
    (testing "Follows rule test:"
      (doseq [[rule-name parsed-rule] rule-tuples]
        (testing rule-name
          (is (r/follows-rule? long-statement parsed-rule)))))))

(deftest not-follows-rule-test
  (testing "Does not follow rules"
    (are [statement rule]
         (->> rule r/parse-rule (r/follows-rule? statement) not)
      ;; No ID
      (dissoc short-statement "id")
      (get example-rules 0)
      ;; Bad Verb ID
      (assoc-in short-statement
                ["verb" "id"]
                "http://foo.org")
      (get example-rules 2)
      ;; Bad Verb ID 2
      (assoc-in short-statement
                ["verb" "id"]
                "http://adlnet.gov/expapi/verbs/launched")
      (get example-rules 6)
      ;; Out-of-place grouping contextActivity
      (assoc-in short-statement
                ["context" "contextActivities" "grouping"]
                [{"id" "https://w3id.org/xapi/tla/v0.13"}])
      (get example-rules 8))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Application Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- apply-rule-gen [statement rule]
  (r/apply-rules-gen statement [rule] :seed gen-seed))

;; Apply one rule at a time
(deftest apply-rule-gen-test
  ;; Actors
  (testing "apply-rules-gen for Actors in short statement"
    (are [new-actor rule]
         (= new-actor
            (-> (apply-rule-gen short-statement rule) (get "actor")))
      ;; Replace actor
      {"name" "Bob Fakename"
       "mbox" "mailto:bob@example.org"}
      {:location "$.actor"
       :all      [{"name" "Bob Fakename"
                   "mbox" "mailto:bob@example.org"}]}
      ;; Replace actor type
      {"name" "Alice Faux"
       "mbox" "mailto:alice@example.org"
       "objectType" "Group"}
      {:location "$.actor.objectType"
       :presence "included"
       :all      ["Group"]}
      ;; Replace actor name
      {"name" "Bob Fakename"
       "mbox" "mailto:alice@example.org"}
      {:location "$.actor.name"
       :all      ["Bob Fakename"]}
      ;; Replace actor name via "any"
      {"name" "Bob Fakename"
       "mbox" "mailto:alice@example.org"}
      {:location "$.actor.name"
       :any      ["Bob Fakename"]}
      ;; Replace actor name - not applied since name is already included
      {"name" "Alice Faux"
       "mbox" "mailto:alice@example.org"}
      {:location "$.actor.name"
       :any      ["Alice Faux" "Bob Fakename"]}
      ;; Replace actor name - applied...
      ;; FIXME???
      {"name" "Bob Fakename"
       "mbox" "mailto:alice@example.org"}
      {:location "$.actor.name"
       :all      ["Alice Faux" "Bob Fakename"]}
      ;; Remove actor name via "none"
      {"name" "ux8B8sU7otk14" ; randomly generated
       "mbox" "mailto:alice@example.org"}
      {:location "$.actor.name"
       :none     ["Alice Faux" "Bob Fakename"]}
      ;; Remove actor name via "excluded" ("any" values)
      {"mbox" "mailto:alice@example.org"}
      {:location "$.actor.name"
       :presence "excluded"
       :any     ["Alice Faux" "Bob Fakename"]}
      ;; Remove actor name via "excluded" ("all" values)
      {"mbox" "mailto:alice@example.org"}
      {:location "$.actor.name"
       :presence "excluded"
       :none     ["Alice Faux" "Bob Fakename"]}
      ;; Remove actor name via "excluded" ("none" values)
      {"mbox" "mailto:alice@example.org"}
      {:location "$.actor.name"
       :presence "excluded"
       :none     ["Alice Faux" "Bob Fakename"]}
      ;; Remove actor via "excluded"
      nil
      {:location "$.actor"
       :presence "excluded"}
      ;; Remove actor properties
      nil
      {:location "$.actor.*"
       :presence "excluded"}))
  ;; Verbs
  (testing "apply-rules-gen for Verbs in short statement"
    (are [new-verb rule]
         (= new-verb
            (-> (apply-rule-gen short-statement rule) (get "verb")))
      ;; Replace verb ID using "any"
      {"id" "https://adlnet.gov/expapi/verbs/launched"}
      {:location "$.verb.id"
       :presence "included"
       :any      ["https://adlnet.gov/expapi/verbs/launched"
                  "https://adlnet.gov/expapi/verbs/initialized"]}
      ;; Replace verb ID using "all"
      {"id" "https://adlnet.gov/expapi/verbs/launched"}
      {:location "$.verb.id"
       :presence "included"
       :all      ["https://adlnet.gov/expapi/verbs/launched"]}
      ;; Remove verb ID using "none"
      {"id" "rdc://vpugpmcqun.ikxdfny.bkq/qljkldinf"} ; ID result from fixed seed
      {:location "$.verb.id"
       :presence "included"
       :none     ["https://adlnet.gov/expapi/verbs/launched"]}
      ;; Remove verb using "excluded"
      nil
      {:location "$.verb.id"
       :presence "excluded"}
      ;; Insert verb description ("any")
      {"id" "https://adlnet.gov/expapi/verbs/launched"
       "display" {"en-US" "Launched"}}
      {:location "$.verb.display"
       :any      [{"en-US" "Launched"}]}
      ;; Insert verb description ("all")
      {"id" "https://adlnet.gov/expapi/verbs/launched"
       "display" {"en-US" "Launched"}}
      {:location "$.verb.display.en-US"
       :all      ["Launched"]}))
  ;; Context Activities
  (testing "apply-rules-gen for Context activities in long statement"
    (are [activity-property new-context rule]
         (= new-context
            (-> (apply-rule-gen long-statement rule)
                (get-in ["context" "contextActivities" activity-property])))
      ;; Increment one single ID
      "parent" [{"id" "http://www.example.com/meetings/series/268"
                 "objectType" "Activity"}]
      {:location "$.context.contextActivities.parent[0].id"
       :all      ["http://www.example.com/meetings/series/268"]}
      ;; Increment potentially multiple IDs using wildcard
      "parent" [{"id" "http://www.example.com/meetings/series/268"
                 "objectType" "Activity"}
                {"id" "http://www.example.com/meetings/series/268"}
                {"id" "http://www.example.com/meetings/series/268"}
                {"id" "http://www.example.com/meetings/series/268"}
                {"id" "http://www.example.com/meetings/series/268"}
                {"id" "http://www.example.com/meetings/series/268"}
                {"id" "http://www.example.com/meetings/series/268"}]
      {:location "$.context.contextActivities.parent[*].id"
       :all      ["http://www.example.com/meetings/series/268"]}
      ;; Replace ID
      "category" [{"id" "rdc://vpugpmcqun.ikxdfny.bkq/qljkldinf" ; randomly gen
                   "objectType" "Activity"
                   "definition" {"name" {"en" "team meeting"}
                                 "description" {"en" "A category of meeting used for regular team meetings."}
                                 "type" "http://example.com/expapi/activities/meetingcategory"}}]
      {:location "$.context.contextActivities.category[0].id"
       :none     ["http://www.example.com/meetings/categories/teammeeting"]}
      ;; Replace two different IDs
      "other" [{"id" "http://www.example.com/meetings/occurances/bar"
                "objectType" "Activity"}
               {"id" "http://www.example.com/meetings/occurances/foo"
                "objectType" "Activity"}]
      {:location "$.context.contextActivities.other[0,1].id"
       :all      ["http://www.example.com/meetings/occurances/foo"
                  "http://www.example.com/meetings/occurances/bar"]}
      ;; Replace and insert IDs using wildcard
      "other" [{"id" "http://www.example.com/meetings/occurances/baz"
                "objectType" "Activity"}
               {"id" "http://www.example.com/meetings/occurances/bar"
                "objectType" "Activity"}
               {"id" "http://www.example.com/meetings/occurances/bar"}
               {"id" "http://www.example.com/meetings/occurances/qux"}
               {"id" "http://www.example.com/meetings/occurances/bar"}
               {"id" "http://www.example.com/meetings/occurances/foo"}
               {"id" "http://www.example.com/meetings/occurances/bar"}]
      {:location "$.context.contextActivities.other[*].id"
       :all      ["http://www.example.com/meetings/occurances/foo"
                  "http://www.example.com/meetings/occurances/bar"
                  "http://www.example.com/meetings/occurances/baz"
                  "http://www.example.com/meetings/occurances/qux"]}
      ;; Replace one ID with "any"
      "other" [{"id" "http://www.example.com/meetings/occurances/34257"
                "objectType" "Activity"}
               ;; selected via deteministed seeded rng
               {"id" "http://www.example.com/meetings/occurances/bar"
                "objectType" "Activity"}]
      {:location "$.context.contextActivities.other[1].id"
       :any      ["http://www.example.com/meetings/occurances/foo"
                  "http://www.example.com/meetings/occurances/bar"]}
      ;; Replace Activity definitions
      "category" [{"id"         "http://www.example.com/meetings/categories/teammeeting"
                   "objectType" "Activity"
                   "definition" {"name"        {"en" "team meeting"}
                                 "description" {"en" "foo"}
                                 "type"        "http://example.com/expapi/activities/meetingcategory"}}
                  {"definition" {"description" {"en" "foo"}}}]
      {:location "$.context.contextActivities.category[0,1].definition.description"
       :any      [{"en" "foo"}]}
      ;; Try creating multiple IDs, but only one ID is available
      ;; IDs should be distinct...
      "grouping" [{"id" "http://www.example.com/only-id"}
                  {"id" "http://www.example.com/only-id"}
                  {"id" "http://www.example.com/only-id"}]
      {:location "$.context.contextActivities.grouping[0,1,2].id"
       :presence "included"
       :all      ["http://www.example.com/only-id"]}
      ;; Same thing as above but skipping an entry
      "grouping" [{"id" "http://www.example.com/only-id"}
                  nil
                  {"id" "http://www.example.com/only-id"}]
      {:location "$.context.contextActivities.grouping[0,2].id"
       :presence "included"
       :all      ["http://www.example.com/only-id"]})))

;; Apply a collection of rules
(deftest apply-rule-coll-gen-test
  (testing "apply-rules-gen with multiple rules for Actors"
    ;; Turn the Actor into a Group using repeated rule applications
    (is (= {"name" "Alice Faux"
            "mbox" "mailto:alice@example.org"
            "objectType" "Group"
            "member" [{"mbox" "mailto:milt@yetanalytics.com"
                       "name" "milt"
                       "objectType" "Agent"}]}
           (-> short-statement
               (r/apply-rules-gen
                [{:location "$.actor.objectType"
                  :presence "included"
                  :all      ["Group"]}
                 {:location "$.actor.member[0]"
                  :presence "included"
                  :all      [{"mbox"       "mailto:milt@yetanalytics.com"
                              "name"       "milt"
                              "objectType" "Agent"}]}]
                :seed gen-seed)
               (get "actor")))))
  (testing "apply-rules-gen with multiple rules for Verbs"
    ;; Add two lang map entires at once
    (is (= {"id" "https://adlnet.gov/expapi/verbs/launched"
            "display" {"en-US" "Launched"
                       "zh-CN" "展开"}}
           (-> short-statement
               (r/apply-rules-gen
                [{:location "$.verb.display.en-US"
                  :all      ["Launched"]}
                 {:location "$.verb.display.zh-CN"
                  :all      ["展开"]}]
                :seed gen-seed)
               (get "verb"))))
    ;; Add, then try to remove, lang map entries
    (is (= {"id" "https://adlnet.gov/expapi/verbs/launched"
            "display" {"en-US" "Launched"
                       "zh-CN" "7gp0jt8lp77HHQ1mSe72yOdCrN"}} ; randomly gen
           (-> short-statement
               (r/apply-rules-gen
                [{:location "$.verb.display.en-US"
                  :all      ["Launched"]}
                 {:location "$.verb.display.zh-CN"
                  :all      ["展开"]}
                 {:location "$.verb.display.zh-CN"
                  :none     ["展开"]}]
                :seed gen-seed)
               (get "verb")))))
  (testing "apply-rules-gen with multiple rules for Context Activities"
    ;; two "any" rules - second "any" overwrites first
    (is (= [{"id" "http://www.example.com/meetings/occurances/bar"
             "objectType" "Activity"}
            {"id" "http://www.example.com/meetings/occurances/bar"
             "objectType" "Activity"}
            {"id" "http://www.example.com/meetings/occurances/bar"}
            {"id" "http://www.example.com/meetings/occurances/bar"}
            {"id" "http://www.example.com/meetings/occurances/bar"}
            {"id" "http://www.example.com/meetings/occurances/bar"}
            {"id" "http://www.example.com/meetings/occurances/bar"}]
           (-> long-statement
               (r/apply-rules-gen
                [{:location "$.context.contextActivities.other[*].id"
                  :any ["http://www.example.com/meetings/occurances/foo"]}
                 {:location "$.context.contextActivities.other[*].id"
                  :any ["http://www.example.com/meetings/occurances/bar"]}]
                :seed gen-seed)
               (get-in ["context" "contextActivities" "other"]))))
    ;; "all" followed by "any" - "any" overwrites "all"
    (is (= [{"id" "http://www.example.com/meetings/occurances/bar"
             "objectType" "Activity"}
            {"id" "http://www.example.com/meetings/occurances/bar"
             "objectType" "Activity"}
            {"id" "http://www.example.com/meetings/occurances/bar"}
            {"id" "http://www.example.com/meetings/occurances/bar"}
            {"id" "http://www.example.com/meetings/occurances/bar"}
            {"id" "http://www.example.com/meetings/occurances/bar"}
            {"id" "http://www.example.com/meetings/occurances/bar"}]
           (-> long-statement
               (r/apply-rules-gen
                [{:location "$.context.contextActivities.other[*].id"
                  :all ["http://www.example.com/meetings/occurances/foo"]}
                 {:location "$.context.contextActivities.other[*].id"
                  :any ["http://www.example.com/meetings/occurances/bar"]}]
                :seed gen-seed)
               (get-in ["context" "contextActivities" "other"]))))
    ;; "any" followed by "all" - "all" overwrites "any"
    (is (= [{"id" "http://www.example.com/meetings/occurances/bar"
             "objectType" "Activity"}
            {"id" "http://www.example.com/meetings/occurances/bar"
             "objectType" "Activity"}
            {"id" "http://www.example.com/meetings/occurances/bar"}
            {"id" "http://www.example.com/meetings/occurances/bar"}
            {"id" "http://www.example.com/meetings/occurances/bar"}
            {"id" "http://www.example.com/meetings/occurances/bar"}
            {"id" "http://www.example.com/meetings/occurances/bar"}]
           (-> long-statement
               (r/apply-rules-gen
                [{:location "$.context.contextActivities.other[*].id"
                  :any ["http://www.example.com/meetings/occurances/foo"]}
                 {:location "$.context.contextActivities.other[*].id"
                  :all ["http://www.example.com/meetings/occurances/bar"]}]
                :seed gen-seed)
               (get-in ["context" "contextActivities" "other"]))))
    ;; two "all" rules - second "all" overwrites first
    (is (= [{"id" "http://www.example.com/meetings/occurances/bar"
             "objectType" "Activity"}
            {"id" "http://www.example.com/meetings/occurances/bar"
             "objectType" "Activity"}
            {"id" "http://www.example.com/meetings/occurances/bar"}
            {"id" "http://www.example.com/meetings/occurances/bar"}
            {"id" "http://www.example.com/meetings/occurances/bar"}
            {"id" "http://www.example.com/meetings/occurances/bar"}
            {"id" "http://www.example.com/meetings/occurances/bar"}]
           (-> long-statement
               (r/apply-rules-gen
                [{:location "$.context.contextActivities.other[*].id"
                  :all ["http://www.example.com/meetings/occurances/foo"]}
                 {:location "$.context.contextActivities.other[*].id"
                  :all ["http://www.example.com/meetings/occurances/bar"]}]
                :seed gen-seed)
               (get-in ["context" "contextActivities" "other"]))))))

(deftest apply-rules-gen-exception-test
  (testing "apply-rules-gen throws exceptions if rules are invalid"
    ;; Flat-out invalid Statement property
    (is (= ::r/gen-error
           (try (r/apply-rules-gen
                 short-statement
                 [{:location "$.object.zooWeeMama"
                   :presence "included"}]
                 :seed gen-seed)
                nil
                (catch ExceptionInfo e (-> e ex-data :type)))))
    ;; Actor name in an Activity object
    (is (= ::r/gen-error
           (try (r/apply-rules-gen
                 short-statement
                 [{:location "$.object.name"
                   :presence "included"}]
                 :seed gen-seed)
                nil
                (catch ExceptionInfo e (-> e ex-data :type)))))
    ;; Activity definition in an Actor object
    (is (= ::r/gen-error
           (try (r/apply-rules-gen
                 short-statement
                 [;; First replace the Activity object with an Actor object
                  {:location "$.object"
                   :all      [{"objectType" "Agent"
                               "name"       "Owen Overrider"
                               "mbox"       "mailto:owoverrider@example.com"}]}
                  {:location "$.object.definition.type"
                   :presence "included"}]
                 :seed gen-seed)
                nil
                (catch ExceptionInfo e (-> e ex-data :type)))))
    ;; Try to generate with gaps in the array
    ;; TODO: Add another test case where this rule also has a grouping[0]
    ;; counterpart
    ;; FIXME: Should either do one of the following:
    ;; a) create dummy values to fill in the gaps
    ;; b) throw/return a validation error for invalid rule/template
    ;; c) throw/return an error that's more descriptive than IndexOutOfBounds
    (is (try (r/apply-rules-gen
              long-statement
              [{:location "$.context.contextActivities.grouping[1].definition.type"
                :presence "included"
                :all      ["https://xapinet.com/xapi/blooms/activitytypes/cognitive-process-dimension"]}]
              :seed gen-seed)
             false
             (catch IndexOutOfBoundsException _ true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CMI5 Rule Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We can pull some actual rules from cmi5

(def cmi5-templates
  (:templates const/cmi5-profile))

(def cmi5-template-rules
  (mapcat :rules cmi5-templates))

(def cmi5-statement ; simple statement used for cmi5 tests
  (with-open [r (io/reader const/simple-statement-filepath)]
    (json/parse-stream r)))

(deftest cmi5-rule-tests
  ;; Individual Rule Parsing
  (testing "parse cmi5 rules"
    (is (every?
         (comp nil? (partial s/explain-data ::r/parsed-rule))
         (map r/parse-rule cmi5-template-rules))))
  ;; Individual Rule Application
  (testing "apply cmi5 Rule:"
    (doseq [{:keys [rules]} cmi5-templates
            rule rules]
      (testing (format "Rule: %s" rule)
        (let [processed (r/apply-rules-gen cmi5-statement [rule] :seed gen-seed)]
          (is (r/follows-rule? processed (r/parse-rule rule)))
          (is (nil? (s/explain-data ::xs/statement processed)))))))
  ;; Collected Rules Application
  (testing "apply cmi5 Template:"
    (doseq [{:keys [id rules]} cmi5-templates]
      (testing (format "Template: %s" id)
        (let [processed (r/apply-rules-gen cmi5-statement rules :seed gen-seed)]
          (is (every? (partial r/follows-rule? processed)
                      (map r/parse-rule rules)))
          (is (nil? (s/explain-data ::xs/statement processed))))))))

(comment
  (r/apply-rules-gen
   {"id" "fd41c918-b88b-4b20-a0a5-a4c32391aaa0",
    "timestamp" "2015-11-18T12:17:00+00:00",
    "actor"
    {"objectType" "Agent",
     "name" "Project Tin Can API",
     "mbox" "mailto:user@example.com"},
    "verb"
    {"id" "http://example.com/xapi/verbs#sent-a-statement",
     "display" {"en-US" "sent"}},
    "object"
    {"id" "http://example.com/xapi/activity/simplestatement",
     "definition"
     {"name" {"en-US" "simple statement"},
      "description"
      {"en-US"
       "A simple Experience API statement. Note that the LRS \n\t\t\t\tdoes not need to have any prior information about the Actor (learner), the \n\t\t\t\tverb, or the Activity/object."}}}}
   [{:location "$.object.definition.type"}]
   #_[{:location "$.verb.display.en-US", :all ["Launched"]}
    {:location "$.verb.display.zh-CN", :all ["展开"]}
    {:location "$.verb.display.zh-CN", :none ["展开"]}]
   #_[{:location "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchmode']"
       :presence "included"
       :all      ["Review" "Normal" "Browse"]}]
   :seed gen-seed) 

   (r/follows-rule?
    {"id" "fd41c918-b88b-4b20-a0a5-a4c32391aaa0",
     "timestamp" "2015-11-18T12:17:00+00:00",
     "actor"
     {"objectType" "Agent",
      "name" "Project Tin Can API",
      "mbox" "mailto:user@example.com"},
     "verb"
     {"id" "http://example.com/xapi/verbs#sent-a-statement",
      "display" {"en-US" "sent"}},
     "object"
     {"objectType" "Agent"
      "name"       "Owen Overrider"
      "mbox"       "mailto:owoverrider@example.com"}}
    (r/parse-rule {:location "$.object.definition.type"
                   :presence "included"}))

   (r/apply-rules-gen
    {"id" "fd41c918-b88b-4b20-a0a5-a4c32391aaa0",
     "timestamp" "2015-11-18T12:17:00+00:00",
     "actor"
     {"objectType" "Agent",
      "name" "Project Tin Can API",
      "mbox" "mailto:user@example.com"},
     "verb"
     {"id" "http://example.com/xapi/verbs#sent-a-statement",
      "display" {"en-US" "sent"}},
     "object"
     {"objectType" "Agent"
      "name"       "Owen Overrider"
      "mbox"       "mailto:owoverrider@example.com"}}
    [{:location "$.object.definition.type"
      :presence "included"}]
    #_[{:location "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/launchmode']"
        :presence "included"
        :all      ["Review" "Normal" "Browse"]}]
    :seed gen-seed))
