(ns com.yetanalytics.datasim.xapi.profile.template.rule-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [clojure.spec.alpha :as s]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.xapi.profile.template.rule :as r]
            [com.yetanalytics.datasim.input :as input]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixtures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def gen-seed 42)

(def example-statement
  {"id"        "59de1b06-bb6c-4708-a51a-b3d403c491db",
   "actor"     {"name" "Alice Faux"
                "mbox" "mailto:alice@example.org"},
   "verb"      {"id" "https://adlnet.gov/expapi/verbs/launched"},
   "object"    {"id"         "https://example.org/career/1054719918",
                "definition" {"type" "https://w3id.org/xapi/tla/activity-types/career"},
                "objectType" "Activity"},
   "context"   {"registration" "d7acfddb-f4c2-49f4-a081-ad1fb8490448"},
   "timestamp" "2021-03-18T17:36:22.131Z"})

;; we can pull some actual rules from cmi5
(def cmi5-templates
  (:templates
   (input/from-location :profile :json "dev-resources/profiles/cmi5/fixed.json")))

(def all-rules
  (mapcat :rules cmi5-templates))

;; A simple statement to augment
(def simple-statement
  (with-open [r (io/reader "dev-resources/xapi/statements/simple.json")]
    (json/parse-stream r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest parse-rule-test
  (testing "parses cmi5 rules"
    (is (every?
         (comp nil? (partial s/explain-data ::r/parsed-rule))
         (map r/parse-rule all-rules)))))

(deftest follows-rule?-test
  (testing "given a statement that follows the rule, it returns true"
    (is (r/follows-rule?
         example-statement
         {:presence "excluded",
          :location [#{"context"} #{"contextActivities"}]})))
  (testing "given a statement that doesn't follow the rule, it returns false"
    (is (not
         (r/follows-rule?
          (assoc-in example-statement
                    ["context" "contextActivities" "category"]
                    [{"id" "https://w3id.org/xapi/tla/v0.13"}])
          {:presence "excluded",
           :location [#{"context"} #{"contextActivities"}]})))))

(deftest apply-rules-gen-test
  ;; Individual Rule Application
  (doseq [{:keys [rules]} cmi5-templates
          rule rules]
    (testing (format "Rule: %s" rule)
      (let [processed (r/apply-rules-gen simple-statement [rule] :seed gen-seed)]
        (is (r/follows-rule? processed (r/parse-rule rule)))
        (is (nil? (s/explain-data ::xs/statement processed))))))
  ;; Collected Rules Application
  (doseq [{:keys [id rules]} cmi5-templates]
    (testing (format "Template: %s" id)
      (let [processed (r/apply-rules-gen simple-statement rules :seed gen-seed)]
        (is (every? (partial r/follows-rule? processed)
                    (map r/parse-rule rules)))
        (is (nil? (s/explain-data ::xs/statement processed))))))
  ;; Various Cases
  (testing "Case: Group as Actor"
    (let [rules [;; TODO: Right now this will only apply the second rule if the first is present
                 ;; the path->spec function can't resolve ["actor" "member"]
                 {:location "$.actor.objectType"
                  :presence "included"
                  :all      ["Group"]}
                 {:location "$.actor.member[0]"
                  :presence "included"
                  :all      [{"mbox"       "mailto:milt@yetanalytics.com"
                              "name"       "milt"
                              "objectType" "Agent"}]}]
          processed (r/apply-rules-gen simple-statement rules :seed gen-seed)]
      (is (every? (partial r/follows-rule? processed)
                  (map r/parse-rule rules)))
      (is (nil? (s/explain-data ::xs/statement processed))))))
