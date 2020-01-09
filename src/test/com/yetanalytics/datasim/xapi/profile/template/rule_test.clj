(ns com.yetanalytics.datasim.xapi.profile.template.rule-test
  (:require
   [clojure.test :refer :all]
   [com.yetanalytics.datasim.xapi.profile.template.rule :refer :all :as r]
   [com.yetanalytics.datasim.input :as input]
   [clojure.java.io :as io]
   [clojure.data.json :as json]
   [clojure.spec.alpha :as s]
   [xapi-schema.spec :as xs]
   ))

;; we can pull some actual rules from cmi5
(def cmi5-templates
  (:templates
   (input/from-location :profile :json "dev-resources/profiles/cmi5/fixed.json")))

(def all-rules
  (mapcat :rules cmi5-templates))

;; A simple statement to augment
(def simple-statement
  (with-open [r (io/reader "dev-resources/xapi/statements/simple.json")]
    (json/read r)))

(deftest parse-rule-test
  (testing "parses cmi5 rules"
    (is (every?
         (comp nil?
               (partial s/explain-data ::r/parsed-rule))
         (map parse-rule
              all-rules)))))

(deftest apply-rules-gen-test
  (testing "individual rule application"
    (doseq [{:keys [rules]} cmi5-templates
            rule rules]
      (testing (format "rule: %s" rule)
        (let [processed (apply-rules-gen simple-statement [rule] :seed 42)]
          (testing "follows rule"
            (is (follows-rule? processed (parse-rule rule))))
          (testing "is a valid statement"
            (is (nil? (s/explain-data ::xs/statement processed))))))))

  (testing "collected rule application"
    (doseq [{:keys [id
                    rules]} cmi5-templates]
      (testing (format "template: %s" id)
        (let [processed (apply-rules-gen simple-statement rules :seed 42)]
          (testing "follows rules"
            (is (every? (partial follows-rule? processed) (map parse-rule
                                                               rules))))
          (testing "is a valid statement"
            (is (nil? (s/explain-data ::xs/statement processed))))))))
  (testing "various cases"
    (are [case-name statement rules]
        (let [processed (apply-rules-gen simple-statement rules :seed 42)]
          (and (every? (partial follows-rule? processed) (map parse-rule
                                                              rules))
               (is (nil? (s/explain-data ::xs/statement processed)))
               case-name))
      "group as actor" simple-statement [;; TODO: Right now this will only apply the second rule if the first is present
                                         ;; the path->spec function can't resolve ["actor" "member"]
                                         {:location "$.actor.objectType"
                                          :presence "included"
                                          :all ["Group"]}
                                         {:location "$.actor.member[0]"
                                          :presence "included"
                                          :all [{"mbox" "mailto:milt@yetanalytics.com"
                                                 "name" "milt"
                                                 "objectType" "Agent"}]}])))
