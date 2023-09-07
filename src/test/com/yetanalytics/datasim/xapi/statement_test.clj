(ns com.yetanalytics.datasim.xapi.statement-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [clojure.walk :as w]
            [java-time.api :as t]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.math.random :as random]
            [com.yetanalytics.datasim.xapi.statement :refer [generate-statement]]
            [com.yetanalytics.datasim.xapi.profile :as profile]
            [com.yetanalytics.datasim.test-constants :as const]))

;; FIXME: generate-statement will still generate statements with blatantly contradictory rules,
;; e.g.
;; {:rules [{:location "$.id" :presence "included" :none ["3829c803-1f4c-44ed-8d8f-36e502cadd0f"]}
;;          {:location "$.id" :presence "included" :all ["3829c803-1f4c-44ed-8d8f-36e502cadd0f"]}}}]}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def top-seed 42)

(def actor
  (-> const/simple-input :personae-array first :member first (dissoc :role)))

(def alignments
  (reduce
   (fn [acc {:keys [id weight]}]
     (assoc-in acc [:weights id] weight))
   {:weights {}}
   (get-in const/simple-input [:models 0 :alignments])))

(def profiles-map
  (profile/profiles->profile-map (:profiles const/simple-input)
                                 (:parameters const/simple-input)
                                 100))

(def default-template
  (get-in profiles-map [:type-iri-map
                        "StatementTemplate"
                        "https://w3id.org/xapi/cmi5#satisfied"]))

(def pattern-ancestors
  [{:id      "https://w3id.org/xapi/cmi5#toplevel"
    :primary true}
   {:id      "https://w3id.org/xapi/cmi5#satisfieds"
    :primary false}])

(def registration
  "01234567-0000-4000-8000-123456789012")

(def arguments
  (merge profiles-map
         {:actor             actor
          :alignments        alignments
          :timestamp         (t/instant 0)
          :timezone          "UTC"
          :time-since-last   (t/duration 60000)
          :seed              top-seed
          :template          default-template
          :pattern-ancestors pattern-ancestors
          :registration      registration}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regular Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- gen-statement [partial-template]
  (->> partial-template
       (merge {:id       "https://template-1"
               :type     "StatementTemplate"
               :inScheme "https://w3id.org/xapi/cmi5/v1.0"})
       (assoc arguments :template)
       generate-statement))

(defn- statement-inputs? [statement]
  (and (= "1970-01-01T00:00:00Z"
          (get statement "timestamp"))
       (= {"name" "Bob Fakename" "mbox" "mailto:bobfake@example.org"}
          (get statement "actor"))
       (= "01234567-0000-4000-8000-123456789012"
          (get-in statement ["context" "registration"]))
       (= {"id" "https://w3id.org/xapi/cmi5/v1.0"}
          (-> statement
              (get-in ["context" "contextActivities" "category"])
              last))))

(deftest generate-statement-test
  (testing "Template specifies no properties or rules"
    (let [statement (gen-statement {})]
      (is (s/valid? ::xs/statement statement))
      (is (statement-inputs? statement))
      ;; The statement ID should be generated deterministically via rng
      (is (= "aee7bbe1-0c45-4028-8f08-3ce3f12bbb4b"
             (get statement "id")))))
  
  (testing "Template specifies ID"
    (let [statement (gen-statement {:rules [{:location "$.id"
                                             :presence "included"}]})]
      (is (s/valid? ::xs/statement statement))
      (is (statement-inputs? statement))
      ;; The statement ID should be generated deterministically via rng
      (is (= "aee7bbe1-0c45-4028-8f08-3ce3f12bbb4b"
             (get statement "id")))))

  ;; Actor
  
  (testing "Template specifies Actor"
    ;; TODO: Throw an error or show warning if actor rules exist?
    (testing "name and mbox property - always overwritten"
      (let [statement
            (gen-statement {:rules [{:location "$.actor.name"
                                     :all      ["Alice Faux"]}
                                    {:location "$.actor.mbox"
                                     :all      ["mailto:alice@example.org"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (not= "Alice Faux"
                  (get-in statement ["actor" "name"])))
        (is (not= "mailto:alice@example.org"
                  (get-in statement ["actor" "mbox"]))))))
  
  ;; Verb 

  (testing "Template specifies Verb"
    (testing "ID property"
      (let [statement
            (gen-statement {:verb "https://w3id.org/xapi/adl/verbs/satisfied"})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id" "https://w3id.org/xapi/adl/verbs/satisfied"
                "display" {"en" "satisfied"}}
               (get statement "verb")))))

    (testing "ID property outside of profile"
      (let [statement
            (gen-statement {:verb "https://example.org/custom-verb"})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id" "https://example.org/custom-verb"}
               (get statement "verb")))))

    (testing "ID rule"
      (let [statement
            (gen-statement
             {:rules [{:location "$.verb.id"
                       :all      ["https://w3id.org/xapi/adl/verbs/satisfied"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id" "https://w3id.org/xapi/adl/verbs/satisfied"
                "display" {"en" "satisfied"}}
               (get statement "verb")))))

    (testing "ID property and ID rule - rule overwrites property"
      (let [statement
            (gen-statement
             {:verb  "https://w3id.org/xapi/adl/verbs/satisfied"
              :rules [{:location "$.verb.id"
                       :all      ["https://w3id.org/xapi/adl/verbs/satisfied"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id" "https://w3id.org/xapi/adl/verbs/satisfied"
                "display" {"en" "satisfied"}}
               (get statement "verb")))))

    (testing "ID property and display rule"
      (let [statement
            (gen-statement
             {:verb  "https://w3id.org/xapi/adl/verbs/satisfied"
              :rules [{:location "$.verb.display"
                       :all      [{"es" "satisfecho"}]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id" "https://w3id.org/xapi/adl/verbs/satisfied"
                "display" {"en" "satisfied"
                           "es" "satisfecho"}}
               (get statement "verb")))))

    (testing "inclusion only"
      (let [statement
            (gen-statement
             {:rules [{:location "$.verb"
                       :presence "included"}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id"      "https://w3id.org/xapi/adl/verbs/abandoned"
                "display" {"en" "abandoned"}}
               (get statement "verb")))))
    
    (testing "ID inclusion only"
      (let [statement
            (gen-statement
             {:rules [{:location "$.verb.id"
                       :presence "included"}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id"      "https://w3id.org/xapi/adl/verbs/satisfied"
                "display" {"en" "satisfied"}}
               (get statement "verb")))))
    
    (testing "display rule only - spec generation"
      (let [statement
            (gen-statement
             {:rules [{:location "$.verb.display"
                       :all      [{"en-us" "Custom Verb"
                                   "en-uk" "Verb that is Custom"}]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id" "lctlar://lyubpwtqn.pqzkisi.lku/szcokfsgupq" ; ID is randomly generated
                "display" {"en-us" "Custom Verb"
                           "en-uk" "Verb that is Custom"}}
               (get statement "verb")))))

    (testing "display exclusion rule"
      (let [statement
            (gen-statement
             {:verb  "https://w3id.org/xapi/adl/verbs/satisfied"
              :rules [{:location "$.verb.display"
                       :presence "excluded"}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id" "https://w3id.org/xapi/adl/verbs/satisfied"}
               (get statement "verb")))))

    (testing "ID exclusion rule - invalid"
      (let [statement
            (gen-statement
             {:verb  "https://w3id.org/xapi/adl/verbs/satisfied"
              :rules [{:location "$.verb.id"
                       :presence "excluded"}]})]
        (is (statement-inputs? statement))
        (is (= {"display" {"en" "satisfied"}}
               (get statement "verb")))
        ;; This rule is egregiously invalid
        (is (not (s/valid? ::xs/statement statement))))))

  ;; Activity Object
  
  (testing "Template specifies Activity Object"
    (testing "activity type property"
      (let [statement
            (gen-statement
             {:objectActivityType "https://w3id.org/xapi/cmi5/activitytype/course"})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id"         "https://example.org/course/1671689032"
                "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}
               (get statement "object")))))

    (testing "activity type rule"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.definition.type"
                       :all      ["https://w3id.org/xapi/cmi5/activitytype/course"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id"         "https://example.org/course/1671689032"
                "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}
               (get statement "object")))))

    (testing "activity type and objectType rules"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.definition.type"
                       :all      ["https://w3id.org/xapi/cmi5/activitytype/course"]}
                      {:location "$.object.objectType"
                       :any      ["Activity"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id"         "https://example.org/course/1671689032"
                "objectType" "Activity"
                "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}
               (get statement "object")))))

    (testing "ID rule"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.id"
                       :all      ["https://example.org/course/1671689032"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id"         "https://example.org/course/1671689032"
                "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}
               (get statement "object")))))

    (testing "definition rules only (non-activity type) - spec generation"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.definition.name"
                       :all      [{"en" "Custom Activity"}]}
                      {:location "$.object.definition.description"
                       :all      [{"en" "This is a custom activity used to test statement generation"}]}
                      {:location "$.object.definition.moreInfo"
                       :all      ["http://example.org/more-activity-info"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id"         "dmnc://wkjfbiuz.vxxlao.duj/bepzenzgmci" ; The ID is randomly generated
                "objectType" "Activity"
                "definition" {"name"        {"en" "Custom Activity"}
                              "description" {"en" "This is a custom activity used to test statement generation"}
                              "moreInfo"    "http://example.org/more-activity-info"}}
               (get statement "object")))))

    (testing "activity type property and ID rule"
      (let [statement
            (gen-statement
             {:objectActivityType "https://w3id.org/xapi/cmi5/activitytype/course"
              :rules [{:location "$.object.id"
                       :all      ["https://example.org/course/1550503926"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id"         "https://example.org/course/1550503926"
                "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}
               (get statement "object")))))

    (testing "ID and objectType rule"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.id"
                       :all      ["https://example.org/course/1432714272"]}
                      {:location "$.object.objectType"
                       :any      ["Activity"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id"         "https://example.org/course/1432714272"
                "objectType" "Activity"
                "definition" {"type" "https://w3id.org/xapi/cmi5/activities/course"}}
               (get statement "object")))))

    (testing "inclusion only"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object"
                       :presence "included"}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id"         "https://example.org/block/1328449717"
                "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/block"}}
               (get statement "object")))))

    (testing "ID inclusion only"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.id"
                       :presence "included"}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id"         "https://example.org/block/418707894"
                "definition" {"type" "https://w3id.org/xapi/cmi5/activities/block"}}
               (get statement "object")))))

    (testing "activity type inclusion only"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.definition.type"
                       :presence "included"}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id"         "https://example.org/block/1328449717"
                "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/block"}}
               (get statement "object"))))))

  ;; Agent/Group Object
  
  (testing "Template specifies Agent/Group Object"
    (testing "name and objectType rule"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.name"
                       :all      ["Andrew Downes" "Toby Nichols" "Ena Hills"]}
                      {:location "$.object.objectType"
                       :any      ["Agent" "Group"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        ;; objectType, name, and IFI were chosen + generated by rng
        (is (= {"objectType" "Agent"
                "name"       "Ena Hills"
                "account"    {"name"     "9"
                              "homePage" "xvijclmo://jlfhg.rdkqfjq.hodx/emmpg"}}
               (get statement "object")))))

    (testing "objectType rule"
      (let [statement-1
            (gen-statement
             {:rules [{:location "$.object.objectType"
                       :any      ["Agent"]}]})
            statement-2
            (gen-statement
             {:rules [{:location "$.object.objectType"
                       :any      ["Group"]}]})]
        (is (s/valid? ::xs/statement statement-1))
        (is (s/valid? ::xs/statement statement-2))
        (is (statement-inputs? statement-1))
        (is (statement-inputs? statement-2))
        ;; objectType, name, and IFI were chosen + generated by rng
        (is (= {"objectType" "Agent"
                "name"       "Z"
                "openid"     "http://ngfx.ibfoget.qpin/jyrpvftharj"}
               (get statement-1 "object")))
        (is (= {"objectType" "Group"
                "name"       "Z"
                "member"     [] ; FIXME: This is a generation flaw in xapi-schema...
                "account"    {"name"     "9J"
                              "homePage" "olwhn://niv.zsyugxjy.jgto/syyeoigztbu"}}
               (get statement-2 "object")))))

    (testing "name rule"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.name"
                       :all      ["Andrew Downes" "Toby Nichols" "Ena Hills"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        ;; objectType, name, and IFI were chosen + generated by rng
        ;; Agent is chosen first since that's the default for objects w/ "name"
        (is (= {"objectType" "Agent"
                "name"       "Ena Hills"
                "openid"     "http://ngfx.ibfoget.qpin/jyrpvftharj"}
               (get statement "object")))))

    (testing "mbox rule"
      (let [statement (gen-statement {:rules [{:location "$.object.mbox"
                                               :presence "included"}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"objectType" "Agent"
                "mbox"       "mailto:dgykibt@scade.ppiq"}
               (get statement "object")))))

    (testing "mbox_sha1sum rule"
      (let [statement (gen-statement {:rules [{:location "$.object.mbox_sha1sum"
                                               :presence "included"}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"objectType"   "Agent"
                "mbox_sha1sum" "43657A3CB4AFAF7B2874E3EE7EA90E45BE31E740"}
               (get statement "object")))))

    (testing "openid rule"
      (let [statement (gen-statement {:rules [{:location "$.object.openid"
                                               :presence "included"}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"objectType" "Agent"
                "openid"     "https://scan.wzkahpj.ropx/yidyiuaofrug"}
               (get statement "object")))))

    (testing "account rule"
      (let [statement (gen-statement {:rules [{:location "$.object.account"
                                               :presence "included"}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"objectType" "Agent"
                "account"    {"name"     "N46CD51RXVjD"
                              "homePage" "zrderkyg://ndig.mcgovvox.dzvo/svagnafrrphkcf"}}
               (get statement "object")))))

    (testing "objectType + account name rule"
      (let [statement-1
            (gen-statement {:rules [{:location "$.object.account.name"
                                     :all      ["Agent Account Name"]}]})
            statement-2
            (gen-statement {:rules [{:location "$.object.objectType"
                                     :all      ["Group"]}
                                    {:location "$.object.account.name"
                                     :all      ["Group Account Name"]}]})]
        (is (s/valid? ::xs/statement statement-1))
        (is (s/valid? ::xs/statement statement-2))
        (is (statement-inputs? statement-1))
        (is (statement-inputs? statement-2))
        (is (= {"objectType" "Agent"
                "account"    {"name"     "Agent Account Name" ; homePage is randomly generated
                              "homePage" "dguy://gqaxtmpji.lsnzq.gdyj/baicdpq"}}
               (get statement-1 "object")))
        (is (= {"objectType" "Group"
                "account"    {"name"     "Group Account Name" ; homePage is randomly generated
                              "homePage" "mhqttko://mfyvhxsgxj.wpl.gnll/xlumwozhnvx"}}
               (get statement-2 "object")))))

    (testing "account homePage rule"
      (let [statement-1
            (gen-statement {:rules [{:location "$.object.account.homePage"
                                     :all      ["http://example.org/agent"]}]})
            statement-2
            (gen-statement {:rules [{:location "$.object.objectType"
                                     :all      ["Group"]}
                                    {:location "$.object.account.homePage"
                                     :all      ["http://example.org/group"]}]})]
        (is (s/valid? ::xs/statement statement-1))
        (is (s/valid? ::xs/statement statement-2))
        (is (statement-inputs? statement-1))
        (is (statement-inputs? statement-2))
        (is (= {"objectType" "Agent"
                "account"    {"name"     "75" ; name is randomly generated
                              "homePage" "http://example.org/agent"}}
               (get statement-1 "object")))
        (is (= {"objectType" "Group"
                "account"    {"name"     "0" ; name is randomly generated
                              "homePage" "http://example.org/group"}}
               (get statement-2 "object")))))

    (testing "member rule"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.member"
                       :all      [[{"name" "Group Member"
                                    "mbox" "mailto:group@example.com"}]]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"objectType" "Group"
                "member"     [{"name"       "Group Member"
                               "mbox"       "mailto:group@example.com"
                               "objectType" "Agent"}]}
               (get statement "object")))))

    (testing "member IFI rules"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.member.*.mbox"
                       :all      ["mailto:one@example.com"
                                  "mailto:two@example.com"
                                  "mailto:three@example.com"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        ;; IFIs like "mbox" are considered a distinct property, so they
        ;; cannot be repeated
        (is (= {"objectType" "Group"
                "member"
                [{"mbox"       "mailto:two@example.com"
                  "objectType" "Agent"}]}
               (get statement "object")))))

    (testing "member name rules"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.member.*.name"
                       :all      ["Number One" "Number Two" "Number Three"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        ;; "name" is considered a distinct property, like IDs and IFIs
        ;; The IFIs are randomly generated and each name is randomly chosen,
        ;; as is the number of names
        (is (= {"objectType" "Group"
                "member"
                [{"objectType" "Agent"
                  "name"       "Number One"
                  "openid"     "http://ngfx.ibfoget.qpin/jyrpvftharj"}]}
               (get statement "object")))))

    (testing "member account name rules"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.member.*.account.name"
                       :all      ["Number One" "Number Two" "Number Three"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        ;; account "name", not just Agent/Group name is considered distinct
        ;; The homePages are randomly generated and each name is randomly
        ;; chosen, as is the number of names
        (is (= {"objectType" "Group"
                "member"
                [{"account"    {"name"     "Number One"
                                "homePage" "dguy://gqaxtmpji.lsnzq.gdyj/baicdpq"}
                  "objectType" "Agent"}]}
               (get statement "object"))))))

  ;; StatementRef Object
  
  (testing "Template specifies StatementRef"
    (testing "ID and objectType rule"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.id"
                       :presence "included"
                       :all      ["00000000-0000-4000-8000-123412341234"]}
                      {:location "$.object.objectType"
                       :presence "included"
                       :any      ["StatementRef"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id"         "00000000-0000-4000-8000-123412341234"
                "objectType" "StatementRef"}
               (get statement "object")))))
    
    (testing "objectType rule"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.objectType"
                       :presence "included"
                       :any      ["StatementRef"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {; ID is randomly generated
                "id"         "b13fc9e2-20ba-4a54-9d6d-5906e60a620a"
                "objectType" "StatementRef"}
               (get statement "object")))))

    (testing "ID and multiple objectTypes - invalid"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.id"
                       :presence "included"}
                      {:location "$.object.objectType"
                       :presence "included"
                       :any      ["Activity" "StatementRef"]}]})]
        (is (statement-inputs? statement))
        ;; This template actually fails validation, since an activity ID was
        ;; chosen (since "Activity" is present in `any`) but the objectType
        ;; was separately and randomly chosen from `any`
        (is (not (s/valid? ::xs/statement statement)))
        (is (= {"id"         "https://example.org/block/418707894"
                "objectType" "StatementRef"}
               (get statement "object")))))

    (testing "UUID ID only - invalid"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.id"
                       :presence "included"
                       :all      ["00000000-0000-4000-8000-123412341234"]}]})]
        (is (statement-inputs? statement))
        ;; This template actually fails validation, since it's treated as
        ;; an Activity but the ID is not an activity ID...
        (is (not (s/valid? ::xs/statement statement)))
        (is (= {"id" "00000000-0000-4000-8000-123412341234"}
               (get statement "object"))))))

  ;; SubStatement Object
  
  (testing "Template specifies SubStatement Object"
    (testing "objectType rule"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.objectType"
                       :presence "included"
                       :any      ["SubStatement"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        ;; The SubStatement has randomly chosen verb and object properties
        (is (= {"objectType" "SubStatement"
                "actor"      {"name" "Bob Fakename"
                              "mbox" "mailto:bobfake@example.org"}
                "verb"       {"id"      "https://w3id.org/xapi/adl/verbs/waived"
                              "display" {"en" "waived"}}
                "object"     {"id"         "https://example.org/course/1432714272"
                              "definition" {"type" "https://w3id.org/xapi/cmi5/activities/course"}}}
               (get statement "object")))))

    (testing "objectType + Activity object"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.objectType"
                       :presence "included"
                       :any      ["SubStatement"]}
                      {:location "$.object.object.id"
                       :presence "included"
                       :any      ["https://example.org/course/1432714272"]}
                      {:location "$.object.object.objectType"
                       :presence "included"
                       :any      ["Activity"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= "SubStatement"
               (get-in statement ["object" "objectType"])))
        (is (= {"id"         "https://example.org/course/1432714272"
                "objectType" "Activity"
                "definition" {"type" "https://w3id.org/xapi/cmi5/activities/course"}}
               (get-in statement ["object" "object"])))))

    (testing "Agent object"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.object.objectType"
                       :all      ["Agent"]}
                      {:location "$.object.object.name"
                       :presence "included"
                       :any      ["Andrew Downes" "Toby Nichols" "Ena Hills"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= "SubStatement"
               (get-in statement ["object" "objectType"])))
        (is (= {"name"       "Ena Hills"
                "objectType" "Agent"
                "openid"     "https://ijw.rbizkif.xgt/etjgqieuegeywj"}
               (get-in statement ["object" "object"])))))

    (testing "Group object"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.object.objectType"
                       :all      ["Group"]}
                      {:location "$.object.object.member"
                       :presence "included"
                       :any      [[{"name" "Group Member"
                                    "mbox" "mailto:group@example.com"}]]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= "SubStatement"
               (get-in statement ["object" "objectType"])))
        (is (= {"objectType" "Group"
                "member"     [{"name"       "Group Member"
                               "mbox"       "mailto:group@example.com"
                               "objectType" "Agent"}]}
               (get-in statement ["object" "object"])))))
    
    (testing "Group object w/ only member rules"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.object.member"
                       :presence "included"
                       :any      [[{"name" "Group Member"
                                    "mbox" "mailto:group@example.com"}]]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= "SubStatement"
               (get-in statement ["object" "objectType"])))
        (is (= {"objectType" "Group"
                "member"     [{"name"       "Group Member"
                               "mbox"       "mailto:group@example.com"
                               "objectType" "Agent"}]}
               (get-in statement ["object" "object"])))))

    (testing "Agent or Group object"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.object.name"
                       :presence "included"}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= "SubStatement"
               (get-in statement ["object" "objectType"])))
        ;; Properties are all spec generated
        (is (= {"objectType" "Agent"
                "name"       "9ushxJMzhBbYBf93yGO9K1Wk5"
                "account"    {"name"     "9"
                              "homePage" "xvijclmo://jlfhg.rdkqfjq.hodx/emmpg"}}
               (get-in statement ["object" "object"])))))

    (testing "StatementRef object"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.object.objectType"
                       :all      ["StatementRef"]}
                      {:location "$.object.object.id"
                       :any      ["00000000-4000-4000-8000-000000000000"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= "SubStatement"
               (get-in statement ["object" "objectType"])))
        (is (= {"id"         "00000000-4000-4000-8000-000000000000"
                "objectType" "StatementRef"}
               (get-in statement ["object" "object"])))))
    
    (testing "Context and Attachment rules"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.context.language"
                       :all      ["en-US"]}
                      {:location "$.object.attachments[0].usageType"
                       :any      ["http://example.com/substatement-usage-type"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"language" "en-US"}
               (get-in statement ["object" "context"])))
        ;; attachment is randomly generated
        (is (= [{"usageType"   "http://example.com/substatement-usage-type"
                 "description" {"en-US" "V"}
                 "display"     {"en-GB" "Qv"
                                "en-US" "H"}
                 "contentType" ""
                 "length"      -1
                 "sha2"        "1EC7A364BCE5081975A21AA62956908FC62EAA7D33EC942DDE349F6E4D1BBDB0"}]
               (get-in statement ["object" "attachments"]))))))

  ;; Context
  
  (testing "Template specifies Context"
    (testing "activity type properties"
      (let [statement
            (gen-statement
             {:contextCategoryActivityType
              ["https://w3id.org/xapi/cmi5/activitytype/course"]
              :contextGroupingActivityType
              ["https://w3id.org/xapi/cmi5/activities/course"]
              :contextParentActivityType
              ["https://w3id.org/xapi/cmi5/activitytype/block"]
              :contextOtherActivityType
              ["https://w3id.org/xapi/cmi5/activities/block"]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"category" [{"id"         "https://example.org/course/1671689032"
                             "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}
                            {"id" "https://w3id.org/xapi/cmi5/v1.0"}]
                "grouping" [{"id"         "https://example.org/course/1432714272"
                             "definition" {"type" "https://w3id.org/xapi/cmi5/activities/course"}}]
                "parent"   [{"id"         "https://example.org/block/1328449717"
                             "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/block"}}]
                "other"    [{"id"         "https://example.org/block/418707894"
                             "definition" {"type" "https://w3id.org/xapi/cmi5/activities/block"}}]}
               (get-in statement ["context" "contextActivities"])))))

    (testing "activity type properties + ID rules"
      (let [statement
            (gen-statement
             {:contextCategoryActivityType
              ["https://w3id.org/xapi/cmi5/activitytype/course"
               "https://w3id.org/xapi/cmi5/activitytype/block"]
              :rules [{:location "$.context.contextActivities.category[0].id"
                       :presence "included"
                       :all      ["https://example.org/course/zero"]}
                      {:location "$.context.contextActivities.category[1].id"
                       :presence "included"
                       :all      ["https://example.org/course/one"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"category" [{"id" "https://example.org/course/zero"
                             "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}
                            {"id" "https://example.org/course/one"
                             "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/block"}}
                            {"id" "https://w3id.org/xapi/cmi5/v1.0"}]}
               (get-in statement ["context" "contextActivities"])))))

    (testing "activity type properties + ID rules (overwrite profile ID)"
      (let [statement
            (gen-statement
             {:contextCategoryActivityType
              ["https://w3id.org/xapi/cmi5/activitytype/course"
               "https://w3id.org/xapi/cmi5/activitytype/block"]
              :rules [{:location "$.context.contextActivities.category[0].id"
                       :presence "included"
                       :all      ["https://example.org/course/zero"]}
                      {:location "$.context.contextActivities.category[1].id"
                       :presence "included"
                       :all      ["https://example.org/course/one"]}
                      {:location "$.context.contextActivities.category[2].id"
                       :presence "included"
                       :all      ["https://example.org/course/two"]}]})]
        (is (s/valid? ::xs/statement statement))
        ;; Profile context category activity gets restored during completion
        (is (statement-inputs? statement))
        (is (= {"category" [{"id" "https://example.org/course/zero"
                             "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}
                            {"id" "https://example.org/course/one"
                             "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/block"}}
                            {"id" "https://example.org/course/two"}
                            {"id" "https://w3id.org/xapi/cmi5/v1.0"}]}
               (get-in statement ["context" "contextActivities"])))))

    (testing "team and instructor"
      (let [statement
            (gen-statement {:rules [{:location "$.context.instructor"
                                     :presence "included"}
                                    {:location "$.context.team"
                                     :presence "included"}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        ;; Both instructor and team are completely spec-generated
        (is (= {"objectType" "Group"
                "mbox"       "mailto:lnbb@iyevnm.dplk"}
               (get-in statement ["context" "instructor"])))
        (is (= {"objectType" "Group"
                "openid"     "https://elwskid.ppur.kkz/qtpcyyrres"}
               (get-in statement ["context" "team"])))))

    (testing "agent instructor"
      (let [statement
            (gen-statement {:rules [{:location "$.context.instructor.objectType"
                                     :presence "included"
                                     :none      ["Group"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        ;; The instructor is completely spec-generated
        (is (= {"objectType"   "Agent"
                "mbox_sha1sum" "A2856240185B0DF7B04B7F6B6A2B12D970DF7513"}
               (get-in statement ["context" "instructor"]))))))

  ;; Extensions
  
  ;; TODO: Add Activity Extensions
  (testing "Template specifies Extensions on"
    (testing "Context"
      (let [ext-id "https://w3id.org/xapi/cmi5/context/extensions/sessionid"
            stmt   (gen-statement
                    {:rules [{:location (format "$.context.extensions['%s']" ext-id)
                              :presence "included"}]})]
        (is (s/valid? ::xs/statement stmt))
        (is (statement-inputs? stmt))
        ;; launchparameters also describes a string
        (is (string? (get-in stmt ["context" "extensions" ext-id]))))
      (let [ext-id "https://w3id.org/xapi/cmi5/context/extensions/masteryscore"
            stmt   (gen-statement
                    {:rules [{:location (format "$.context.extensions['%s']" ext-id)
                              :presence "included"}]})]
        (is (s/valid? ::xs/statement stmt))
        (is (statement-inputs? stmt))
        (is (<= 0 (get-in stmt ["context" "extensions" ext-id]) 1)))
      (let [ext-id "https://w3id.org/xapi/cmi5/context/extensions/launchmode"
            stmt   (gen-statement
                    {:rules [{:location (format "$.context.extensions['%s']" ext-id)
                              :presence "included"}]})]
        (is (s/valid? ::xs/statement stmt))
        (is (statement-inputs? stmt))
        ;; FIXME: This extension generates a completely wrong spec!
        #_(is (#{"Normal" "Browse" "Review"}
               (get-in stmt ["context" "extensions" ext-id]))))
      (let [ext-id "https://w3id.org/xapi/cmi5/context/extensions/launchurl"
            stmt   (gen-statement
                    {:rules [{:location (format "$.context.extensions['%s']" ext-id)
                              :presence "included"}]})]
        (is (s/valid? ::xs/statement stmt))
        (is (statement-inputs? stmt))
        ;; FIXME: This extension generates non-IRI strings!
        (is (string? (get-in stmt ["context" "extensions" ext-id])))
        #_(is (s/valid? ::xs/iri (get-in stmt ["context" "extensions" ext-id]))))
      (let [ext-id "https://w3id.org/xapi/cmi5/context/extensions/moveon"
            stmt   (gen-statement
                    {:rules [{:location (format "$.context.extensions['%s']" ext-id)
                              :presence "included"}]})]
        (is (s/valid? ::xs/statement stmt))
        (is (statement-inputs? stmt))
        ;; FIXME: This extension generates a completely wrong spec!
        #_(is (#{"Passed" "Completed" "CompletedAndPassed" "CompletedOrPassed" "NotApplicable"}
               (get-in stmt ["context" "extensions" ext-id])))))
    (testing "Result"
      (let [ext-id "https://w3id.org/xapi/cmi5/result/extensions/progress"
            stmt   (gen-statement
                    {:rules [{:location (format "$.result.extensions['%s']" ext-id)
                              :presence "included"}]})]
        (is (s/valid? ::xs/statement stmt))
        (is (statement-inputs? stmt))
        (is (<= 0 (get-in stmt ["result" "extensions" ext-id]) 100)))
      (let [ext-id "https://w3id.org/xapi/cmi5/result/extensions/reason"
            stmt   (gen-statement
                    {:rules [{:location (format "$.result.extensions['%s']" ext-id)
                              :presence "included"}]})]
        (is (s/valid? ::xs/statement stmt))
        (is (statement-inputs? stmt))
        (is (string? (get-in stmt ["result" "extensions" ext-id]))))))
  
  ;; Miscellaneous
  
  (testing "Template specifies attachmentUsageType properties"
    (let [statement
          (gen-statement
           {:attachmentUsageType ["http://example.org/attachment-type-1"
                                  "http://example.org/attachment-type-2"]})]
      (is (s/valid? ::xs/statement statement))
      (is (statement-inputs? statement))
      ;; Spec-generated attachment properties should be the same given
      ;; the same seed/rng
      (is (= [{"usageType"   "http://example.org/attachment-type-1"
               "display"     {"en" "SH5"}
               "contentType" ""
               "length"      0
               "sha2"        "9163CAA018D41EF373179A1B4DE448C877DBCFF8A26D0F841914A7FC21DAF6C1"}
              {"usageType"   "http://example.org/attachment-type-2"
               "display"     {"en"    "j"
                              "en-US" "1"}
               "contentType" ""
               "length"      0
               "sha2"        "AC3C4A4BE6A759BF56B874A60770BF3E4A9891B4B7DFC32529B0EF7451BCEE9A"}]
             (get statement "attachments")))))

  ;; TODO: WARNING if authority is set by non-LRS
  (testing "Template specifies authority rules"
    (let [statement
          (gen-statement
           {:rules [{:location "$.authority"
                     :presence "included"}]})]
      (is (s/valid? ::xs/statement statement))
      (is (statement-inputs? statement))
      ;; Spec-generated authority properties should be the same given
      ;; the same seed/rng
      (is (= {"objectType" "Group"
              "member"     [{"objectType" "Agent"
                             "account"    {"name"     "NpF26FAHxGJq8V89165U4OZ0G"
                                           "homePage" "cvdemv://vgttxdimtn.cszd.jyr/wkfapvby"}}
                            {"objectType"   "Agent"
                             "name"         "1v5vXOnr4fcG8B88I6"
                             "mbox_sha1sum" "9E8BC7156717957496372598626CDB0776B6BD2F"}]}
             (get statement "authority"))))
    
    (let [statement
          (gen-statement
           {:rules [{:location "$.authority.objectType"
                     :all      ["Agent"]}]})]
      (is (s/valid? ::xs/statement statement))
      (is (statement-inputs? statement))
      (is (= {"objectType"   "Agent"
              "mbox_sha1sum" "A2856240185B0DF7B04B7F6B6A2B12D970DF7513"}
             (get statement "authority")))))
  
  (testing "Template specifies authority rules - invalid"
    (let [statement
          (gen-statement
           {:rules [{:location "$.authority.member[0,1,2]"
                     :presence "included"}]})]
      (is (not (s/valid? ::xs/statement statement))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object Override Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- gen-statement-override
  [object-override partial-template]
  (let [arguments*
        (-> arguments
            (assoc :object-overrides {:objects [object-override]}))]
    (->> partial-template
         (merge {:id       "https://template-1"
                 :type     "StatementTemplate"
                 :inScheme "https://w3id.org/xapi/cmi5/v1.0"})
         (assoc arguments* :template)
         generate-statement)))

(deftest generate-statement-override-test
  (testing "Override with Activity"
    (testing "with no Template properties or rules"
      (let [override  {:objectType "Activity"
                       :id         "https://www.whatever.com/activities#course1"
                       :definition {:name        {:en-US "Course 1"}
                                    :description {:en-US "Course Description 1"}
                                    :type        "http://adlnet.gov/expapi/activities/course"}}
            statement (gen-statement-override override {})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= (w/stringify-keys override)
               (get statement "object")))))

    (testing "with Agent/Group object rules"
      (let [override  {:objectType "Activity"
                       :id         "https://www.whatever.com/activities#course1"
                       :definition {:name        {:en-US "Course 1"}
                                    :description {:en-US "Course Description 1"}
                                    :type        "http://adlnet.gov/expapi/activities/course"}}
            template  {:rules [{:location "$.object.objectType"
                                :all      ["Agent" "Group"]}
                               {:location "$.object.mbox"
                                :presence "included"}]}
            statement (gen-statement-override override template)]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= (w/stringify-keys override)
               (get statement "object"))))))

  (testing "Override with Agent"
    (testing "with no Template properties or rules"
      (let [override  {:objectType "Agent"
                       :name       "My Override"
                       :mbox       "mailto:myoverride@example.com"}
            statement (gen-statement-override override {})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= (w/stringify-keys override)
               (get statement "object")))))
    
    (testing "with objectActivityType property"
      (let [override  {:objectType "Agent"
                       :name       "My Override"
                       :mbox       "mailto:myoverride@example.com"}
            template  {:objectActivityType
                       "https://w3id.org/xapi/cmi5/activitytype/course"}
            statement (gen-statement-override override template)]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= (w/stringify-keys override)
               (get statement "object")))))
    
    (testing "with Activity object rules"
      (let [override  {:objectType "Agent"
                       :name       "My Override"
                       :mbox       "mailto:myoverride@example.com"}
            template  {:rules
                       [{:location "$.object.objectType"
                         :all      ["Activity"]}
                        {:location "$.object.id"
                         :all      ["https://example.org/course/1550503926"]}]}
            statement (gen-statement-override override template)]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= (w/stringify-keys override)
               (get statement "object"))))))

  (testing "Override with Activity or Agent"
    (testing "with equal probability"
      (let [override-1
            {:objectType "Activity"
             :id         "https://www.whatever.com/activities#course1"
             :definition {:name        {:en-US "Course 1"}
                          :description {:en-US "Course Description 1"}
                          :type        "http://adlnet.gov/expapi/activities/course"}}
            override-2
            {:objectType "Agent"
             :name       "My Override"
             :mbox       "mailto:myoverride@example.com"}
            overrides
            {:weights {override-1 0.5 override-2 0.5}
             :objects [override-1 override-2]}
            statement
            (generate-statement (assoc arguments
                                       :object-overrides overrides
                                       :template {:id       "https://template-1"
                                                  :type     "StatementTemplate"
                                                  :inScheme "https://w3id.org/xapi/cmi5/v1.0"}))]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (or (= (w/stringify-keys override-1)
                   (get statement "object"))
                (= (w/stringify-keys override-2)
                   (get statement "object"))))))
    (testing "with only Agent with nonzero probability"
      (let [override-1
            {:objectType "Activity"
             :id         "https://www.whatever.com/activities#course1"
             :definition {:name        {:en-US "Course 1"}
                          :description {:en-US "Course Description 1"}
                          :type        "http://adlnet.gov/expapi/activities/course"}}
            override-2
            {:objectType "Agent"
             :name       "My Override"
             :mbox       "mailto:myoverride@example.com"}
            overrides
            {:weights {override-1 0.0 override-2 1.0}
             :objects [override-1 override-2]}
            statement
            (generate-statement (assoc arguments
                                       :object-overrides overrides
                                       :template {:id       "https://template-1"
                                                  :type     "StatementTemplate"
                                                  :inScheme "https://w3id.org/xapi/cmi5/v1.0"}))]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= (w/stringify-keys override-2)
               (get statement "object")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repeat Generation Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest generate-statement-repeat-test
  (testing "Statement generation is valid regardless of seed"
    (let [the-rng (random/seed-rng top-seed)]
      (is (->> #(generate-statement
                 (assoc arguments :seed (random/rand-int the-rng 1000)))
               (repeatedly 30)
               (every? #(s/valid? ::xs/statement %))))))
  (testing "Statement generation is deterministic"
    (is (->> #(generate-statement arguments)
             (repeatedly 100)
             (apply distinct?)
             not))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Weighted Generation Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- gen-weighted-statements
  [weights]
  (let [arguments*
        (-> arguments
            (assoc :template {:id       "https://template-1"
                              :type     "StatementTemplate"
                              :inScheme "https://w3id.org/xapi/cmi5/v1.0"})
            (assoc-in [:alignments :weights] weights))
        init-rng
        (random/seed-rng 1000)]
    (->> (repeatedly 1000 #(random/rand-unbound-int init-rng))
         (map (partial assoc arguments* :seed))
         (map generate-statement))))

(deftest generate-weighted-statement-test
  (testing "Verb weights"
    (let [weights    {"https://w3id.org/xapi/adl/verbs/abandoned" 1.0
                      "https://w3id.org/xapi/adl/verbs/satisfied" 0.0
                      "https://w3id.org/xapi/adl/verbs/waived"    0.0}
          statements (gen-weighted-statements weights)
          verb-ids   (map #(get-in % ["verb" "id"]) statements)
          verb-freqs (frequencies verb-ids)]
      (is (= 1000 (get verb-freqs "https://w3id.org/xapi/adl/verbs/abandoned")))
      (is (nil? (get verb-freqs "https://w3id.org/xapi/adl/verbs/satisfied")))
      (is (nil? (get verb-freqs "https://w3id.org/xapi/adl/verbs/waived"))))
    (let [weights    {"https://w3id.org/xapi/adl/verbs/abandoned" 1.0
                      "https://w3id.org/xapi/adl/verbs/satisfied" 1.0
                      "https://w3id.org/xapi/adl/verbs/waived"    0.0}
          statements (gen-weighted-statements weights)
          verb-ids   (map #(get-in % ["verb" "id"]) statements)
          verb-freqs (frequencies verb-ids)]
      ;; The exact counts should be reasonably close to the mean of 500;
      ;; they are deterministic given the same sequence of seeds.
      (is (= 483 (get verb-freqs "https://w3id.org/xapi/adl/verbs/abandoned")))
      (is (= 517 (get verb-freqs "https://w3id.org/xapi/adl/verbs/satisfied")))
      (is (nil? (get verb-freqs "https://w3id.org/xapi/adl/verbs/waived"))))
    (let [weights    {"https://w3id.org/xapi/adl/verbs/abandoned" 1.0
                      "https://w3id.org/xapi/adl/verbs/satisfied" 1.0
                      "https://w3id.org/xapi/adl/verbs/waived"    1.0}
          statements (gen-weighted-statements weights)
          verb-ids   (map #(get-in % ["verb" "id"]) statements)
          verb-freqs (frequencies verb-ids)]
      ;; The exact counts should be reasonably close to the mean of 333;
      ;; they are deterministic given the same sequence of seeds.
      (is (= 337 (get verb-freqs "https://w3id.org/xapi/adl/verbs/abandoned")))
      (is (= 343 (get verb-freqs "https://w3id.org/xapi/adl/verbs/satisfied")))
      (is (= 320 (get verb-freqs "https://w3id.org/xapi/adl/verbs/waived"))))
    (let [weights    {"https://w3id.org/xapi/adl/verbs/abandoned" 1.0
                      "https://w3id.org/xapi/adl/verbs/satisfied" 0.4
                      "https://w3id.org/xapi/adl/verbs/waived"    0.0}
          statements (gen-weighted-statements weights)
          verb-ids   (map #(get-in % ["verb" "id"]) statements)
          verb-freqs (frequencies verb-ids)]
      ;; See `datasim.math.random-test` for details on how the expected means
      ;; (800 and 200, respectively) are computed.
      (is (= 793 (get verb-freqs "https://w3id.org/xapi/adl/verbs/abandoned")))
      (is (= 207 (get verb-freqs "https://w3id.org/xapi/adl/verbs/satisfied")))
      (is (nil? (get verb-freqs "https://w3id.org/xapi/adl/verbs/waived")))))

  (testing "Activity Type weights"
    (let [weights    {"https://w3id.org/xapi/cmi5/activities/block"    1.0
                      "https://w3id.org/xapi/cmi5/activities/course"   0.0
                      "https://w3id.org/xapi/cmi5/activitytype/block"  0.0
                      "https://w3id.org/xapi/cmi5/activitytype/course" 0.0}
          statements (gen-weighted-statements weights)
          act-types  (map #(get-in % ["object" "definition" "type"]) statements)
          act-freqs  (frequencies act-types)]
      (is (= 1000 (get act-freqs "https://w3id.org/xapi/cmi5/activities/block")))
      (is (nil? (get act-freqs "https://w3id.org/xapi/cmi5/activities/course")))
      (is (nil? (get act-freqs "https://w3id.org/xapi/cmi5/activitytype/block")))
      (is (nil? (get act-freqs "https://w3id.org/xapi/cmi5/activitytype/course"))))
    (let [weights    {"https://w3id.org/xapi/cmi5/activities/block"    1.0
                      "https://w3id.org/xapi/cmi5/activities/course"   1.0
                      "https://w3id.org/xapi/cmi5/activitytype/block"  0.0
                      "https://w3id.org/xapi/cmi5/activitytype/course" 0.0}
          statements (gen-weighted-statements weights)
          act-types  (map #(get-in % ["object" "definition" "type"]) statements)
          act-freqs  (frequencies act-types)]
      (is (= 520 (get act-freqs "https://w3id.org/xapi/cmi5/activities/block")))
      (is (= 480 (get act-freqs "https://w3id.org/xapi/cmi5/activities/course")))
      (is (nil? (get act-freqs "https://w3id.org/xapi/cmi5/activitytype/block")))
      (is (nil? (get act-freqs "https://w3id.org/xapi/cmi5/activitytype/course"))))
    (let [weights    {"https://w3id.org/xapi/cmi5/activities/block"    1.0
                      "https://w3id.org/xapi/cmi5/activities/course"   1.0
                      "https://w3id.org/xapi/cmi5/activitytype/block"  1.0
                      "https://w3id.org/xapi/cmi5/activitytype/course" 0.0}
          statements (gen-weighted-statements weights)
          act-types  (map #(get-in % ["object" "definition" "type"]) statements)
          act-freqs  (frequencies act-types)]
      (is (= 353 (get act-freqs "https://w3id.org/xapi/cmi5/activities/block")))
      (is (= 328 (get act-freqs "https://w3id.org/xapi/cmi5/activities/course")))
      (is (= 319 (get act-freqs "https://w3id.org/xapi/cmi5/activitytype/block")))
      (is (nil? (get act-freqs "https://w3id.org/xapi/cmi5/activitytype/course"))))
    (let [weights    {"https://w3id.org/xapi/cmi5/activities/block"    1.0
                      "https://w3id.org/xapi/cmi5/activities/course"   1.0
                      "https://w3id.org/xapi/cmi5/activitytype/block"  1.0
                      "https://w3id.org/xapi/cmi5/activitytype/course" 1.0}
          statements (gen-weighted-statements weights)
          act-types  (map #(get-in % ["object" "definition" "type"]) statements)
          act-freqs  (frequencies act-types)]
      (is (= 247 (get act-freqs "https://w3id.org/xapi/cmi5/activities/block")))
      (is (= 251 (get act-freqs "https://w3id.org/xapi/cmi5/activities/course")))
      (is (= 231 (get act-freqs "https://w3id.org/xapi/cmi5/activitytype/block")))
      (is (= 271 (get act-freqs "https://w3id.org/xapi/cmi5/activitytype/course"))))
    (let [weights    {"https://w3id.org/xapi/cmi5/activities/block"    1.0
                      "https://w3id.org/xapi/cmi5/activities/course"   0.4
                      "https://w3id.org/xapi/cmi5/activitytype/block"  0.0
                      "https://w3id.org/xapi/cmi5/activitytype/course" 0.0}
          statements (gen-weighted-statements weights)
          act-types  (map #(get-in % ["object" "definition" "type"]) statements)
          act-freqs  (frequencies act-types)]
      (is (= 803 (get act-freqs "https://w3id.org/xapi/cmi5/activities/block")))
      (is (= 197 (get act-freqs "https://w3id.org/xapi/cmi5/activities/course")))
      (is (nil? (get act-freqs "https://w3id.org/xapi/cmi5/activitytype/block")))
      (is (nil? (get act-freqs "https://w3id.org/xapi/cmi5/activitytype/course"))))))

(defn- gen-weighted-override-statements
  [weights]
  (let [arguments*
        (-> arguments
            (assoc :template {:id       "https://template-1"
                              :type     "StatementTemplate"
                              :inScheme "https://w3id.org/xapi/cmi5/v1.0"})
            (assoc :object-overrides {:objects (vec (keys weights))
                                      :weights weights}))
        init-rng
        (random/seed-rng 1000)]
    (->> (repeatedly 1000 #(random/rand-unbound-int init-rng))
         (map (partial assoc arguments* :seed))
         (map generate-statement))))

(deftest generated-weighted-statement-override-test
  (testing "Weighted Overrides"
    (let [object-1 {:objectType "Activity"
                    :id         "https://www.whatever.com/activities#course1"
                    :definition {:name        {:en-US "Course 1"}
                                 :description {:en-US "Course Description 1"}
                                 :type        "http://adlnet.gov/expapi/activities/course"}}
          object-2 {:objectType "Agent"
                    :name       "My Override"
                    :mbox       "mailto:myoverride@example.com"}]
      (let [weights      {object-1 1.0
                          object-2 0.0}
            statements   (gen-weighted-override-statements weights)
            objects      (map #(get % "object") statements)
            object-freqs (frequencies objects)]
        (is (= 1000 (get object-freqs (w/stringify-keys object-1))))
        (is (nil? (get object-freqs (w/stringify-keys object-2)))))
      (let [weights      {object-1 1.0
                          object-2 1.0}
            statements   (gen-weighted-override-statements weights)
            objects      (map #(get % "object") statements)
            object-freqs (frequencies objects)]
        (is (= 486 (get object-freqs (w/stringify-keys object-1))))
        (is (= 514 (get object-freqs (w/stringify-keys object-2)))))
      (let [weights      {object-1 1.0
                          object-2 0.4}
            statements   (gen-weighted-override-statements weights)
            objects      (map #(get % "object") statements)
            object-freqs (frequencies objects)]
        (is (= 797 (get object-freqs (w/stringify-keys object-1))))
        (is (= 203 (get object-freqs (w/stringify-keys object-2))))))))
