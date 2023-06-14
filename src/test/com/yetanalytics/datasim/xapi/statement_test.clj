(ns com.yetanalytics.datasim.xapi.statement-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [clojure.walk :as w]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.random :as random]
            [com.yetanalytics.datasim.xapi.statement :refer [generate-statement]]
            [com.yetanalytics.datasim.xapi.profile :as profile]
            [com.yetanalytics.datasim.xapi.activity :as activity]
            [com.yetanalytics.datasim.test-constants :as const]))

;; FIXME: generate-statement will still generate statements with blatantly contradictory rules,
;; e.g.
;; {:rules [{:location "$.id" :presence "included" :none ["3829c803-1f4c-44ed-8d8f-36e502cadd0f"]}
;;          {:location "$.id" :presence "included" :all ["3829c803-1f4c-44ed-8d8f-36e502cadd0f"]}}}]}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def top-seed 42)

(def profile-iri-map
  (-> const/simple-input :profiles profile/profiles->map))

(def activities
  (-> const/simple-input (activity/derive-cosmos 100)))

(def actor
  (-> const/simple-input :personae-array first :member first (dissoc :role)))

(def alignments*
  (-> const/simple-input (get-in [:alignments :alignment-vector 0 :alignments])))

(def object-override
  {:objectType "Activity"
   :id         "https://www.whatever.com/activities#course1"
   :definition {:name        {:en-US "Course 1"}
                :description {:en-US "Course Description 1"}
                :type        "http://adlnet.gov/expapi/activities/course"}})

(def alignments
  (reduce
   (fn [acc {:keys [component weight objectOverride]}]
     (assoc acc component {:weight          weight
                           :object-override objectOverride}))
   {}
   alignments*))

(def template
  (get profile-iri-map "https://w3id.org/xapi/cmi5#satisfied"))

(def pattern-ancestors
  [{:id      "https://w3id.org/xapi/cmi5#toplevel"
    :primary true}
   {:id      "https://w3id.org/xapi/cmi5#satisfieds"
    :primary false}])

(def registration
  "01234567-0000-4000-8000-123456789012")

(def valid-args
  {:input             const/simple-input
   :iri-map           profile-iri-map
   :activities        activities
   :actor             actor
   :alignment         alignments
   :sim-t             0
   :seed              top-seed #_(random/rand-long top-rng)
   :template          template
   :pattern-ancestors pattern-ancestors
   :registration      registration})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- gen-statement [partial-template]
  (->> partial-template
       (merge {:id       "https://template-1"
               :type     "StatementTemplate"
               :inScheme "https://w3id.org/xapi/cmi5/v1.0"})
       (assoc valid-args :template)
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
      ;; This UUID should be generated deterministically via rng
      (is (= "4f083ce3-f12b-4b4b-86ee-9d82b52c856d"
             (get statement "id")))))
  
  (testing "Template specifies ID"
    (let [statement (gen-statement {:rules [{:location "$.id"
                                             :presence "included"}]})]
      (is (s/valid? ::xs/statement statement))
      (is (statement-inputs? statement))
      (is (= "4f083ce3-f12b-4b4b-86ee-9d82b52c856d"
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

    (testing "display rule only - spec generation"
      (let [statement
            (gen-statement
             {:rules [{:location "$.verb.display"
                       :all      [{"en-us" "Custom Verb"
                                   "en-uk" "Verb that is Custom"}]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id" "vmpe://hiato.zvt.emzk/ahlqn" ; ID is randomly generated
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
        (is (= {"id"         "https://example.org/course/1550503926"
                "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}
               (get statement "object")))))

    (testing "activity type rule"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.definition.type"
                       :all      ["https://w3id.org/xapi/cmi5/activitytype/course"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id"         "https://example.org/course/1550503926"
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
        (is (= {"id"         "https://example.org/course/1550503926"
                "objectType" "Activity"
                "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}
               (get statement "object")))))

    (testing "ID rule"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.id"
                       :all      ["https://example.org/course/1550503926"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id"         "https://example.org/course/1550503926"
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
        (is (= {"id"         "poprhf://ddm.jbanfdcu.unx/hvfqllxhbpwpsypb" ; The ID is randomly generated
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
                       :all      ["https://example.org/course/1550503926"]}
                      {:location "$.object.objectType"
                       :any      ["Activity"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"id"         "https://example.org/course/1550503926"
                "objectType" "Activity"
                "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}
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
        (is (= {"name"         "Toby Nichols"
                "objectType"   "Agent"
                "mbox_sha1sum" "A2856240185B0DF7B04B7F6B6A2B12D970DF7513"}
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
                "openid"     "https://osnb.drokqem.obx/bgdmtnrvblgu"}
               (get statement-1 "object")))
        (is (= {"objectType" "Group"
                "name"       "i"
                "member"     [] ; FIXME: This is a generation flaw in xapi-schema...
                "openid"     "http://uqqqwki.qkhf.anqj/pkxndpkngky"}
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
        (is (= {"name"       "Toby Nichols"
                "objectType" "Agent"
                "openid"     "https://osnb.drokqem.obx/bgdmtnrvblgu"}
               (get statement "object")))))
    
    (testing "mbox rule"
      (let [statement (gen-statement {:rules [{:location "$.object.mbox"
                                               :presence "included"}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"objectType" "Agent"
                "mbox"       "mailto:lctlaqxlfg@lyubpwtqpm.txj"}
               (get statement "object")))))
    
    (testing "mbox_sha1sum rule"
      (let [statement (gen-statement {:rules [{:location "$.object.mbox_sha1sum"
                                               :presence "included"}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"objectType"   "Agent"
                "mbox_sha1sum" "7E86B6E11F478B874ED1F87C450EA644FA508662"}
               (get statement "object")))))
    
    (testing "openid rule"
      (let [statement (gen-statement {:rules [{:location "$.object.openid"
                                               :presence "included"}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"objectType" "Agent"
                "openid"     "https://lyubpwtqn.pqzkisi.lku/szcokfsgupq"}
               (get statement "object")))))
    
    (testing "account rule"
      (let [statement (gen-statement {:rules [{:location "$.object.account"
                                               :presence "included"}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= {"objectType" "Agent"
                "account"    {"name"     "40FcY0"
                              "homePage" "lzxisjwd://yptumdyz.zzm.uip/wnhbntxrmumwh"}}
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
                              "homePage" "mhqttko://mfyvhxsgxj.wpl.gnll/xlumwozhnvx"}}
               (get statement-1 "object")))
        (is (= {"objectType" "Group"
                "account"    {"name"     "Group Account Name" ; homePage is randomly generated
                              "homePage" "kpwkefqm://ttz.ebiclv.zyl/qfvgbycmul"}}
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
                "account"    {"name"     "0" ; name is randomly generated
                              "homePage" "http://example.org/agent"}}
               (get statement-1 "object")))
        (is (= {"objectType" "Group"
                "account"    {"name"     "9" ; name is randomly generated
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
        ;; FIXME: mbox and other IFI properties should count as distinct
        (is (= {"objectType" "Group"
                "member"
                [{"mbox" "mailto:three@example.com", "objectType" "Agent"}
                 {"mbox" "mailto:three@example.com", "objectType" "Agent"}
                 {"mbox" "mailto:three@example.com", "objectType" "Agent"}
                 {"mbox" "mailto:two@example.com", "objectType" "Agent"}
                 {"mbox" "mailto:three@example.com", "objectType" "Agent"}
                 {"mbox" "mailto:three@example.com", "objectType" "Agent"}
                 {"mbox" "mailto:three@example.com", "objectType" "Agent"}]}
               (get statement "object")))))

    (testing "member name rules"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.member.*.name"
                       :all      ["Number One" "Number Two" "Number Three"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        ;; "name" is not a distinct property so the same name can repeat
        ;; The IFIs are randomly generated and each name is randomly chosen,
        ;; as is the number of names
        (is (= {"objectType" "Group"
                "member"
                [{"name"         "Number Two"
                  "objectType"   "Agent"
                  "mbox_sha1sum" "E3940FBAF32D6B8976ACDDF3AE538905EF4FD352"}
                 {"name"       "Number Two"
                  "objectType" "Agent"
                  "openid"     "http://frqbpv.fyv.hnh/meayik"}
                 {"name"       "Number Two"
                  "objectType" "Agent"
                  "account"    {"name"     "0"
                                "homePage" "krlzvdpj://qyjpb.ozt.fjsc/efuetppixjm"}}
                 {"name"       "Number One"
                  "objectType" "Agent"
                  "account"    {"name"     "8V"
                                "homePage" "upwzseiz://hxde.iwczjmywox.uku/mwtevtjhg"}}
                 {"name"       "Number Two"
                  "objectType" "Agent"
                  "account"    {"name"     "x"
                                "homePage" "zmimtp://uno.kum.fgy/dsylyuesap"}}
                 {"name"         "Number Two"
                  "objectType"   "Agent"
                  "mbox_sha1sum" "949BD396DCA71F8BB3A430654E29AD582960F866"}
                 {"name"       "Number Two"
                  "objectType" "Agent"
                  "mbox"       "mailto:wbkivamvqjp@vmuonfkuxufx.qkr"}]}
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
                "id"         "b22038ce-a747-4e0f-a4d4-7b34d816253e"
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
        (is (= {"id"         "https://example.org/course/418707894"
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
                "verb"       {"id" "https://w3id.org/xapi/adl/verbs/waived"
                              "display" {"en" "waived"}}
                "object"     {"id" "https://example.org/course/1550503926"
                              "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}}
               (get statement "object")))))

    (testing "objectType + Activity object"
      (let [statement
            (gen-statement
             {:rules [{:location "$.object.objectType"
                       :presence "included"
                       :any      ["SubStatement"]}
                      {:location "$.object.object.id"
                       :presence "included"
                       :any      ["https://example.org/block/1432714272"]}
                      {:location "$.object.object.objectType"
                       :presence "included"
                       :any      ["Activity"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= "SubStatement"
               (get-in statement ["object" "objectType"])))
        (is (= {"id"         "https://example.org/block/1432714272"
                "objectType" "Activity"
                "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/block"}}
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
        (is (= {"name"       "Toby Nichols"
                "objectType" "Agent"
                "account"    {"name"     "Z"
                              "homePage" "hfzs://lulqixugpx.ckpsmcqvrd.aff/ausyu"}}
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
        (is (= {"objectType"   "Agent"
                "mbox_sha1sum" "A2856240185B0DF7B04B7F6B6A2B12D970DF7513"
                "name"         "AN0rXE9qvu36cCDOPUQPcCna0"}
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
                 "description" {"en-GB" "L53l" "en-US" "o"}
                 "display"     {"en-US" "N"}
                 "contentType" "1"
                 "length"      0
                 "sha2"        "39E3969BCDED0A6362E4EDBFC2DABD7A6D3F78E74971B98EC466C7988541FB0C"}]
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
        (is (= {"category" [{"id" "https://example.org/course/1550503926"
                             "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}
                            {"id" "https://w3id.org/xapi/cmi5/v1.0"}]
                "grouping" [{"id" "https://example.org/course/418707894"
                             "definition" {"type" "https://w3id.org/xapi/cmi5/activities/course"}}]
                "parent"   [{"id" "https://example.org/block/1432714272"
                             "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/block"}}]
                "other"    [{"id" "https://example.org/block/1671689032"
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
        ;; FIXME: profile context category activity should not be overwritten
        (is (not (statement-inputs? statement)))
        (is (= {"category" [{"id" "https://example.org/course/zero"
                             "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}
                            {"id" "https://example.org/course/one"
                             "definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/block"}}
                            {"id" "https://example.org/course/two"}]}
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
                "name"       "7G"
                "openid"     "http://yfzygv.ppktocltom.rvx/tmweii"
                "member"
                [{"objectType" "Agent"
                  "openid"     "https://szkenjsa.pplhlfxtml.gvcv/hqjajyomdhkm"}
                 {"objectType" "Agent"
                  "name"       "ch0LZ9Tb"
                  "mbox"       "mailto:xmx@tcln.yqsr"}]}
               (get-in statement ["context" "instructor"])))
        (is (= {"objectType"   "Group"
                "mbox_sha1sum" "F93A8918A5B7A2163C2C30767A2196B674E7E563"
                "member"
                [{"name"         "4wyym75I1KyfmAloAss8En"
                  "mbox_sha1sum" "13A48E290619240465477D6EA1C12D1049547D32"
                  "objectType"   "Agent"}
                 {"objectType"   "Agent"
                  "name"         ""
                  "mbox_sha1sum" "F35DC021BDE81815E35C3738BBE30BC2EBFD4D99"}
                 {"name"       "jFEO6RV0nvN2XKT0aRgm"
                  "account"    {"name"     "eMbZXJRWzzDr03sEa7F5kFDf9qZpl2x"
                                "homePage" "xqddl://yqvg.cwnvurfjm.egv/foaahwnmnssgwbz"}
                  "objectType" "Agent"}]}
               (get-in statement ["context" "team"])))))

    (testing "agent instructor"
      (let [statement
            (gen-statement {:rules [{:location "$.context.instructor.objectType"
                                     :presence "included"
                                     :none      ["Group"]}]})]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        ;; The instructor is completely spec-generated
        (is (= {"objectType" "Agent"
                "account"    {"name"     "Z"
                              "homePage" "hfzs://lulqixugpx.ckpsmcqvrd.aff/ausyu"}}
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
      (is (= [{"usageType" "http://example.org/attachment-type-1"
               "display" {"en-GB" "Ik"}
               "contentType" ""
               "length" 0
               "sha2" "EBD14C60DCDCC8D3FA248367F8601D28F2838BBAB6160F79A346261F86B0DEEC"}
              {"usageType" "http://example.org/attachment-type-2"
               "description" {"en-US" "o2Sf", "fr" "L"}
               "display" {"en-US" "p6" "en-GB" "T" "en" "7"}
               "contentType" ""
               "length" 0
               "sha2" "7E77EC5BC52256B5F8804ED073E39B3371518AF9E62EA42007E8222A9D1A96C4"}]
             (get statement "attachments")))))

  ;; TODO: Should authorities even be completed since they're supposed to only
  ;; be set by the accepting LRS
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
              "member"     [{"name"       "P5ia8r1379ZDUMyJ0ZkR7HPj6060vb"
                             "objectType" "Agent"
                             "account"    {"name"     "YQ26WGI7MILRKxuZ5oIiv1n0laGY"
                                           "homePage" "bal://wjimefp.aiuohxb.gxiz/wtd"}}
                            {"account"    {"name"     "i1VMk4"
                                           "homePage" "hizkf://lcdssef.ljjdknrxt.gmkz/yoha"}
                             "objectType" "Agent"}]}
             (get statement "authority"))))))

(defn- gen-statement-override [object-override partial-template]
  (let [valid-args*
        (-> valid-args
            (assoc-in [:alignment "https://example.org/activity/a" :object-override]
                      object-override)
            (update-in [:alignment] dissoc "https://example.org/activity/c"))]
    (->> partial-template
         (merge {:id       "https://template-1"
                 :type     "StatementTemplate"
                 :inScheme "https://w3id.org/xapi/cmi5/v1.0"})
         (assoc valid-args* :template)
         generate-statement)))

(deftest generate-statemnet-override-test
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
            alignments
            {"https://example.org/activity/a"
             {:weight 0.5, :object-override override-1}
             "https://example.org/activity/c"
             {:weight 0.5, :object-override override-2}}
            statement
            (generate-statement (assoc valid-args
                                       :alignment alignments
                                       :template  {:id       "https://template-1"
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
            alignments
            {"https://example.org/activity/a"
             {:weight -1.0, :object-override override-1}
             "https://example.org/activity/c"
             {:weight 1.0, :object-override override-2}}
            statement
            (generate-statement (assoc valid-args
                                       :alignment alignments
                                       :template  {:id       "https://template-1"
                                                   :type     "StatementTemplate"
                                                   :inScheme "https://w3id.org/xapi/cmi5/v1.0"}))]
        (is (s/valid? ::xs/statement statement))
        (is (statement-inputs? statement))
        (is (= (w/stringify-keys override-2)
               (get statement "object")))))))

(deftest generate-statement-repeat-test
  (testing "Statement generation is valid regardless of seed"
    (let [the-rng (random/seed-rng top-seed)]
      (is (->> #(generate-statement
                 (assoc valid-args :seed (random/rand-int* the-rng 1000)))
               (repeatedly 30)
               (every? #(s/valid? ::xs/statement %))))))
  (testing "Statement generation is deterministic"
    (is (->> #(generate-statement valid-args)
             (repeatedly 100)
             (apply distinct?)
             not))))
