(ns com.yetanalytics.datasim.input-test
  (:require [clojure.test :refer [deftest testing is are]]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.datasim.input
             :refer [from-location validate validate-throw]
             :as input]))

;; simple property test to cover the input types

(deftest input-objects-test
  (are [test-name      ; friendly name
        k              ; key
        loc            ; location of good json input
        invalidator-fn ; fn to make test invalid
        ]
       (testing (format "testing %s:" test-name)
         (let [x (from-location k :json loc)]
           (testing "protocols"
             (is (satisfies? p/FromInput x))
             (is (satisfies? p/JSONRepresentable x)))
           (testing "validation"
             (is (nil? (validate x)))
             (is (some? (validate (invalidator-fn x))))
             (is (thrown? clojure.lang.ExceptionInfo
                          (validate-throw (invalidator-fn x)))))))
    "xAPI Profile"
    :profile
    "dev-resources/profiles/cmi5/fixed.json"
    #(assoc % :id "foo") ; profiles need an IRI ID

    "Actor Personae"
    :personae
    "dev-resources/personae/simple.json"
    #(assoc % :member []) ; groups need members

    "Actor Alignments"
    :alignments
    "dev-resources/alignments/simple.json"
    ;; alignments are a vector of maps containing a vector of maps
    #(assoc % :alignment-vector [{:id "notanid" :alignments [{:component "notaniri" :weight "bar"}]}])

    ;; Fails if JSON parsing is lazy
    "Actor Alignments, Long"
    :alignments
    "dev-resources/alignments/tccc_dev.json"
    #(assoc % :alignment-vector [{:id "notanid" :alignments [{:component "notaniri" :weight "bar"}]}])

    "Actor Alignments w/ Overrides"
    :alignments
    "dev-resources/alignments/simple_with_overrides.json"
    #(assoc % :alignment-vector [{:id "notanid" :alignments [{:component "notaniri" :weight "bar"}]}])

    "Simulation Parameters"
    :parameters
    "dev-resources/parameters/simple.json"
    #(assoc % :seed "hey") ; seed is a number

    "Combined Input Spec"
    :input
    "dev-resources/input/simple.json"
    #(update % :profiles first) ; profiles are a vector
    ))

(deftest profile-cosmos-validation-test
  (testing "input is valid if all template refs are valid"
    (is (nil? (input/validate-profiles
               [(from-location :profile :json "dev-resources/profiles/cmi5/fixed.json")]))))
  (testing "input is invalid if invalid template ref iri exists"
    (is (= 1 (->>
              [(-> (from-location :profile :json "dev-resources/profiles/cmi5/fixed.json")
                   (assoc-in [:patterns 0 :zeroOrMore]
                             "https://w3id.org/xapi/cmi5#bad-template"))]
              input/validate-profiles
              count)))
    ;; XXX: If we replaced satisfiedbad3 with satisfiedbad2, we only get a count
    ;; of 2 errors, not 3.
    (is (= 3 (->>
              [(-> (from-location :profile :json "dev-resources/profiles/cmi5/fixed.json")
                   (assoc-in [:patterns 0 :zeroOrMore]
                             "https://w3id.org/xapi/cmi5#satisfiedbad1")
                   (assoc-in [:patterns 1 :sequence 0]
                             "https://w3id.org/xapi/cmi5#satisfiedbad2")
                   (assoc-in [:patterns 1 :sequence 2]
                             "https://w3id.org/xapi/cmi5#satisfiedbad3"))]
              input/validate-profiles
              count))))
  (testing "validation works for multi-profile cosmos"
    (is (nil? (input/validate-profiles
               [(from-location :profile :json "dev-resources/profiles/cmi5/fixed.json")
                (from-location :profile :json "dev-resources/profiles/video/profile.jsonld")])))
    ;; Add connections between Profiles
    (is (nil? (input/validate-profiles
               [(from-location :profile :json "dev-resources/profiles/cmi5/fixed.json")
                (-> (from-location :profile :json "dev-resources/profiles/video/profile.jsonld")
                    (assoc-in [:patterns 0 :sequence 0]
                              "https://w3id.org/xapi/cmi5#initialized")
                    (assoc-in [:patterns 0 :sequence 2]
                              "https://w3id.org/xapi/cmi5#terminated")
                    (assoc-in [:patterns 1 :alternates 6]
                              "https://w3id.org/xapi/cmi5#completed"))])))
    (is (= 1 (->> [(-> (from-location :profile :json "dev-resources/profiles/cmi5/fixed.json")
                       (assoc-in [:patterns 0 :zeroOrMore]
                                 "https://w3id.org/xapi/cmi5#bad-template"))
                   (from-location :profile :json "dev-resources/profiles/video/profile.jsonld")]
                  input/validate-profiles
                  count))))
  ;; Following tests exist to point out flaws in Profiles
  (testing "invalid profiles"
    ;; AcrossX and ActivityStreams violate spec:
    ;; "related MUST only be used on deprecated Concepts"
    (is (some? (input/validate-profiles
                [(from-location :profile :json "dev-resources/profiles/acrossx/profile.jsonld")])))
    (is (nil? (input/validate-profiles
               [(from-location :profile :json "dev-resources/profiles/acrossx/fixed.jsonld")])))
    (is (some? (input/validate-profiles
                [(from-location :profile :json "dev-resources/profiles/activity_streams/profile.jsonld")])))
    (is (nil? (input/validate-profiles
               [(from-location :profile :json "dev-resources/profiles/activity_streams/fixed.jsonld")])))
    ;; TC3 Profile violates spec:
    ;; "alternates Pattern MUST NOT contain zeroOrMore"
    (is (some? (input/validate-profiles
                [(from-location :profile :json "dev-resources/profiles/tccc/cuf_hc_video_and_asm_student_survey_profile.jsonld")])))
    (is (nil? (input/validate-profiles
               [(from-location :profile :json "dev-resources/profiles/tccc/fixed.jsonld")])))))

(deftest personae-array-validation-test
  (testing "personae-array spec"
    (is (nil? (input/validate-personae-array
               [(from-location :personae :json "dev-resources/personae/simple.json")
                (from-location :personae :json "dev-resources/personae/tccc_dev.json")])))
    (is (some? (input/validate-personae-array
                [(-> (from-location :personae :json "dev-resources/personae/simple.json")
                     (assoc-in [:member 0 :mbox] "not-an-email"))
                 (from-location :personae :json "dev-resources/personae/tccc_dev.json")])))
    (is (some? (input/validate-personae-array []))))
  (testing "duplicate member ids across different groups"
    (is (some? (input/validate-personae-array
                [(-> (from-location :personae :json "dev-resources/personae/simple.json")
                     (assoc-in [:member 0 :mbox] "mailto:bob@example.org")
                     (assoc-in [:member 1 :mbox] "mailto:alice@example.org")
                     (assoc-in [:member 2 :mbox] "mailto:fred@example.org"))
                 (from-location :personae :json "dev-resources/personae/tccc_dev.json")])))))

(deftest subobject-validation-test
  (testing "input is valid with a minimal profile"
    (is (nil? (p/validate
               (assoc-in (from-location :input :json "dev-resources/input/simple.json")
                         [:profiles 0]
                         (from-location :profile :json "dev-resources/profiles/minimal.jsonld")))))))

(deftest input-validation-test
  (testing "input is valid"
    (is (nil? (p/validate
               (from-location :input :json "dev-resources/input/simple.json"))))
    (is (satisfies? p/FromInput
                    (from-location :input :json "dev-resources/input/simple.json")))
    (is (try (validate-throw
              (from-location :input :json "dev-resources/input/simple.json"))
             true
             (catch Exception _ false)))))
