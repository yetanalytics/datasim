(ns com.yetanalytics.datasim.input-test
  (:require [clojure.test :refer [deftest testing is are]]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.datasim.input
             :refer [from-location validate validate-throw]
             :as input]))

;; Simple property test to cover the input types

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filepath Fixtures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Profiles
(def minimal-profile-filepath
  "dev-resources/profiles/minimal.jsonld")
(def cmi5-profile-filepath
  "dev-resources/profiles/cmi5/fixed.json")
(def video-profile-filepath
  "dev-resources/profiles/video/profile.jsonld")
(def acrossx-profile-filepath*
  "dev-resources/profiles/acrossx/profile.jsonld")
(def acrossx-profile-filepath
  "dev-resources/profiles/acrossx/fixed.jsonld")
(def activity-profile-filepath*
  "dev-resources/profiles/activity_streams/profile.jsonld")
(def activity-profile-filepath
  "dev-resources/profiles/activity_streams/fixed.jsonld")
(def tc3-profile-filepath
  "dev-resources/profiles/tccc/cuf_hc_video_and_asm_student_survey_profile.jsonld")

;; Personae
(def simple-personae-filepath
  "dev-resources/personae/simple.json")
(def tc3-personae-filepath
  "dev-resources/personae/tccc_dev.json")

;; Alignments
(def simple-alignments-filepath
  "dev-resources/alignments/simple.json")
(def simple-overrides-alginments-filepath
  "dev-resources/alignments/simple_with_overrides.json")
(def tc3-alignments-filepath
  "dev-resources/alignments/tccc_dev.json")

;; Parameters
(def simple-parameters-filepath
  "dev-resources/parameters/simple.json")

;; Combined Input
(def simple-input-filepath
  "dev-resources/input/simple.json")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest input-invalidation-test
  (are [test-name
        input-key
        file-loc
        invalidator-fn]
       (testing (format "testing %s: -" test-name)
         (let [x (from-location input-key :json file-loc)]
           (testing "protocols"
             (is (satisfies? p/FromInput x))
             (is (satisfies? p/JSONRepresentable x)))
           (testing "validation"
             (is (nil? (validate x)))
             (is (some? (validate (invalidator-fn x))))
             (is (thrown? clojure.lang.ExceptionInfo
                          (validate-throw (invalidator-fn x)))))))
    "xAPI Profile - invalid due to non-IRI ID"
    :profile cmi5-profile-filepath
    #(assoc % :id "foo")
    "Actor Personae - invalid due to empty `:members` coll"
    :personae simple-personae-filepath
    #(assoc % :member [])
    "Actor Alignments - invalid due to invalid alignments"
    :alignments simple-alignments-filepath
    #(assoc % :alignment-vector [{:id         "notanid"
                                  :alignments [{:component "notaniri"
                                                :weight    "bar"}]}])
    "Actor Alignments, Long - invalid due to invalid alignments"
    :alignments
    tc3-alignments-filepath
    #(assoc % :alignment-vector [{:id         "notanid"
                                  :alignments [{:component "notaniri"
                                                :weight    "bar"}]}])
    "Actor Alignments w/ Overrides - invalid due to invalid alignments"
    :alignments
    simple-overrides-alginments-filepath
    #(assoc % :alignment-vector [{:id         "notanid"
                                  :alignments [{:component "notaniri"
                                                :weight    "bar"}]}])
    "Simulation Parameters - invalid due to non-numeric seed"
    :parameters simple-parameters-filepath
    #(assoc % :seed "hey")
    "Combined Input Spec - invalid due to `:profiles` not being a vector"
    :input simple-input-filepath
    #(update % :profiles first)))

(deftest profile-cosmos-validation-test
  (testing "input is valid if all template refs are valid"
    (is (nil? (input/validate-profiles
               [(from-location :profile :json cmi5-profile-filepath)]))))
  (testing "input is invalid if invalid template ref iri exists"
    (is (= 1 (->> [(-> (from-location :profile :json cmi5-profile-filepath)
                       (assoc-in [:patterns 0 :zeroOrMore]
                                 "https://w3id.org/xapi/cmi5#bad-template"))]
                  input/validate-profiles
                  count)))
    ;; XXX: If we replaced satisfiedbad3 with satisfiedbad2, we only get a count
    ;; of 2 errors, not 3.
    (is (= 3 (->> [(-> (from-location :profile :json cmi5-profile-filepath)
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
               [(from-location :profile :json cmi5-profile-filepath)
                (from-location :profile :json video-profile-filepath)])))
    ;; Add connections between Profiles
    (is (nil? (input/validate-profiles
               [(from-location :profile :json cmi5-profile-filepath)
                (-> (from-location :profile :json video-profile-filepath)
                    (assoc-in [:patterns 0 :sequence 0]
                              "https://w3id.org/xapi/cmi5#initialized")
                    (assoc-in [:patterns 0 :sequence 2]
                              "https://w3id.org/xapi/cmi5#terminated")
                    (assoc-in [:patterns 1 :alternates 6]
                              "https://w3id.org/xapi/cmi5#completed"))])))
    (is (= 1 (->> [(-> (from-location :profile :json cmi5-profile-filepath)
                       (assoc-in [:patterns 0 :zeroOrMore]
                                 "https://w3id.org/xapi/cmi5#bad-template"))
                   (from-location :profile :json video-profile-filepath)]
                  input/validate-profiles
                  count))))
  (testing "fixed / valid profiles"
    (is (nil? (input/validate-profiles
               [(from-location :profile :json acrossx-profile-filepath)])))
    (is (nil? (input/validate-profiles
               [(from-location :profile :json activity-profile-filepath)])))
    (is (nil? (input/validate-profiles
               [(from-location :profile :json tc3-profile-filepath)]))))
  ;; Following tests exist to point out flaws in Profiles
  (testing "invalid profiles"
    ;; AcrossX and ActivityStreams violate spec:
    ;; "related MUST only be used on deprecated Concepts"
    (is (some? (input/validate-profiles
                [(from-location :profile :json acrossx-profile-filepath*)])))
    (is (some? (input/validate-profiles
                [(from-location :profile :json activity-profile-filepath*)])))))

(deftest personae-array-validation-test
  (testing "personae-array spec"
    (is (nil? (input/validate-personae-array
               [(from-location :personae :json simple-personae-filepath)
                (from-location :personae :json tc3-personae-filepath)])))
    (is (some? (input/validate-personae-array
                [(-> (from-location :personae :json simple-personae-filepath)
                     (assoc-in [:member 0 :mbox] "not-an-email"))
                 (from-location :personae :json tc3-profile-filepath)])))
    (is (some? (input/validate-personae-array []))))
  (testing "duplicate member ids across different groups"
    (is (some? (input/validate-personae-array
                [(-> (from-location :personae :json simple-personae-filepath)
                     (assoc-in [:member 0 :mbox] "mailto:bob@example.org")
                     (assoc-in [:member 1 :mbox] "mailto:alice@example.org")
                     (assoc-in [:member 2 :mbox] "mailto:fred@example.org"))
                 (from-location :personae :json tc3-personae-filepath)])))))

(deftest subobject-validation-test
  (testing "input is valid with a minimal profile"
    (is (nil? (p/validate
               (assoc-in
                (from-location :input :json simple-input-filepath)
                [:profiles 0]
                (from-location :profile :json minimal-profile-filepath)))))))

(deftest input-validation-test
  (testing "input is valid"
    (is (nil? (p/validate
               (from-location :input :json simple-input-filepath))))
    (is (satisfies? p/FromInput
                    (from-location :input :json simple-input-filepath)))
    (is (try (validate-throw
              (from-location :input :json simple-input-filepath))
             true
             (catch Exception _ false))))
  (testing "input is invalid"
    (testing "with invalid gen-profiles"
      (is (try
            (validate-throw
             (assoc-in (from-location :input :json simple-input-filepath)
                       [:parameters :gen-profiles]
                       ["http://example.com/nonexistent.jsonld"]))
            false
            (catch Exception _ true))))
    (testing "with invalid gen-patterns"
      (is (try
            (validate-throw
             (assoc-in (from-location :input :json simple-input-filepath)
                       [:parameters :gen-patterns]
                       ["http://example.com/nonexistent#pattern"]))
            false
            (catch Exception _ true))))))
