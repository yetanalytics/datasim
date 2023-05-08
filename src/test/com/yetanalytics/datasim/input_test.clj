(ns com.yetanalytics.datasim.input-test
  (:require [clojure.test :refer [deftest testing is are]]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.datasim.input
             :refer [from-location validate validate-throw]
             :as input]
            [com.yetanalytics.datasim.test-fixtures :as fix])
  (:import [clojure.lang ExceptionInfo]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input Read Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We need to test that the process of reading itself is valid, rather than
;; entirely rely on already-read fixtures

(deftest valid-invalid-read-test
  (are [test-name
        invalid-reason
        input-key
        file-loc
        invalidator-fn]
       (testing (format "Reading %s" test-name)
         (let [x (from-location input-key :json file-loc)]
           (testing "satisfies protocols"
             (is (satisfies? p/FromInput x))
             (is (satisfies? p/JSONRepresentable x)))
           (testing "is valid"
             (is (nil? (validate x))))
           (testing (format "is invalid due to %s" invalid-reason)
             (is (some? (validate (invalidator-fn x))))
             (is (thrown? ExceptionInfo
                          (validate-throw (invalidator-fn x)))))))
    "xAPI Profile" "non-IRI ID" 
    :profile fix/cmi5-profile-filepath
    #(assoc % :id "foo")
    "Actor Personae" "empty `:members` coll"
    :personae fix/simple-personae-filepath
    #(assoc % :member [])
    "Actor Alignments" "invalid due to invalid alignments"
    :alignments fix/simple-alignments-filepath
    #(assoc % :alignment-vector [{:id         "notanid"
                                  :alignments [{:component "notaniri"
                                                :weight    "bar"}]}])
    "Actor Alignments, Long" "invalid alignments"
    :alignments fix/tc3-alignments-filepath
    #(assoc % :alignment-vector [{:id         "notanid"
                                  :alignments [{:component "notaniri"
                                                :weight    "bar"}]}])
    "Actor Alignments w/ Overrides" "invalid alignments"
    :alignments fix/overrides-alignments-filepath
    #(assoc % :alignment-vector [{:id         "notanid"
                                  :alignments [{:component "notaniri"
                                                :weight    "bar"}]}])
    "Simulation Parameters" "non-numeric seed"
    :parameters fix/simple-parameters-filepath
    #(assoc % :seed "hey")
    "Combined Input Spec" "`:profiles` not being a vector"
    :input fix/simple-input-filepath
    #(update % :profiles first)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input Validation Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def cmi5-satisfied-bad-1 "https://w3id.org/xapi/cmi5#satisfiedbad1")
(def cmi5-satisfied-bad-2 "https://w3id.org/xapi/cmi5#satisfiedbad2")
(def cmi5-satisfied-bad-3 "https://w3id.org/xapi/cmi5#satisfiedbad3")
(def cmi5-initialized "https://w3id.org/xapi/cmi5#initialized")
(def cmi5-terminated "https://w3id.org/xapi/cmi5#terminated")
(def cmi5-completed "https://w3id.org/xapi/cmi5#completed")

(deftest profile-cosmos-validation-test
  (testing "input is valid if all template refs are valid"
    (is (nil? (input/validate-profiles [fix/cmi5-profile]))))
  (testing "input is invalid if invalid template ref iri exists"
    (is (= 1 (->> [(-> fix/cmi5-profile
                       (assoc-in [:patterns 0 :zeroOrMore]
                                 "https://w3id.org/xapi/cmi5#bad-template"))]
                  input/validate-profiles
                  count)))
    ;; XXX: If we replaced satisfiedbad3 with satisfiedbad2, we only get a count
    ;; of 2 errors, not 3.
    (is (= 3
           (->> [(-> fix/cmi5-profile
                     (assoc-in [:patterns 0 :zeroOrMore] cmi5-satisfied-bad-1)
                     (assoc-in [:patterns 1 :sequence 0] cmi5-satisfied-bad-2)
                     (assoc-in [:patterns 1 :sequence 2] cmi5-satisfied-bad-3))]
                input/validate-profiles
                count))))
  (testing "validation works for multi-profile cosmos"
    (is (nil? (input/validate-profiles
               [fix/cmi5-profile fix/video-profile])))
    ;; Add connections between Profiles
    (is (nil? (input/validate-profiles
               [fix/cmi5-profile
                (-> fix/video-profile
                    (assoc-in [:patterns 0 :sequence 0] cmi5-initialized)
                    (assoc-in [:patterns 0 :sequence 2] cmi5-terminated)
                    (assoc-in [:patterns 1 :alternates 6] cmi5-completed))])))
    (is (= 1 (->> [(-> fix/cmi5-profile
                       (assoc-in [:patterns 0 :zeroOrMore]
                                 "https://w3id.org/xapi/cmi5#bad-template"))
                   fix/video-profile]
                  input/validate-profiles
                  count))))
  (testing "fixed / valid profiles"
    (is (nil? (input/validate-profiles [fix/acrossx-profile])))
    (is (nil? (input/validate-profiles [fix/activity-profile])))
    (is (nil? (input/validate-profiles [fix/tc3-profile]))))
  ;; Following tests exist to point out flaws in Profiles
  (testing "invalid profiles"
    ;; AcrossX and ActivityStreams violate spec:
    ;; "related MUST only be used on deprecated Concepts"
    (is (some? (input/validate-profiles [fix/acrossx-profile*])))
    (is (some? (input/validate-profiles [fix/activity-profile*])))))

(deftest personae-array-validation-test
  (testing "personae-array spec"
    (is (nil? (input/validate-personae-array
               [fix/simple-personae fix/tc3-personae])))
    (is (some? (input/validate-personae-array
                [(-> fix/simple-personae
                     (assoc-in [:member 0 :mbox] "not-an-email"))
                 fix/tc3-personae])))
    (is (some? (input/validate-personae-array []))))
  (testing "duplicate member ids across different groups"
    (is (some? (input/validate-personae-array
                [(-> fix/simple-personae
                     (assoc-in [:member 0 :mbox] "mailto:bob@example.org")
                     (assoc-in [:member 1 :mbox] "mailto:alice@example.org")
                     (assoc-in [:member 2 :mbox] "mailto:fred@example.org"))
                 fix/tc3-personae])))))

(deftest subobject-validation-test
  (testing "input is valid with a minimal profile"
    (is (nil? (p/validate
               (assoc-in fix/simple-input
                         [:profiles 0]
                         fix/minimal-profile))))))

(deftest combined-input-validation-test
  (testing "combined input is valid"
    (is (nil? (p/validate fix/simple-input)))
    (is (satisfies? p/FromInput fix/simple-input))
    (is (try (validate-throw fix/simple-input)
             true
             (catch Exception _ false))))
  (testing "combined input is invalid"
    (testing "with invalid gen-profiles"
      (is (try
            (validate-throw
             (assoc-in fix/simple-input
                       [:parameters :gen-profiles]
                       ["http://example.com/nonexistent.jsonld"]))
            false
            (catch Exception _ true))))
    (testing "with invalid gen-patterns"
      (is (try
            (validate-throw
             (assoc-in fix/simple-input
                       [:parameters :gen-patterns]
                       ["http://example.com/nonexistent#pattern"]))
            false
            (catch Exception _ true))))))
