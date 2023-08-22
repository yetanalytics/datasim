(ns com.yetanalytics.datasim.input-test
  (:require [clojure.test :refer [deftest testing is are]]
            [com.yetanalytics.datasim.input
             :refer [from-location validate validate-throw]
             :as input]
            [com.yetanalytics.datasim.test-constants :as const])
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
           (testing "is valid"
             (is (nil? (validate input-key x))))
           (testing (format "is invalid due to %s" invalid-reason)
             (is (some? (validate input-key (invalidator-fn x))))
             (is (thrown? ExceptionInfo
                          (validate-throw input-key (invalidator-fn x)))))))
    "xAPI Profile" "non-IRI ID" 
    :profile const/cmi5-profile-filepath
    #(assoc % :id "foo")

    "Actor Personae" "empty `:members` coll"
    :personae const/simple-personae-filepath
    #(assoc % :member [])
    
    "Actor Models" "invalid due to invalid alignments"
    :models const/simple-models-filepath
    #(conj % {:personae   [{:id   "notanid"
                            :type "notatype"}]
              :alignments [{:component "notaniri"
                            :weight    "bar"}]})
    
    "Actor Models, Long" "invalid alignments"
    :models const/tc3-models-filepath
    #(conj % {:personae   [{:id   "notanid"
                            :type "notatype"}]
              :alignments [{:component "notaniri"
                            :weight    "bar"}]})
    
    "Actor Models w/ Overrides" "invalid alignments"
    :models const/overrides-models-filepath
    #(conj % {:personae        [{:id   "notanid"
                                 :type "notatype"}]
              :objectOverrides [{:component "notaniri"
                                 :weight    "bar"}]})
    
    "Simulation Parameters" "non-numeric seed"
    :parameters const/simple-parameters-filepath
    #(assoc % :seed "hey")
    
    "Combined Input Spec" "`:profiles` not being a vector"
    :input const/simple-input-filepath
    #(update % :profiles first)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input Validation Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest subobject-validation-test
  (testing "input is valid with a minimal profile"
    (is (nil? (input/validate
               :input
               (assoc-in const/simple-input
                         [:profiles 0]
                         const/minimal-profile))))))

(deftest combined-input-validation-test
  (testing "combined input is valid"
    (is (nil? (validate :input const/simple-input)))
    (is (try (validate-throw :input const/simple-input)
             true
             (catch Exception _ false))))
  (testing "combined input is invalid"
    (testing "with invalid gen-profiles"
      (is (try
            (validate-throw
             :input
             (assoc-in const/simple-input
                       [:parameters :gen-profiles]
                       ["http://example.com/nonexistent.jsonld"]))
            false
            (catch Exception _ true))))
    (testing "with invalid gen-patterns"
      (is (try
            (validate-throw
             :input
             (assoc-in const/simple-input
                       [:parameters :gen-patterns]
                       ["http://example.com/nonexistent#pattern"]))
            false
            (catch Exception _ true))))))
