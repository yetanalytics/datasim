(ns com.yetanalytics.datasim.input-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.datasim.input 
             :refer [from-location validate validate-throw]
             :as input]
            [clojure.template :as t]))

;; simple property test to cover the input types

(deftest input-objects-test
  (t/do-template
   [nm         ;; friendly name
    k           ;; key
    loc         ;; location of good json
    invalidator ;; a function to make it invalid
    ]
   (testing (format "testing %s:" nm)
     (let [x (from-location k :json loc)]
       (testing "protocols"
         (is (satisfies? p/FromInput x))
         (is (satisfies? p/JSONRepresentable x)))
       (testing "validation"
         (is (nil? (validate x)))
         (is (some? (validate (invalidator x))))
         (is (thrown? clojure.lang.ExceptionInfo
                      (validate-throw (invalidator x)))))))
   "xAPI Profile" :profile "dev-resources/profiles/cmi5/fixed.json"
   #(assoc % :id "foo") ;; profiles need an IRI ID

   "Actor Personae" :personae "dev-resources/personae/simple.json"
   #(assoc % :member []) ;; groups need members

   "Actor Alignments" :alignments "dev-resources/alignments/simple.json"
   #(assoc % :alignment-vector [{:id "notanid" :alignments [{:component "notaniri" :weight "bar"}]}]) ;;alignments are a vector of maps containing a vector of maps

   "Simulation Parameters" :parameters "dev-resources/parameters/simple.json"
   #(assoc % :seed "hey") ;; seed is a number

   "Combined Input Spec" :input "dev-resources/input/simple.json"
   #(update % :profiles first) ;; profiles are a vector
   ))

(deftest profile-cosmos-validation-test
  (testing "input is valid if all template refs are valid"
    (is (nil? (s/explain-data
               ::input/profiles
               [(from-location :profile :json "dev-resources/profiles/cmi5/fixed.json")]))))
  (testing "input is invalid if invalid template ref iri exists"
    (is (some? (s/explain-data
                ::input/profiles
                [(-> (from-location :profile :json "dev-resources/profiles/cmi5/fixed.json")
                     (assoc-in [:patterns 0 :zeroOrMore]
                               "https://w3id.org/xapi/cmi5#bad-template"))]))))
  ;; Both the cmi5 and video profiles have Patterns
  ;; So does the tc3 profile, but it violates the "alternates MUST NOT contain zeroOrMore" spec
  (testing "validation works for multi-profile cosmos"
    (is (nil? (s/explain-data
               ::input/profiles
               [(from-location :profile :json "dev-resources/profiles/cmi5/fixed.json")
                (from-location :profile :json "dev-resources/profiles/video/profile.jsonld")])))
    (is (nil? (s/explain-data
               ::input/profiles
               [(from-location :profile :json "dev-resources/profiles/cmi5/fixed.json")
                (-> (from-location :profile :json "dev-resources/profiles/video/profile.jsonld")
                    (assoc-in [:patterns 0 :sequence 0]
                              "https://w3id.org/xapi/cmi5#initialized")
                    (assoc-in [:patterns 0 :sequence 2]
                              "https://w3id.org/xapi/cmi5#terminated")
                    (assoc-in [:patterns 1 :alternates 6]
                              "https://w3id.org/xapi/cmi5#completed"))])))
    (is (some? (s/explain-data
                ::input/profiles
                [(-> (from-location :profile :json "dev-resources/profiles/cmi5/fixed.json")
                     (assoc-in [:patterns 0 :zeroOrMore]
                               "https://w3id.org/xapi/cmi5#bad-template"))
                 (from-location :profile :json "dev-resources/profiles/video/profile.jsonld")])))))

(deftest subobject-validation-test
  (testing "input is valid with a minimal profile"
    (is (nil? (p/validate (assoc-in (from-location
                                     :input :json "dev-resources/input/simple.json")
                                    [:profiles 0]
                                    (from-location
                                     :profile :json
                                     "dev-resources/profiles/minimal.jsonld")))))))
