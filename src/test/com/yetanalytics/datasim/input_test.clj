(ns com.yetanalytics.datasim.input-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.datasim.input :refer :all]
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
   #(assoc-in % [:alignment-map "notanid"] "bar") ;; alignments are a map of strings to maps...

   "Simulation Parameters" :parameters "dev-resources/parameters/simple.json"
   #(assoc % :seed "hey") ;; seed is a number

   "Combined Input Spec" :input "dev-resources/input/simple.json"
   #(update % :profiles first) ;; profiles are a vector
   ))
