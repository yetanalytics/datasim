(ns com.yetanalytics.datasim.input.profile-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.data.json :as json]
            [com.yetanalytics.pan :as pan]
            [com.yetanalytics.datasim.input.profile  :as profile]
            [com.yetanalytics.datasim.util.io        :as dio]
            [com.yetanalytics.datasim.test-constants :as const])
  (:import [java.io File]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def minimal-profile
  (dio/read-json-location const/minimal-profile-filepath))

(def minimal-profile-map
  {:id         "https://xapinet.org/xapi/yet/minimal"
   :type       "Profile"
   :_context   "https://w3id.org/xapi/profiles/context"
   :conformsTo "https://w3id.org/xapi/profiles#1.0"
   :prefLabel  {:en "Minimal - Experimental xAPI Profile"}
   :definition {:en "This xAPI Profile demonstrates the minimal required properties of an xAPI profile"}
   :versions   [{:id "https://xapinet.org/xapi/yet/minimal/v1"
                 :generatedAtTime "2020-03-25T15:45:31.907Z"}]
   :author     {:url  "https://www.yetanalytics.com/"
                :name "Yet Analytics"
                :type "Organization"}})

(def cmi5-satisfied-bad-1 "https://w3id.org/xapi/cmi5#satisfiedbad1")
(def cmi5-satisfied-bad-2 "https://w3id.org/xapi/cmi5#satisfiedbad2")
(def cmi5-satisfied-bad-3 "https://w3id.org/xapi/cmi5#satisfiedbad3")
(def cmi5-initialized "https://w3id.org/xapi/cmi5#initialized")
(def cmi5-terminated "https://w3id.org/xapi/cmi5#terminated")
(def cmi5-completed "https://w3id.org/xapi/cmi5#completed")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- profile-key [k]
  (let [kname (name k)]
    (if (= "@context" kname)
      :_context
      (keyword kname))))

(deftest minimal-profile-test 
  (testing "produces the correct profile"
    ;; Coerce `minimal-profile` back into non-record map
    (is (= minimal-profile-map
           (into {} minimal-profile))))
  (testing "is valid"
    (is (nil? (profile/validate-profile minimal-profile))))
  (testing "is valid when written"
    (let [^File tf (File/createTempFile "profiletest" nil)]
      (try
        (dio/write-json-file minimal-profile tf)
        (is (nil? (pan/validate-profile
                   (json/read-str (slurp tf) :key-fn profile-key))))
        (finally
          (.delete tf))))))

(deftest profile-cosmos-validation-test
  (testing "input is valid if all template refs are valid"
    (is (nil? (profile/validate-profiles [const/cmi5-profile]))))
  (testing "input is invalid if invalid template ref iri exists"
    (is (= 1 (->> [(-> const/cmi5-profile
                       (assoc-in [:patterns 0 :zeroOrMore]
                                 "https://w3id.org/xapi/cmi5#bad-template"))]
                  profile/validate-profiles
                  count)))
    ;; XXX: If we replaced satisfiedbad3 with satisfiedbad2, we only get a count
    ;; of 2 errors, not 3.
    (is (= 3
           (->> [(-> const/cmi5-profile
                     (assoc-in [:patterns 0 :zeroOrMore] cmi5-satisfied-bad-1)
                     (assoc-in [:patterns 1 :sequence 0] cmi5-satisfied-bad-2)
                     (assoc-in [:patterns 1 :sequence 2] cmi5-satisfied-bad-3))]
                profile/validate-profiles
                count))))
  (testing "validation works for multi-profile cosmos"
    (is (nil? (profile/validate-profiles
               [const/cmi5-profile const/video-profile])))
    ;; Add connections between Profiles
    (is (nil? (profile/validate-profiles
               [const/cmi5-profile
                (-> const/video-profile
                    (assoc-in [:patterns 0 :sequence 0] cmi5-initialized)
                    (assoc-in [:patterns 0 :sequence 2] cmi5-terminated)
                    (assoc-in [:patterns 1 :alternates 6] cmi5-completed))])))
    (is (= 1 (->> [(-> const/cmi5-profile
                       (assoc-in [:patterns 0 :zeroOrMore]
                                 "https://w3id.org/xapi/cmi5#bad-template"))
                   const/video-profile]
                  profile/validate-profiles
                  count))))
  (testing "fixed / valid profiles"
    (is (nil? (profile/validate-profiles [const/acrossx-profile])))
    (is (nil? (profile/validate-profiles [const/activity-streams-profile])))
    (is (nil? (profile/validate-profiles [const/tc3-profile]))))
  ;; Following tests exist to point out flaws in Profiles
  (testing "invalid profiles"
    ;; AcrossX and ActivityStreams violate spec:
    ;; "related MUST only be used on deprecated Concepts"
    (is (some? (profile/validate-profiles [const/acrossx-profile*])))
    (is (some? (profile/validate-profiles [const/activity-streams-profile*])))))
