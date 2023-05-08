(ns com.yetanalytics.datasim.input.profile-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.data.json :as json]
            [com.yetanalytics.pan :as pan]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.datasim.input.profile :refer [map->Profile]]
            [com.yetanalytics.datasim.io :as dio]
            [com.yetanalytics.datasim.test-constants :as const])
  (:import [java.io File]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def minimal-profile
  (dio/read-loc-json (map->Profile {}) const/minimal-profile-filepath))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest minimal-profile-test 
  (testing "produces the correct profile"
    ;; Coerce `minimal-profile` back into non-record map
    (is (= minimal-profile-map
           (into {} minimal-profile))))
  (testing "is valid"
    (is (nil? (p/validate minimal-profile))))
  (testing "is valid when written"
    (let [^File tf (File/createTempFile "profiletest" nil)]
      (try
        (dio/write-loc-json minimal-profile tf)
        (is (nil? (pan/validate-profile
                   (json/read-str (slurp tf)
                                  :key-fn
                                  (partial p/read-key-fn minimal-profile)))))
        (finally
          (.delete tf))))))
