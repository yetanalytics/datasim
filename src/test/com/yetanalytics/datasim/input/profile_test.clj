(ns com.yetanalytics.datasim.input.profile-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.datasim.input.profile :refer [map->Profile]]
            [com.yetanalytics.pan.objects.profile :as profile]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.io :as dio]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.template :as t])
  (:import [java.io File]))

(deftest minimal-profile-test
  (let [minimal-profile (dio/read-loc-json (map->Profile {})
                                           "dev-resources/profiles/minimal.jsonld")]
    (testing "is valid"
      (is (nil? (p/validate minimal-profile))))
    (testing "is valid when written"
      (let [^File tf (File/createTempFile "profiletest" nil)]
        (try
          (dio/write-loc-json minimal-profile tf)
          (is
           (nil?
            (s/explain-data ::profile/profile
                            (json/read-str
                             (slurp tf)
                             :key-fn
                             (partial p/read-key-fn minimal-profile)))))
          (finally
            (.delete tf)))))))
