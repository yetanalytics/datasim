(ns com.yetanalytics.datasim.input.profile-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.datasim.input.profile :refer :all]
            [com.yetanalytics.datasim.io :as dio]
            [clojure.template :as t]))

(deftest minimal-profile-test
  (let [minimal-profile (dio/read-loc-json (map->Profile {})
                                           "dev-resources/profiles/minimal.jsonld")]
    (testing "is valid"
      (is (nil? (p/validate minimal-profile))))))
