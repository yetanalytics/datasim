(ns com.yetanalytics.datasim.input.profile-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.input.profile :as profile]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.datasim.io :as dio]))

(deftest read-in-test
  (testing "Read a profile in from file"
    (let [p (-> (profile/map->Profile {})
                (dio/read-loc-json "dev-resources/profiles/cmi5/fixed.json"))]
      (is (instance? com.yetanalytics.datasim.input.profile.Profile p))
      (is (satisfies? p/FromInput p))
      (is (satisfies? p/JSONRepresentable p))))
  (testing "Validate a profile"
    (is (nil? (p/validate (-> (profile/map->Profile {})
                              (dio/read-loc-json "dev-resources/profiles/cmi5/fixed.json")))))
    (is (not
         (nil? (p/validate (assoc
                            (-> (profile/map->Profile {})
                                (dio/read-loc-json "dev-resources/profiles/cmi5/fixed.json"))
                            :type nil)))))))
