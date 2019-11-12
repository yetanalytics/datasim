(ns com.yetanalytics.datasim.profile-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.profile :as profile]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.datasim.io :as dio]))

(deftest read-in-test
  (testing "Read a profile in from file"
    (let [p (-> (profile/map->Profile {})
                (dio/read-loc "dev-resources/profiles/cmi5/fixed.json"))]
      (is (instance? com.yetanalytics.datasim.profile.Profile p))
      (is (satisfies? p/FromInput p))
      (is (satisfies? p/Serializable p))))
  (testing "Validate a profile"
    (is (nil? (p/validate (-> (profile/map->Profile {})
                              (dio/read-loc "dev-resources/profiles/cmi5/fixed.json")))))
    (is (not
         (nil? (p/validate (assoc
                            (-> (profile/map->Profile {})
                                (dio/read-loc "dev-resources/profiles/cmi5/fixed.json"))
                            :type nil)))))))
