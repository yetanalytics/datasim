(ns com.yetanalytics.datasim.profile-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.profile :as profile]
            [com.yetanalytics.datasim.protocols :as p]))

(deftest read-in-test
  (testing "Read a profile in from file"
    (let [p (-> (profile/map->Profile {})
                (p/read-in "dev-resources/profiles/cmi5/fixed.json"))]
      (is (instance? com.yetanalytics.datasim.profile.Profile p))
      (is (satisfies? p/FromInput p))))
  (testing "Validate a profile"
    (is (nil? (p/validate (p/read-in (profile/map->Profile {})
                                     "dev-resources/profiles/cmi5/fixed.json"))))
    (is (not
         (nil? (p/validate (assoc
                            (p/read-in
                             (profile/map->Profile {})
                             "dev-resources/profiles/cmi5/fixed.json")
                            :type nil)))))))
