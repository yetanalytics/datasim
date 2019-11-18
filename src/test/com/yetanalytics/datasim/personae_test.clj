(ns com.yetanalytics.datasim.personae-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.personae :as personae]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.datasim.io :as dio]))

(deftest read-in-test
  (testing "Read a personae group in from file"
    (let [p (-> (personae/map->Personae {})
                (dio/read-loc "dev-resources/personae/simple.json"))]
      (is (instance? com.yetanalytics.datasim.personae.Personae p))
      (is (satisfies? p/FromInput p))
      (is (satisfies? p/Serializable p))))
  (testing "Validate a personae"
    (let [personae (-> (personae/map->Personae {})
                       (dio/read-loc "dev-resources/personae/simple.json"))]
      (is (nil? (p/validate personae)))
      (is (not
           (nil? (p/validate (assoc
                              personae
                              :objectType nil))))))))
