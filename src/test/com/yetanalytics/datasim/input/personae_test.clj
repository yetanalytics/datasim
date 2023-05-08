(ns com.yetanalytics.datasim.input.personae-test
  (:require [clojure.test :refer [deftest testing is]]
            [com.yetanalytics.datasim.io :as dio]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.datasim.input.personae :refer [map->Personae]]
            [com.yetanalytics.datasim.test-fixtures :as fix]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixtures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def simple-personae
  (dio/read-loc-json (map->Personae {}) fix/simple-personae-filepath))

(def tc3-personae
  (dio/read-loc-json (map->Personae {}) fix/tc3-personae-filepath))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest personae-test
  (testing "personae without roles"
    (is (nil? (p/validate simple-personae)))
    (is (nil? (p/validate tc3-personae))))
  (testing "personae with roles"
    (is (nil? (-> simple-personae
                  (assoc-in [:member 0 :role] "Lead Developer")
                  (assoc-in [:member 1 :role] "Data Engineer")
                  (assoc-in [:member 2 :role] "CEO")
                  p/validate)))
    (is (nil? (-> tc3-personae
                  (assoc-in [:member 0 :role] "Avatar")
                  (assoc-in [:member 1 :role] "Water Tribe Chief")
                  (assoc-in [:member 2 :role] "Earth Queen")
                  (assoc-in [:member 3 :role] "Fire Lord")
                  (assoc-in [:member 4 :role] "Air Nomand")
                  (assoc-in [:member 5 :role] "Cabbage Merchant")
                  p/validate)))))
