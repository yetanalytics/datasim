(ns com.yetanalytics.datasim.alignments-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.alignments :as alignments]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.datasim.io :as dio]))

(deftest read-in-test
  (testing "Read alignments in from file"
    (let [p (-> (alignments/map->Alignments {})
                (dio/read-loc "dev-resources/alignments/simple.json"))]
      (is (instance? com.yetanalytics.datasim.alignments.Alignments p))
      (is (satisfies? p/FromInput p))
      (is (satisfies? p/Serializable p))))
  (testing "Validate alignments"
    (let [alignments (-> (alignments/map->Alignments {})
                       (dio/read-loc "dev-resources/alignments/simple.json"))]
      (is (nil? (p/validate alignments))))))
