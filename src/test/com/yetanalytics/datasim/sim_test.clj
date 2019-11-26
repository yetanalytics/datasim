(ns com.yetanalytics.datasim.sim-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.sim :refer :all]
            [com.yetanalytics.datasim.input :as input]
            [clojure.spec.alpha :as s]))

(def valid-input
  (input/from-location
   :input :json "dev-resources/input/simple.json"))

(deftest build-skeleton-test
  (testing "given valid input, returns a valid skeleton"
    (is (s/valid?
         :com.yetanalytics.datasim.sim/skeleton
         (build-skeleton
          valid-input)))))

(deftest disjoint-rng-test
  (testing "Make sure RNGs aren't shared across threads."
    (let [skeleton (build-skeleton valid-input)]
      (are [actor-id] (let [prob-seq (get-in skeleton
                                             [actor-id
                                              :prob-seq])
                            f1 (future (nth prob-seq 1000))
                            f2 (future (nth prob-seq 1000))]
                        (= @f1 @f2))
        "mbox::mailto:alice@example.org"
        "mbox::mailto:bob@example.org"
        "mbox::mailto:fred@example.org"))))
