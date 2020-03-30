(ns com.yetanalytics.datasim.sim-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.sim :refer :all]
            [com.yetanalytics.datasim.input :as input]
            [clojure.spec.alpha :as s]
            [xapi-schema.spec :as xs]))

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
    (let [skeleton (build-skeleton (assoc-in valid-input
                                             [:parameters :end] nil))]
      (are [actor-id] (let [statement-seq (get skeleton
                                               actor-id)
                            f1 (future (nth statement-seq 1000))
                            f2 (future (nth statement-seq 1000))]
                        (= @f1 @f2))
        "mbox::mailto:alice@example.org"
        "mbox::mailto:bob@example.org"
        "mbox::mailto:fred@example.org"))))

(deftest xapi-test
  (testing "sim returns valid xapi statements"
    (let [skeleton (build-skeleton (assoc-in valid-input
                                             [:parameters :end] nil))]
      (are [actor-id] (s/valid? (s/every ::xs/statement)
                                (get skeleton
                                     actor-id))
        "mbox::mailto:alice@example.org"
        "mbox::mailto:bob@example.org"
        "mbox::mailto:fred@example.org"))))

(deftest stack-test
  (testing "that we can iterate for a long time w/o a stack overflow"
    (is (s/valid? ::xs/statement
                  (-> valid-input
                      (assoc-in [:parameters :end] nil)
                      build-skeleton
                      (get "mbox::mailto:bob@example.org")
                      (nth 10000))))))

(deftest sim-seq-test
  (testing "returns statements"
    (is (s/valid? (s/every ::xs/statement) (sim-seq valid-input))))
  (testing "respects max param"
    (let [ret (sim-seq (assoc-in valid-input [:parameters :max] 3))]
      (is (s/valid? (s/every ::xs/statement) ret))
      (is (= 3 (count ret)))))
  (testing "respects from param"
    (let [[s0 s1 & _] (sim-seq valid-input)
          [s1' & _]   (sim-seq (assoc-in valid-input
                                         [:parameters
                                          :from]
                                         (get s0 "timestamp")))]
      (is (not= s0 s1'))
      (is (= s1 s1')))))
