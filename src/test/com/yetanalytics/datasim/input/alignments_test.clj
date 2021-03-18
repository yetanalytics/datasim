(ns com.yetanalytics.datasim.input.alignments-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.input.alignments :as a]))

(def alignment1 {:component "http://www.whateveer.com/activity1"
                 :weight 0.9})

(def alignment2 {:component "http://www.whateveer.com/activity2"
                 :weight 0.8})
  
(def actor-alignment1 {:id "mbox::mailto:cliff@yetanalytics.com"
                       :type "Agent"
                       :alignments [alignment1 alignment2]})
  
(def actor-alignment2 {:id "mbox::mailto:cliff1@yetanalytics.com"
                       :type "Agent"
                       :alignments [alignment1 alignment2]})
  
(def alignments-example [actor-alignment1 actor-alignment2])

(deftest alignments-test
  (testing "valid alignments"
    (is (s/valid? ::a/alignment alignment1))
    (is (s/valid? ::a/alignment alignment2))
    (is (s/valid? ::a/alignments [alignment1 alignment2]))
    (is (s/valid? ::a/actor-alignment actor-alignment1))
    (is (s/valid? ::a/actor-alignment actor-alignment2))
    (is (s/valid? ::a/alignment-vector alignments-example)))
  (testing "invalid agent alignment ids"
    (is (not (s/valid? ::a/actor-alignment
                       (assoc actor-alignment1 :id "foo"))))
    (is (s/valid? ::a/actor-alignment
                  (assoc actor-alignment1 :id "foo" :type "Group")))
    (is (s/valid? ::a/actor-alignment
                  (assoc actor-alignment1 :id "foo" :type "Role"))))
  (testing "invalid actor alignment type"
    (is (not (s/valid? ::a/actor-alignment
                       (assoc actor-alignment1 :type "FooBar"))))
    (is (not (s/valid? ::a/actor-alignment
                       (assoc actor-alignment1 :type "FooBar" :id "qux"))))))

;; TODO: protocols test, e.g. (satisfies? p/FromInput alignments-example)
