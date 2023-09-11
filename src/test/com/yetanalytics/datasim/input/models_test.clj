(ns com.yetanalytics.datasim.input.models-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.input.model :as model]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def alignment-1 {:id "http://www.whateveer.com/activity1"
                  :weight 0.9
                  :period {:min  2.1
                           :mean 3
                           :unit "weeks"}})

(def alignment-2 {:id "http://www.whateveer.com/activity2"
                  :weight 0.8
                  :bounds [{:seconds     [1 2 3]
                            :minutes     [1]
                            :hours       [[8 12]]
                            :daysOfWeek  ["Sunday" "Tuesday" "Thursday"]
                            :daysOfMonth [[1 10] [21 30]]
                            :months      [1 ["April" "May"]]
                            :years       [2023]}]
                  :period {:min  2
                           :mean 3.2
                           :unit "millis"}
                  :repeat-max 10})

(def persona-1 {:id   "mbox::mailto:cliff@yetanalytics.com"
                :type "Agent"})

(def persona-2 {:id   "mbox::mailto:milt@yetanalytics.com"
                :type "Agent"})

(def model-1 {:personae   [persona-1]
              :alignments [alignment-1 alignment-2]})

(def model-2 {:personae   [persona-2]
              :alignments [alignment-1 alignment-2]})

(def object-override-example
  {:object {:objectType "Activity"
            :id "https://www.whatever.com/activities#course1"
            :definition {:name {:en-US "Course 1"}
                         :description {:en-US "Course Description 1"}
                         :type "http://adlnet.gov/expapi/activities/course"}}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest alignments-test
  (testing "valid personae and alignments"
    (is (s/valid? ::model/personae [persona-1 persona-2]))
    (is (s/valid? ::model/alignments [alignment-1 alignment-2]))
    (is (s/valid? ::model/models [model-1 model-2])))
  (testing "invalid persona ids"
    (is (not (s/valid? ::model/personae
                       [(assoc persona-1 :id "foo")])))
    (is (not (s/valid? ::model/personae
                       [(assoc persona-1 :id "foo" :type "Group")])))
    (is (s/valid? ::model/personae ; Role ID can be an aribtrary string
                  [(assoc persona-1 :id "foo" :type "Role")])))
  (testing "invalid persona type"
    (is (not (s/valid? ::model/personae
                       [(assoc persona-1 :type "FooBar")])))
    (is (not (s/valid? ::model/personae
                       [(assoc persona-1 :type "FooBar" :id "qux")]))))
  (testing "invalid temporal properties"
    (is (not (s/valid? ::model/alignments
                       [(assoc-in alignment-1 [:bounds 0 :minutes] 1)])))
    (is (not (s/valid? ::model/alignments
                       [(assoc-in alignment-1 [:bounds 0 :minutes] [[2 1]])])))
    (is (not (s/valid? ::model/alignments
                       [(assoc-in alignment-1 [:bounds 0 :minutes] [60])])))
    (is (not (s/valid? ::model/alignments
                       [(assoc-in alignment-1 [:period :mean] 0)])))
    (is (not (s/valid? ::model/alignments
                       [(assoc-in alignment-1 [:period :mean] -3)])))
    (is (not (s/valid? ::model/alignments
                       [(assoc-in alignment-1 [:period :mean] "4")])))
    (is (not (s/valid? ::model/alignments
                       [(assoc-in alignment-1 [:period :min] -1.2)])))
    (is (not (s/valid? ::model/alignments
                       [(assoc-in alignment-1 [:period :min] "3")])))
    (is (not (s/valid? ::model/alignments
                       [(assoc-in alignment-1 [:period :unit] "months")]))))
  (testing "object overrides"
    (is (s/valid? ::model/objectOverrides
                  [object-override-example]))
    (is (s/valid? ::model/objectOverrides
                  [(assoc object-override-example :weight 1.0)]))
    (is (not (s/valid?
              ::model/objectOverrides
              [(->> {(keyword "https://foo.org") true}
                    (assoc-in object-override-example [:object :definition :extensions]))])))))
