(ns com.yetanalytics.datasim.input.models-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.input.model :as model]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def verb-alignment
  {:id     "http://www.whatever.com/verb"
   :weight 0.99})

(def activity-alignment
  {:id     "http://www.whatever.com/activity"
   :weight 0.12})

(def activity-type-alignment
  {:id     "http://www.whatever.com/activity-type"
   :weight 0.31})

(def pat-alignment-1
  {:id      "http://www.whatever.com/pattern1"
   :weights [{:id     nil
              :weight 0.1}
             {:id     "http://www.whatever.com/pattern1/child"
              :weight 0.9}]
   :periods [{:min  2.1
              :mean 3
              :unit "weeks"}]})

(def pat-alignment-2
  {:id           "http://www.whatever.com/pattern2"
   :weights      [{:id     "http://www.whatever.com/pattern2/child1"
                   :weight 0.8}
                  {:id     "http://www.whatever.com/pattern2/child2"
                   :weight 0.2}]
   :bounds       [{:seconds     [1 2 3]
                   :minutes     [[0 59 2]]
                   :hours       [[8 12]]
                   :daysOfWeek  ["Sunday" "Tuesday" "Thursday"]
                   :daysOfMonth [[1 10] [21 30]]
                   :months      [1 ["April" "May"]]
                   :years       [2023 2024]}]
   :boundRetries ["http://www.whatever.com/pattern1"]
   :periods      [{:min    2
                   :mean   3.2
                   :unit   "millis"
                   :bounds [{:years [2023]}]}
                  {:min    8
                   :mean   1.1
                   :unit   "millis"
                   :bounds [{:years [2024]}]}
                  {:fixed 2
                   :mean  3.1 ; would be ignored
                   :unit  "millis"}]
   :repeatMax    10})

(def template-alignment
  {:id     "http://www.whatever.com/template"
   :period {:mean 30
            :unit "days"}})

(def persona-1 {:id   "mbox::mailto:cliff@yetanalytics.com"
                :type "Agent"})

(def persona-2 {:id   "mbox::mailto:milt@yetanalytics.com"
                :type "Agent"})

(def model-1 {:personae      [persona-1]
              :verbs         [verb-alignment]
              :activities    [activity-alignment]
              :activityTypes [activity-type-alignment]
              :patterns      [pat-alignment-1 pat-alignment-2]
              :templates     [template-alignment]})

(def model-2 {:personae [persona-2]
              :verbs    [verb-alignment]
              :patterns [pat-alignment-1 pat-alignment-2]})

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
    (is (s/valid? ::model/verbs [verb-alignment]))
    (is (s/valid? ::model/activities [activity-alignment]))
    (is (s/valid? ::model/activityTypes [activity-type-alignment]))
    (is (s/valid? ::model/patterns [pat-alignment-1 pat-alignment-2]))
    (is (s/valid? ::model/templates [template-alignment]))
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
    (is (not (s/valid? ::model/patterns
                       [(assoc-in pat-alignment-1 [:bounds 0 :minutes] 1)])))
    (is (not (s/valid? ::model/patterns
                       [(assoc-in pat-alignment-1 [:bounds 0 :minutes] [[2 1]])])))
    (is (not (s/valid? ::model/patterns
                       [(assoc-in pat-alignment-1 [:bounds 0 :minutes] [60])])))
    (is (not (s/valid? ::model/patterns
                       [(assoc-in pat-alignment-1 [:periods 0 :mean] 0)])))
    (is (not (s/valid? ::model/patterns
                       [(assoc-in pat-alignment-1 [:periods 0 :mean] -3)])))
    (is (not (s/valid? ::model/patterns
                       [(assoc-in pat-alignment-1 [:periods 0 :mean] "4")])))
    (is (not (s/valid? ::model/patterns
                       [(assoc-in pat-alignment-1 [:periods 0 :min] -1.2)])))
    (is (not (s/valid? ::model/patterns
                       [(assoc-in pat-alignment-1 [:periods 0 :min] "3")])))
    (is (not (s/valid? ::model/patterns
                       [(assoc-in pat-alignment-1 [:periods 0 :unit] "months")])))
    (is (not (s/valid? ::model/patterns
                       [(assoc-in pat-alignment-2 [:periods 2 :bounds] [{:years [2024]}])]))))
  (testing "object overrides"
    (is (s/valid? ::model/objectOverrides
                  [object-override-example]))
    (is (s/valid? ::model/objectOverrides
                  [(assoc object-override-example :weight 1.0)]))
    (is (not (s/valid?
              ::model/objectOverrides
              [(->> {(keyword "https://foo.org") true}
                    (assoc-in object-override-example [:object :definition :extensions]))])))))
