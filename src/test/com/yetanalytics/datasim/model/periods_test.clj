(ns com.yetanalytics.datasim.model.periods-test
  (:require [clojure.test :refer [deftest testing is are]]
            [java-time.api :as t]
            [com.yetanalytics.datasim.util.random   :as random]
            [com.yetanalytics.datasim.model.periods :as periods]))

(def new-years-date-time
  "The first moment of 2023, 2023-01-01T00:00:00, as a LocalDateTime"
  (t/local-date-time 2023 1 1 0 0 0))

(deftest time-period-test
  (testing "`convert-periods?` function"
    (is (= [{:min  2
             :mean 2}
            {:min  2000
             :mean 2000}
            {:min  120000
             :mean 120000}
            {:min  7200000
             :mean 7200000}
            {:min  172800000
             :mean 172800000}
            {:min  1209600000
             :mean 1209600000}]
           (periods/convert-periods
            [{:min   2
              :mean  2
              :unit  "millis"}
             {:min   2
              :mean  2
              :unit  "seconds"}
             {:min   2
              :mean  2
              :unit  "minutes"}
             {:min   2
              :mean  2
              :unit  "hours"}
             {:min   2
              :mean  2
              :unit  "days"}
             {:min   2
              :mean  2
              :unit  "weeks"}])))
    (is (= [{:fixed 2}
            {:fixed 2000}
            {:fixed 120000}
            {:fixed 7200000}
            {:fixed 172800000}
            {:fixed 1209600000}]
           (periods/convert-periods
            [{:fixed 2
              :unit "millis"}
             {:fixed 2
              :unit "seconds"}
             {:fixed 2
              :unit "minutes"}
             {:fixed 2
              :unit "hours"}
             {:fixed 2
              :unit "days"}
             {:fixed 2
              :unit "weeks"}])))
    (is (= [{:min  60000 ; minute default
             :mean 60000}
            {:fixed 60000}]
           (periods/convert-periods
            [{:min  1
              :mean 1}
             {:min   1
              :mean  1
              :fixed 1}]))))
  (testing "`add-periods` function"
    (testing "(fixed times)"
      (are [expected periods]
           (= (t/local-date-time expected)
              (periods/add-periods
               new-years-date-time
               (random/seed-rng 100) ; rng doesn't matter here
               (periods/convert-periods periods)))
        "2023-01-01T00:01:00" [{:fixed 1}]
        "2023-01-01T00:30:00" [{:fixed 1
                                :bounds [{:years [2024]}]}
                               {:fixed 30
                                :bounds [{:years [2023]}]}]))
    (testing "(minimum times)"
      (are [expected-min periods]
           (t/before?
            (t/local-date-time expected-min)
            (periods/add-periods
             new-years-date-time
             (random/rng)
             (periods/convert-periods periods)))
        "2023-01-01T00:00:30" [{:min  30
                                :unit "seconds"}]
        "2023-01-01T00:30:00" [{:min  30
                                :unit "minutes"}]
        "2023-01-01T12:00:00" [{:min  12
                                :unit "hours"}]
        "2023-01-10T00:00:00" [{:min 10
                                :unit "days"}]
        "2023-01-14T00:00:00" [{:min 2
                                :unit "weeks"}]))
    (testing "(mean times)"
      ;; We use 6 * sd = 6 * mean (b/c exponential distribution)
      (are [expected-max periods]
           (t/before?
            new-years-date-time
            (periods/add-periods
             new-years-date-time
             (random/rng)
             (periods/convert-periods periods))
            (t/local-date-time expected-max))
        "2023-01-01T00:03:00" [{:mean 30
                                :unit "seconds"}]
        "2023-01-01T00:30:00" [{:mean 5
                                :unit "minutes"}]
        "2023-01-01T12:00:00" [{:mean 2
                                :unit "hours"}]
        "2023-01-06T00:00:00" [{:mean 1
                                :unit "days"}]
        "2023-01-21T00:00:00" [{:mean 0.5
                                :unit "weeks"}]))))
