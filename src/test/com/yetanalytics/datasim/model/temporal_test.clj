(ns com.yetanalytics.datasim.model.temporal-test
  (:require [clojure.test :refer [deftest testing is are]]
            [java-time.api :as t]
            [com.yetanalytics.datasim.math.random :as random]
            [com.yetanalytics.datasim.model.temporal :as temporal]))

(deftest time-helpers-test
  (testing "time-map"
    (is (= {:year         2023
            :month        9
            :day-of-month 19
            :day-of-week  2
            :hour         11
            :minute       12
            :second       13}
           (temporal/time-map
            (t/local-date-time 2023 9 19 11 12 13)))))
  (testing "`leap-year?` function"
    (is (true? (temporal/leap-year? 2024)))
    (is (true? (temporal/leap-year? 2000)))
    (is (false? (temporal/leap-year? 2023)))
    (is (false? (temporal/leap-year? 1900))))
  (testing "`day-of-month?` function"
    (testing "(non-leap year)"
      (are [month days]
           (->> days
                (map (fn [d] (temporal/day-of-month? 2023 month d)))
                (every? true?))
        1 (range 32)
        2 (range 29)
        3 (range 32)
        4 (range 31)
        5 (range 32)
        6 (range 31)
        7 (range 32)
        8 (range 32)
        9 (range 31)
        10 (range 32)
        11 (range 31)
        12 (range 32))
      (are [month day]
           (false? (temporal/day-of-month? 2023 month day))
        1 32
        2 29
        3 32
        4 31
        5 32
        6 32
        6 31
        7 32
        8 32
        9 31
        10 32
        11 31
        12 32))
    (testing "(leap year)"
      (are [month days]
           (->> days
                (map (fn [d] (temporal/day-of-month? 2024 month d)))
                (every? true?))
        1 (range 32)
        2 (range 30) ; different from non-leap year
        3 (range 32)
        4 (range 31)
        5 (range 32)
        6 (range 31)
        7 (range 32)
        8 (range 32)
        9 (range 31)
        10 (range 32)
        11 (range 31)
        12 (range 32))))
  (testing "`day-of-week` function"
    (is (= 2 ; Tuesday
           (temporal/day-of-week 2023 9 19)))
    (is (= 0 ; Sunday
           (temporal/day-of-week 2023 1 1)))
    (is (= 1 ; Monday
           (temporal/day-of-week 2024 9 9)))))

(def new-years-date-time
  "The first moment of 2023, 2023-01-01T00:00:00, as a LocalDateTime"
  (t/local-date-time 2023 1 1 0 0 0))

(def year-midpoint-date-time
  "The middle of 2023, 2023-06-15T12:30:30, as a LocalDateTime"
  (t/local-date-time 2023 6 15 12 30 30))

(deftest bounds-test
  (testing "`convert-bounds` function"
    (is (= [{:ranges {:seconds       '(1 2 3)
                      :minutes       '(1)
                      :hours         '(8 9 10 11 12)
                      :days-of-week  '(0 2 4)
                      :days-of-month '(1 2 3 4 5 6 7 8 9 10 21 22 23 24 25 26 27 28 29 30)
                      :months        '(1 4 5)
                      :years         '(2023 2024)}
             :sets   {:seconds       #{1 2 3}
                      :minutes       #{1}
                      :hours         #{8 9 10 11 12}
                      :days-of-week  #{0 2 4}
                      :days-of-month #{1 2 3 4 5 6 7 8 9 10 21 22 23 24 25 26 27 28 29 30}
                      :months        #{1 4 5}
                      :years         #{2023 2024}}}]
           (temporal/convert-bounds
            [{:seconds     [1 2 3]
              :minutes     [1]
              :hours       [[8 12]]
              :daysOfWeek  ["Sunday" "Tuesday" "Thursday"]
              :daysOfMonth [[1 10] [21 30]]
              :months      [1 ["April" "May"]]
              :years       [2023 2024]}])))
    (is (= [{:ranges {:years '(2021 2022 2023 2024)}
             :sets   {:years #{2021 2022 2023 2024}}}
            {:ranges {:years '(1971 1972 1973 1974)}
             :sets   {:years #{1971 1972 1973 1974}}}]
           (temporal/convert-bounds
            [{:years [2024 2023 2022 2021]}
             {:years [1974 1973 1972 1971]}]))))
  (testing "`bounded-time?` function"
    (testing "(always true if bounds are empty)"
      (is (true? (temporal/bounded-time?
                  nil
                  (t/local-date-time (t/instant) "UTC"))))
      (is (true? (temporal/bounded-time?
                  []
                  (t/local-date-time (t/instant) "UTC")))))
    (testing "(non-empty bounds)"
      (are [res bounds]
           (= res (temporal/bounded-time?
                   (temporal/convert-bounds bounds)
                   new-years-date-time))
        ;; year bounds
        true  [{:years [2023 2024]}]
        true  [{:years [[2023 2024]]}]
        true  [{:years [2023]} {:years [2024]}]
        false [{:years [2024]}]
        ;; month bounds
        true  [{:years  [2023]
                :months [1 2 3]}]
        true  [{:months [1 2 3]}]
        true  [{:years  [2023]
                :months [1]}
               {:years  [2024]
                :months [2]}]
        false [{:years  [2023]
                :months [2 3 4]}]
        false [{:months [2 3 4]}]
        ;; day of month bounds
        true  [{:years       [2023]
                :months      [1]
                :daysOfMonth [1 2 3]}]
        true  [{:years       [2023]
                :months      [1]
                :daysOfMonth [[1 3]]}]
        true  [{:years       [2023]
                :daysOfMonth [[1 3]]}
               {:years       [2024]
                :daysOfMonth [[4 6]]}]
        true  [{:daysOfMonth [[1 12]]}]
        false [{:years       [2023]
                :months      [1]
                :daysOfMonth [4 5 6]}]
        ;; day of week bounds
        true  [{:years       [2023]
                :months      [1]
                :daysOfWeek  [0]}]
        true  [{:years      [2023]
                :months     [1]
                :daysOfWeek [[0 6]]}]
        false [{:years      [2023]
                :months     [1]
                :daysOfWeek [[1 6]]}]
        true  [{:years       [2023]
                :months      [1]
                :daysOfWeek  [0]
                :daysOfMonth [1]}]
        false [{:years       [2023]
                :months      [1]
                :daysOfWeek  [0]
                :daysOfMonth [2]}]
        ;; hour bounds
        true  [{:years       [2023]
                :months      [1]
                :daysOfMonth [1]
                :hours       [0]}]
        false [{:years       [2023]
                :months      [1]
                :daysOfMonth [1]
                :hours       [1]}]
        true  [{:hours [0]}]
        true  [{:hours [[0 14]]}]
        true  [{:daysOfWeek [1]
                :hours      [[12 23]]}
               {:daysOfWeek [0]
                :hours      [[0 11]]}]
        false [{:daysOfWeek [0]
                :hours      [[12 23]]}
               {:daysOfWeek [1]
                :hours      [[0 11]]}]
        ;; minute bounds
        true  [{:years       [2023]
                :months      [1]
                :daysOfMonth [1]
                :hour        [0]
                :minutes     [0]}]
        false [{:years       [2023]
                :months      [1]
                :daysOfMonth [1]
                :hour        [0]
                :minutes     [1]}]
        true  [{:minutes [[0 29]]}]
        true  [{:minutes [[30 59]]}
               {:minutes [[0 29]]}]
        false [{:minutes [[30 59]]}]
        ;; second bounds
        true  [{:years       [2023]
                :months      [1]
                :daysOfMonth [1]
                :hour        [0]
                :minutes     [0]
                :seconds     [0]}]
        false [{:years       [2023]
                :months      [1]
                :daysOfMonth [1]
                :hour        [0]
                :minutes     [0]
                :seconds     [1]}]
        true  [{:seconds [[0 29]]}]
        true  [{:seconds [[30 59]]}
               {:seconds [[0 29]]}]
        false [{:seconds [[30 59]]}])))
  (testing "`next-bounded-time` function"
    (are [expected bounds]
         (= (t/local-date-time expected)
            (temporal/next-bounded-time
             (temporal/convert-bounds bounds)
             new-years-date-time))
      "2023-01-01T00:00:00" [{:years [2023 2024]}] ; no change
      "2024-01-01T00:00:00" [{:years [2024]}]
      "2023-02-01T00:00:00" [{:months [2]}]
      "2023-01-02T00:00:00" [{:daysOfMonth [2]}]
      "2023-01-02T00:00:00" [{:daysOfWeek [1]}]
      "2023-01-01T01:00:00" [{:hours [1]}]
      "2023-01-01T00:01:00" [{:minutes [1]}]
      "2023-01-01T00:00:01" [{:seconds [1]}]
      "2024-02-02T01:01:01" [{:years       [2024]
                              :months      [2]
                              :daysOfMonth [2]
                              :daysOfWeek  ["Friday"]
                              :hours       [1]
                              :minutes     [1]
                              :seconds     [1]}]
      "2025-01-01T00:00:00" [{:years [2025]}
                             {:years [2024]}]
      "2023-01-01T00:00:00" [{:years       [2023] ; no change
                              :months      [1]
                              :daysOfMonth [1]
                              :daysOfWeek  [0]
                              :hours       [0]
                              :minutes     [0]
                              :seconds     [0]}])
    (are [expected bounds]
         (= (t/local-date-time expected)
            (temporal/next-bounded-time
             (temporal/convert-bounds bounds)
             year-midpoint-date-time))
      "2023-06-15T12:30:30" [{:years       [2023] ; no change
                              :months      [6]
                              :daysOfMonth [15]
                              :daysOfWeek  ["Thursday"]
                              :hours       [12]
                              :minutes     [30]
                              :seconds     [30]}]
      "2023-07-01T00:00:00" [{:months [7]}]
      "2024-05-01T00:00:00" [{:months [5]}]
      "2023-06-16T00:00:00" [{:daysOfMonth [16]}]
      "2023-07-14T00:00:00" [{:daysOfMonth [14]}]
      "2023-06-16T00:00:00" [{:daysOfWeek ["Friday"]}]
      "2023-06-21T00:00:00" [{:daysOfWeek ["Wednesday"]}]
      "2023-06-15T13:00:00" [{:hours [13]}]
      "2023-06-16T11:00:00" [{:hours [11]}]
      "2023-06-15T12:31:00" [{:minutes [31]}]
      "2023-06-15T13:29:00" [{:minutes [29]}]
      "2023-06-15T12:30:31" [{:seconds [31]}]
      "2023-06-15T12:31:29" [{:seconds [29]}])
    (testing "(time bound exceeded)"
      (is (nil? (temporal/next-bounded-time
                 (temporal/convert-bounds [{:years [2022]}])
                 new-years-date-time)))
      (is (nil? (temporal/next-bounded-time
                 (temporal/convert-bounds [{:years  [2023]
                                            :months [5]}])
                 year-midpoint-date-time)))
      (is (nil? (temporal/next-bounded-time
                 (temporal/convert-bounds [{:years       [2023]
                                            :months      [6]
                                            :daysOfMonth [15]
                                            :hours       [12]
                                            :minutes     [30]
                                            :seconds     [29]}])
                 year-midpoint-date-time))))
    (testing "(daysOfWeek filters out otherwise valid dates)"
      (is (nil? (temporal/next-bounded-time
                 (temporal/convert-bounds [{:years       [2024]
                                            :months      [2]
                                            :daysOfMonth [2]
                                            :daysOfWeek  ["Thursday"]}])
                 new-years-date-time))))))

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
           (temporal/convert-periods
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
           (temporal/convert-periods
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
           (temporal/convert-periods
            [{:min  1
              :mean 1}
             {:min   1
              :mean  1
              :fixed 1}]))))
  (testing "`add-periods` function"
    (testing "(fixed times)"
      (are [expected periods]
           (= (t/local-date-time expected)
              (temporal/add-periods
               new-years-date-time
               (random/seed-rng 100) ; rng doesn't matter here
               (temporal/convert-periods periods)))
        "2023-01-01T00:01:00" [{:fixed 1}]
        "2023-01-01T00:30:00" [{:fixed 1
                                :bounds [{:years [2024]}]}
                               {:fixed 30
                                :bounds [{:years [2023]}]}]))
    (testing "(minimum times)"
      (are [expected-min periods]
           (t/before?
            (t/local-date-time expected-min)
            (temporal/add-periods
             new-years-date-time
             (random/rng)
             (temporal/convert-periods periods)))
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
            (temporal/add-periods
             new-years-date-time
             (random/rng)
             (temporal/convert-periods periods))
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
