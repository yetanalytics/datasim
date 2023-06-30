(ns com.yetanalytics.datasim.timeseries-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.test.alpha :as stest]
            [same.core :refer [ish?]]
            [com.yetanalytics.datasim.math.random :as r]
            [com.yetanalytics.datasim.timeseries  :as ts]))

(deftest timeseries-generative-test
  (testing "generative testing"
    (set! *print-length* 10)         ; unfortunately with-redefs does not always work
    (with-redefs [*print-length* 10] ; prevent printing entire infinite seqs
      (let [results
            (stest/check `#{ts/arma-seq ts/time-seqs})
            {:keys [total check-passed]}
            (stest/summarize-results results)]
        (is (= total check-passed))))
    (set! *print-length* nil)))

(deftest arma-sequence-test
  (testing "prelimary test: get what epsilon values would be with our rng"
    (let [rng (r/seed-rng 100)]
      (is (= [0.6246292191371761
              -0.8581918080882499
              0.6762208162903859
              1.126393826325953
              1.4376621993807677
              1.4920787730184157
              -1.5985374620272976
              -0.07756122563035872]
             ;; epsilon is standard normal for simplicity
             (repeatedly 8 #(r/rand-gauss rng 0 1))))))
  (testing "arma-seq function with phi = 0.5, theta = 0.2, std. normal epsilon"
    (is (= [;; 0.6246292191371761
            (+ 0.6246292191371761
               (* 0.5 0)
               (* 0.2 0))
            ;; -0.42095135469222666
            (+ -0.8581918080882499
               (* 0.5 0.6246292191371761)
               (* 0.2 0.6246292191371761))
            ;; 0.29410677732662255
            (+ 0.6762208162903859
               (* 0.5 -0.42095135469222666)
               (* 0.2 -0.8581918080882499))
            ;; 1.4086913782473414
            (+ 1.126393826325953
               (* 0.5 0.29410677732662255)
               (* 0.2 0.6762208162903859))
            ;; 2.367286653769629
            (+ 1.4376621993807677
               (* 0.5 1.4086913782473414)
               (* 0.2 1.126393826325953))
            ;; 2.9632545397793835
            (+ 1.4920787730184157
               (* 0.5 2.367286653769629)
               (* 0.2 1.4376621993807677))
            ;; 0.18150556246607724
            (+ -1.5985374620272976
               (* 0.5 2.9632545397793835)
               (* 0.2 1.4920787730184157))
            ;; -0.0023206895233918445
            (+ -0.07756122563035872
               (* 0.5 0.18150556246607724)
               (* 0.2 -1.5985374620272976))]
           (take 8 (ts/arma-seq {:phi   [0.5]
                                 :theta [0.2]
                                 :std   1
                                 :c     0
                                 :seed  100})))))
  (testing "arma-seq with no phi or theta values is just white noise"
    (let [seed 123
          rng  (r/seed-rng seed)]
      (is (= (take 1000 (repeatedly #(r/rand-gauss rng 0 1)))
             (take 1000 (ts/arma-seq {:phi   []
                                      :theta []
                                      :std   1
                                      :c     0
                                      :seed  seed}))))))
  (testing "AR(1) is stationary iff `|phi_1| < 1`"
    (let [ar-seq (ts/arma-seq {:phi   [0.99]
                               :theta []
                               :std   0.1
                               :c     0
                               :seed  100})]
      (is (< -10 (nth ar-seq 1000) 10))
      (is (< -10 (nth ar-seq 10000) 10))
      (is (< -10 (nth ar-seq 100000) 10)))
    (let [ar-seq (ts/arma-seq {:phi   [1.01]
                               :theta []
                               :std   0.1
                               :c     0
                               :seed  100})]
      (is (< 1000 (nth ar-seq 1000)))))
  (testing "AR(2) stationary iff `|roots(1 - phi_1 x + phi_2 x^2)| > 1`"
    (let [ar-seq (ts/arma-seq {:phi   [0.5 0.49]
                               :theta []
                               :std   0.1
                               :c     0
                               :seed  100})]
      (is (< -10 (nth ar-seq 1000) 10))
      (is (< -10 (nth ar-seq 10000) 10))
      (is (< -10 (nth ar-seq 100000) 10)))
    (let [ar-seq (ts/arma-seq {:phi   [0.5 0.51]
                               :theta []
                               :std   0.1
                               :c     0
                               :seed  100})]
      (is (< 1000 (nth ar-seq 10000)))))
  (testing "MA is always stationary"
    (let [ma-seq (ts/arma-seq {:phi   []
                               :theta [1.0]
                               :std   1
                               :c     0
                               :seed  100})]
      (is (< -10 (nth ma-seq 1000) 10))
      (is (< -10 (nth ma-seq 10000) 10))
      (is (< -10 (nth ma-seq 100000) 10)))
    (let [ma-seq (ts/arma-seq {:phi   []
                               :theta [100.0]
                               :std   1
                               :c     0
                               :seed  100})]
      (is (< -100 (nth ma-seq 1000) 100))
      (is (< -100 (nth ma-seq 10000) 100))
      (is (< -100 (nth ma-seq 100000) 100)))
    (let [ma-seq (ts/arma-seq {:phi   []
                               :theta [1.0 1.5 2.0 2.5]
                               :std   1
                               :c     0
                               :seed  100})]
      (is (< -10 (nth ma-seq 1000) 10))
      (is (< -10 (nth ma-seq 10000) 10))
      (is (< -10 (nth ma-seq 100000) 10)))))

(deftest time-sequence-test
  (testing "time-seqs function:"
    (testing "milliseconds-seq"
      (is (= '(0 1 2 3 4 5 6 7 8 9)
             (->> (ts/time-seqs) :milliseconds-seq (take 10))))
      (is (= '(0 1 2 3 4 5 6 7 8 9)
             (->> (ts/time-seqs :sample-ms 10) :milliseconds-seq)))
      (is (= '(1 2 3 4 5 6 7 8 9 10)
             (->> (ts/time-seqs :t-zero 1) :milliseconds-seq (take 10))))
      (is (= '(1 2 3 4 5 6 7 8 9 10)
             (->> (ts/time-seqs :t-zero 1 :sample-ms 10) :milliseconds-seq))))
    (testing "second-ms-seq"
      (is (= '(0 1000 2000 3000 4000 5000 6000 7000 8000 9000)
             (->> (ts/time-seqs) :second-ms-seq (take 10))))
      (is (= '(1 1001 2001 3001 4001 5001 6001 7001 8001 9001)
             (->> (ts/time-seqs :t-zero 1) :second-ms-seq (take 10))))
      (is (= '()  ; 00:00:00.999
             (->> (ts/time-seqs :sample-ms 999) :second-ms-seq)))
      (is (= '(0) ; 00:00:01.000
             (->> (ts/time-seqs :sample-ms 1000) :second-ms-seq)))
      (is (= '(0)      ; 00:00:01:999
             (->> (ts/time-seqs :sample-ms 1999) :second-ms-seq)))
      (is (= '(0 1000) ; 00:00:02.000
             (->> (ts/time-seqs :sample-ms 2000) :second-ms-seq)))
      (is (= '(1 1001) ; 00.00.02.000
             (->> (ts/time-seqs :sample-ms 2000 :t-zero 1) :second-ms-seq))))
    (testing "minute-ms-seq"
      (is (= '(0 60000 120000 180000 240000)
             (->> (ts/time-seqs) :minute-ms-seq (take 5))))
      (is (= '(1 60001 120001 180001 240001)
             (->> (ts/time-seqs :t-zero 1) :minute-ms-seq (take 5))))
      (is (= '()  ; 00:00:59.999
             (->> (ts/time-seqs :sample-ms 59999) :minute-ms-seq)))
      (is (= '(0) ; 00:01:00.999
             (->> (ts/time-seqs :sample-ms 60000) :minute-ms-seq))))
    (testing "hour-ms-seq"
      (is (= '(0 3600000 7200000 10800000 14400000)
             (->> (ts/time-seqs) :hour-ms-seq (take 5))))
      (is (= '(1 3600001 7200001 10800001 14400001)
             (->> (ts/time-seqs :t-zero 1) :hour-ms-seq (take 5))))
      (is (= '()  ; 00:59:99.999
             (->> (ts/time-seqs :sample-ms 359999) :hour-ms-seq)))
      (is (= '(0) ; 01:00:00.000
             (->> (ts/time-seqs :sample-ms 3600000) :hour-ms-seq))))
    (testing "day-ms-seq"
      (is (= '(0 86400000 172800000 259200000 345600000)
             (->> (ts/time-seqs) :day-ms-seq (take 5))))
      (is (= '(1 86400001 172800001 259200001 345600001)
             (->> (ts/time-seqs :t-zero 1) :day-ms-seq (take 5))))
      (is (= '()  ; 1970-01-01T23:59:59.999
             (->> (ts/time-seqs :sample-ms 86399999) :day-ms-seq)))
      (is (= '(0) ; 1970-01-02T00:00:00.000
             (->> (ts/time-seqs :sample-ms 86400000) :day-ms-seq))))
    (testing "week-ms-seq"
      (is (= '(0 604800000 1209600000 1814400000 2419200000)
             (->> (ts/time-seqs) :week-ms-seq (take 5))))
      (is (= '(1 604800001 1209600001 1814400001 2419200001)
             (->> (ts/time-seqs :t-zero 1) :week-ms-seq (take 5))))
      (is (= '()  ; 1970-01-06T23:59:59.999
             (->> (ts/time-seqs :sample-ms 604799999) :week-ms-seq)))
      (is (= '(0) ; 1970-01-07T00:00:00.000
             (->> (ts/time-seqs :sample-ms 604800000) :week-ms-seq))))
    (testing "minute-of-hour-seq"
      (is (= '(0 1 2 3 4 5 6 7 8 9)
             (->> (ts/time-seqs) :minute-of-hour-seq (take 10))))
      (is (= 59
             (-> (ts/time-seqs) :minute-of-hour-seq (nth 59))))
      (is (= 0
             (-> (ts/time-seqs) :minute-of-hour-seq (nth 60)))))
    (testing "minute-of-day-seq"
      (is (= '(0 1 2 3 4 5 6 7 8 9)
             (->> (ts/time-seqs) :minute-of-day-seq (take 10))))
      (is (= 1339
             (-> (ts/time-seqs) :minute-of-day-seq (nth 1339))))
      (is (= 0
             (-> (ts/time-seqs) :minute-of-day-seq (nth 1440)))))
    (testing "hour-of-day-seq"
      (is (= '(0 1 2 3 4 5 6 7 8 9)
             (->> (ts/time-seqs) :hour-of-day-seq (take 10))))
      (is (= 23
             (-> (ts/time-seqs) :hour-of-day-seq (nth 23))))
      (is (= 0
             (-> (ts/time-seqs) :hour-of-day-seq (nth 24)))))
    (testing "day-of-week-seq"
      (is (= '(4 5 6 7 1 2 3 4) ; The unix epoch starts on a Thursday
             (->> (ts/time-seqs) :day-of-week-seq (take 8))))
      (is (= '(1 2 3 4 5 6 7 1)
             (->> (ts/time-seqs :t-zero 345600000) :day-of-week-seq (take 8)))))
    (testing "day-of-month-seq"
      (is (= '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
             (->> (ts/time-seqs) :day-of-month-seq (take 19))))
      (is (= '(20 21 22 23 24 25 26 27 28 29 30 31 1)
             (->> (ts/time-seqs) :day-of-month-seq (take 32) (take-last 13))))
      (is (= '(20 21 22 23 24 25 26 27 28 1) ; Jan & Feb have diff day counts
             (->> (ts/time-seqs) :day-of-month-seq (take 60) (take-last 10)))))
    (testing "day-of-year-seq"
      (is (= '(1 2 3 4 5 6 7 8 9 10)
             (->> (ts/time-seqs) :day-of-year-seq (take 10))))
      (is (= '(365 1) ; 1970 was not a leap year
             (->> (ts/time-seqs) :day-of-year-seq (take 366) (take-last 2))))
      (is (= '(365 366 1) ; 1972 was a leap year
             (->> (ts/time-seqs) :day-of-year-seq (take 1097) (take-last 3)))))
    ;; Use `same.core/ish?` instead of `=` for floating point comparisons
    (testing "minute-day-night-seq"
      (is (ish? 1.0
                (-> (ts/time-seqs) :minute-day-night-seq (nth 0))))
      (is (ish? 0.7071067811865476  ; sqrt(0.5)
                (-> (ts/time-seqs) :minute-day-night-seq (nth 180))))
      (is (ish? 0.0
                (-> (ts/time-seqs) :minute-day-night-seq (nth 360))))
      (is (ish? -0.7071067811865476 ; -sqrt(0.5)
                (-> (ts/time-seqs) :minute-day-night-seq (nth 540))))
      (is (ish? -1.0
                (-> (ts/time-seqs) :minute-day-night-seq (nth 720))))
      (is (ish? -0.7071067811865476
                (-> (ts/time-seqs) :minute-day-night-seq (nth 900))))
      (is (ish? 0.0
                (-> (ts/time-seqs) :minute-day-night-seq (nth 1080))))
      (is (ish? 0.7071067811865476
                (-> (ts/time-seqs) :minute-day-night-seq (nth 1260))))
      (is (ish? 1.0
                (-> (ts/time-seqs) :minute-day-night-seq (nth 1440)))))
    (testing "hour-day-night-seq"
      (is (ish? 1.0
                (-> (ts/time-seqs) :hour-day-night-seq (nth 0))))
      (is (ish? 0.7071067811865476
                (-> (ts/time-seqs) :hour-day-night-seq (nth 3))))
      (is (ish? 0.0
                (-> (ts/time-seqs) :hour-day-night-seq (nth 6))))
      (is (ish? -0.7071067811865476
                (-> (ts/time-seqs) :hour-day-night-seq (nth 9))))
      (is (ish? -1.0
                (-> (ts/time-seqs) :hour-day-night-seq (nth 12))))
      (is (ish? -0.7071067811865476
                (-> (ts/time-seqs) :hour-day-night-seq (nth 15))))
      (is (ish? 0.0
                (-> (ts/time-seqs) :hour-day-night-seq (nth 18))))
      (is (ish? 0.7071067811865476
                (-> (ts/time-seqs) :hour-day-night-seq (nth 21))))
      (is (ish? 1.0
                (-> (ts/time-seqs) :hour-day-night-seq (nth 24)))))))
