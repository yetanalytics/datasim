(ns com.yetanalytics.datasim-test-temporal
  (:require [clojure.test  :refer [deftest testing is]]
            [java-time.api :as t]
            [com.yetanalytics.datasim :as ds]
            [com.yetanalytics.datasim.test-constants :as const])
  (:import [java.io FileNotFoundException]))

(defmacro test-temporal
  [file-name & preds]
  (let [fname# (str file-name ".json")
        input# (get const/temporal-input-map fname#)]
    (if input#
      `(testing ~fname#
         (let [~'gen-seq (ds/generate-seq ~input#)]
           ~@(mapv (fn [pred] `(is (~pred ~'gen-seq))) preds)))
      (throw (FileNotFoundException. (str "Model not found: " fname#))))))

(defn- not-empty?
  [statements]
  (boolean (not-empty statements)))

(def verb-1 "http://yetanalytics.com/temporal/verb-1")
(def verb-2 "http://yetanalytics.com/temporal/verb-2")
(def verb-3 "http://yetanalytics.com/temporal/verb-3")
(def verb-4 "http://yetanalytics.com/temporal/verb-4")
(def verb-5 "http://yetanalytics.com/temporal/verb-5")
(def verb-6 "http://yetanalytics.com/temporal/verb-6")
(def verb-7 "http://yetanalytics.com/temporal/verb-7")
(def verb-8 "http://yetanalytics.com/temporal/verb-8")

(defn- get-verb
  ([statement]
   (get-in statement ["verb" "id"]))
  ([statement default-verb]
   (get-in statement ["verb" "id"] default-verb)))

(defn- cyclic-verbs?
  "Do verbs go in order from verb 1 to verb 8 for each registration?
   This returning false is a sign of early termination."
  [statements]
  (= (->> (cycle [verb-1 verb-2 verb-3 verb-4 verb-5 verb-6 verb-7 verb-8])
          (take (count statements)))
     (map get-verb statements)))

(defn- repeating-verbs?
  "Similar to `cyclic-verbs?` but returns `true` even upon early termination."
  [statements]
  (->> (map (fn [stmt-1 stmt-2]
              (let [v1 (get-verb stmt-1)
                    v2 (get-verb stmt-2)]
                (or (and (= verb-2 v2)
                         (= verb-1 v1))
                    (and (= verb-3 v2)
                         (= verb-2 v1))
                    (and (= verb-4 v2)
                         (= verb-3 v1))
                    (and (= verb-5 v2)
                         (= verb-4 v1))
                    (and (= verb-6 v2)
                         (= verb-5 v1))
                    (and (= verb-7 v2)
                         (= verb-6 v1))
                    (and (= verb-8 v2)
                         (= verb-7 v1))
                    (= verb-1 v2))))
            statements
            (rest statements))
       (every? true?)))

(defn- group-statements
  "Group `statements` by `as-keyword` (e.g. `:hour-of-day`) into
   a vector with `num-slots`. The time units start at 0 if `zero-indexed?`
   is `true`, or 1 if `false`."
  [statements as-keyword & {:keys [num-slots zero-indexed?]
                            :or {num-slots     8
                                 zero-indexed? true}}]
  (->> statements
       (group-by
        (fn [statement]
          (-> statement meta :timestamp (t/as as-keyword))))
       (reduce-kv
        (fn [acc k statements]
          (if zero-indexed?
            (assoc acc k (count statements))
            (assoc acc (dec k) (count statements))))
        (vec (repeat num-slots 0)))))

(defn- group-hour-statements
  "Group `statements` into a vector with each index being the hour of day.
   Unlike `group-statemnets`, this preserves the order in which statements
   occur."
  [statements]
  (reduce
   (fn [acc statement]
     (let [last-idx  (dec (count acc))
           prev-stmt (-> acc last last)
           curr-hour (some-> statement meta :timestamp (t/as :hour-of-day))
           prev-hour (or (some-> prev-stmt meta :timestamp (t/as :hour-of-day))
                         -1)]
       (cond
         (= curr-hour prev-hour)
         (update acc last-idx conj statement)
         (= curr-hour (inc prev-hour))
         (conj acc [statement])
         (= curr-hour (+ 2 prev-hour))
         (conj acc [] [statement]))))
   []
   statements))

(deftest generate-temporal-seq-test
  (testing "Temporal properties test:"
    ;; Year bounds
    (test-temporal
     "1a_year_2023"
     not-empty?
     cyclic-verbs?
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      year (t/as timestamp :year)]
                  (#{2023} year)))))
    (test-temporal
     "1b_year_2023-2024"
     not-empty?
     cyclic-verbs?
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      year (t/as timestamp :year)]
                  (#{2023 2024} year)))))
    (test-temporal
     "1c_year_0-2023"
     not-empty?
     cyclic-verbs?
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      year (t/as timestamp :year)]
                  (#{2023} year))))
     (comp boolean not-empty))
    (test-temporal
     "1d_year_0-2022"
     empty?)
    ;; Day of week + hour bounds
    (test-temporal
     "2a_dow_Tues-Thurs_hour_8-12"
     not-empty?
     cyclic-verbs?
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      dow (t/as timestamp :day-of-week)
                      hod (t/as timestamp :hour-of-day)]
                  (and (#{2 4} dow)
                       (#{8 9 10 11 12} hod))))))
    (test-temporal
     "2b_dow_Tues-Thurs_hour_8-12-18"
     not-empty?
     cyclic-verbs?
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      dow (t/as timestamp :day-of-week)
                      hod (t/as timestamp :hour-of-day)]
                  (or (and (#{2} dow)
                           (#{8 9 10 11 12} hod))
                      (and (#{4} dow)
                           (#{13 14 15 16 17 18} hod)))))))
    ;; Last day of month bounds
    (test-temporal
     "3a_dom_28"
     not-empty?
     cyclic-verbs?
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      dom (t/as timestamp :day-of-month)]
                  (= 28 dom)))))
    (test-temporal
     "3b_dom_29"
     not-empty?
     cyclic-verbs?
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      yr  (t/as timestamp :year)
                      moy (t/as timestamp :month-of-year)
                      dom (t/as timestamp :day-of-month)]
                  (and (= 29 dom)
                       (or (not= 2 moy)
                           (= 2024 yr)))))))
    (test-temporal
     "3c_dom_30"
     not-empty?
     cyclic-verbs?
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      moy (t/as timestamp :month-of-year)
                      dom (t/as timestamp :day-of-month)]
                  (and (= 30 dom)
                       (not= 2 moy))))))
    (test-temporal
     "3d_dom_31"
     not-empty?
     cyclic-verbs?
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      moy (t/as timestamp :month-of-year)
                      dom (t/as timestamp :day-of-month)]
                  (and (= 31 dom)
                       (#{1 3 5 7 8 10 12} moy))))))
    ;; Day of month tests
    (test-temporal
     "4a_Dec_25"
     not-empty?
     cyclic-verbs?
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      moy (t/as timestamp :month-of-year)
                      dom (t/as timestamp :day-of-month)]
                  (and (= 12 moy)
                       (= 25 dom))))))
    (test-temporal
     "4b_Dec_25_Mon"
     not-empty?
     cyclic-verbs?
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      yr  (t/as timestamp :year)
                      moy (t/as timestamp :month-of-year)
                      dom (t/as timestamp :day-of-month)
                      dow (t/as timestamp :day-of-week)]
                  (and (= 12 moy)
                       (= 25 dom)
                       (= 2023 yr) ; is Monday only in 2023
                       (= 1 dow))))))
    ;; Second bounds + higher bounds with steps
    ;; Sometimes early termination occurs due to large periods (1 min)
    ;; relative to bounds (10-30 seconds)
    (test-temporal
     "5a_Jan_1_sec_10-30"
     not-empty?
     (comp not cyclic-verbs?)
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      moy (t/as timestamp :month-of-year)
                      dom (t/as timestamp :day-of-month)
                      sec (t/as timestamp :second-of-minute)]
                  (and (= 1 moy)
                       (= 1 dom)
                       (<= 10 sec 30))))))
    (test-temporal
     "5b_Jan_1_min_even_sec_10-30"
     not-empty?
     (comp not cyclic-verbs?)
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      moy (t/as timestamp :month-of-year)
                      dom (t/as timestamp :day-of-month)
                      sec (t/as timestamp :second-of-minute)
                      min (t/as timestamp :minute-of-hour)]
                  (and (= 1 moy)
                       (= 1 dom)
                       (<= 10 sec 30)
                       (zero? (mod min 2)))))))
    (test-temporal
     "5c_Jan_1_hour_even_sec_10-30"
     not-empty?
     (comp not cyclic-verbs?)
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      moy (t/as timestamp :month-of-year)
                      dom (t/as timestamp :day-of-month)
                      sec (t/as timestamp :second-of-minute)
                      hr  (t/as timestamp :hour-of-day)]
                  (and (= 1 moy)
                       (= 1 dom)
                       (<= 10 sec 30)
                       (zero? (mod hr 2)))))))
    (test-temporal
     "5d_Jan_1_min_hour_even_sec_10-30"
     not-empty?
     (comp not cyclic-verbs?)
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      moy (t/as timestamp :month-of-year)
                      dom (t/as timestamp :day-of-month)
                      sec (t/as timestamp :second-of-minute)
                      hr  (t/as timestamp :hour-of-day)
                      min (t/as timestamp :minute-of-hour)]
                  (and (= 1 moy)
                       (= 1 dom)
                       (<= 10 sec 30)
                       (zero? (mod hr 2))
                       (zero? (mod min 2)))))))
    ;; Bounds w/ hour periods
    ;; Should cause early termination due to running into maxRestarts param
    (test-temporal
     "6a_hours_period_every_second_hour"
     not-empty?
     (comp not cyclic-verbs?)
     repeating-verbs?
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      hour (t/as timestamp :hour-of-day)]
                  (zero? (mod hour 2))))))
    (test-temporal
     "6b_hours_period_every_start_hour"
     not-empty?
     (comp not cyclic-verbs?)
     repeating-verbs?
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      hour (t/as timestamp :hour-of-day)]
                  (zero? (mod hour 24))))))
    (test-temporal
     "6c_hours_fixed_period_every_second_hour"
     empty?)
    (test-temporal
     "6d_hours_fixed_period_every_start_hour"
     empty?)
    ;; Periods
    ;; Since statement gen is a Poisson process, we compute
    ;; mean # of occurences = total time / mean period, with 
    ;; +/- 3 * standard deviation = 3 * sqrt(mean # of occurences)
    (test-temporal
     "7a_millis_period"
     cyclic-verbs?
     (fn [statements]
       (let [counts (group-statements statements
                                      :second-of-minute)]
         (and
          (<= 70 (get counts 0) 130) ; 1000 / 10 = 100
          (<= 29 (get counts 1) 71)  ; 1000 / 20 = 50
          (<= 29 (get counts 2) 71)
          (<= 16 (get counts 3) 62)  ; 1000 / 30 = 33.3...
          (<= 16 (get counts 4) 62)
          (<= 10 (get counts 5) 40)  ; 1000 / 40 = 25
          (<= 10 (get counts 6) 30)
          (<= 07 (get counts 7) 23)  ; 1000 / 50 = 20
          ))))
    (test-temporal
     "7b_seconds_period"
     cyclic-verbs?
     (fn [statements]
       (let [counts (group-statements statements
                                      :minute-of-hour)]
         (and
          (<= 37 (get counts 0) 83) ; 60 / 1 = 60
          (<= 14 (get counts 1) 46) ; 60 / 2 = 30
          (<= 14 (get counts 2) 46)
          (<= 07 (get counts 3) 33) ; 60 / 3 = 20
          (<= 07 (get counts 4) 33)
          (<= 03 (get counts 5) 27) ; 60 / 4 = 15
          (<= 03 (get counts 6) 27)
          (<= 02 (get counts 7) 22) ; 60 / 5 = 12
          ))))
    (test-temporal
     "7c_minutes_period"
     cyclic-verbs?
     (fn [statements]
       (let [counts (group-statements statements
                                      :hour-of-day)]
         (and
          (<= 37 (get counts 0) 83) ; 60 / 1 = 60
          (<= 14 (get counts 1) 46) ; 60 / 2 = 30
          (<= 14 (get counts 2) 46)
          (<= 07 (get counts 3) 33) ; 60 / 3 = 20
          (<= 07 (get counts 4) 33)
          (<= 03 (get counts 5) 27) ; 60 / 4 = 15
          (<= 03 (get counts 6) 27)
          (<= 02 (get counts 7) 22) ; 60 / 5 = 12
          ))))
    (test-temporal
     "7d_hours_period"
     cyclic-verbs?
     (fn [statements]
       (let [counts (group-statements statements
                                      :day-of-month
                                      :zero-indexed? false)]
         (and
          (<= 9 (get counts 0) 39) ; 24 / 1 = 24
          (<= 2 (get counts 1) 22) ; 24 / 2 = 12
          (<= 2 (get counts 2) 22)
          (<= 0 (get counts 3) 16) ; 24 / 3 = 8
          (<= 0 (get counts 4) 16)
          (<= 0 (get counts 5) 15) ; 24 / 4 = 6
          (<= 0 (get counts 6) 15)
          (<= 0 (get counts 7) 12) ; 24 / 5 = 4.8
          ))))
    (test-temporal
     "7e_days_period"
     cyclic-verbs?
     (fn [statements]
       (let [counts (group-statements statements
                                      :month-of-year
                                      :zero-indexed? false)]
         (and
          (<= 14 (get counts 0) 46) ; 30 / 1 = 30
          (<= 3 (get counts 1) 27)  ; 30 / 2 = 15
          (<= 3 (get counts 1) 27)
          (<= 1 (get counts 3) 19)  ; 30 / 3 = 10
          (<= 1 (get counts 4) 19)
          (<= 0 (get counts 5) 16)  ; 30 / 4 = 7.5
          (<= 0 (get counts 6) 16)
          (<= 0 (get counts 7) 15)  ; 30 / 5 = 6
          ))))
    (test-temporal
     "7f_weeks_period"
     cyclic-verbs?
     (fn [statements]
       (let [counts (group-statements statements
                                      :month-of-year
                                      :num-slots 4
                                      :zero-indexed? false)]
         (and
          (<= 0 (get counts 0) 10) ; 4 / 1 = 4
          (<= 0 (get counts 1) 6)  ; 4 / 2 = 2
          (<= 0 (get counts 2) 6)
          (<= 0 (get counts 3) 5)  ; 4 / 3 = 1.33...
          ))))
    ;; Fixed periods
    ;; Note that the counts may not exactly be total / period due to bounds
    (test-temporal
     "8a_millis_fixed_period"
     cyclic-verbs?
     (fn [statements]
       (let [counts (group-statements statements
                                      :second-of-minute)]
         (and
          (= 99 (get counts 0)) ; 1000 / 10 = 100
          (= 50 (get counts 1)) ; 1000 / 20 = 50
          (= 34 (get counts 2)) ; 1000 / 30 = 33.3...
          (= 25 (get counts 3)) ; 1000 / 40 = 25
          (= 20 (get counts 4)) ; 1000 / 50 = 20
          (= 17 (get counts 5)) ; 1000 / 60 = 16.6...
          (= 14 (get counts 6)) ; 1000 / 70 = 14.2857...
          (= 13 (get counts 7)) ; 1000 / 80 = 12.5
          ))))
    (test-temporal
     "8b_seconds_fixed_period"
     cyclic-verbs?
     (fn [statements]
       (let [counts (group-statements statements
                                      :minute-of-hour)]
         (and
          (= 59 (get counts 0)) ; 60 / 1 = 60
          (= 30 (get counts 1)) ; 60 / 2 = 30
          (= 20 (get counts 2)) ; 60 / 3 = 20
          (= 15 (get counts 3)) ; 60 / 4 = 15
          (= 12 (get counts 4)) ; 60 / 5 = 12
          (= 10 (get counts 5)) ; 60 / 6 = 10
          (=  9 (get counts 6)) ; 60 / 7 = 8.571428...
          (=  8 (get counts 7)) ; 60 / 8 = 7.5
          ))))
    (test-temporal
     "8c_minutes_fixed_period"
     cyclic-verbs?
     (fn [statements]
       (let [counts (group-statements statements
                                      :hour-of-day)]
         (and
          (= 59 (get counts 0)) ; 60 / 1 = 60
          (= 30 (get counts 1)) ; 60 / 2 = 30
          (= 20 (get counts 2)) ; 60 / 3 = 20
          (= 15 (get counts 3)) ; 60 / 4 = 15
          (= 12 (get counts 4)) ; 60 / 5 = 12
          (= 10 (get counts 5)) ; 60 / 6 = 10
          (=  9 (get counts 6)) ; 60 / 7 = 8.571428...
          (=  8 (get counts 7)) ; 60 / 8 = 7.5
          ))))
    (test-temporal
     "8d_hours_fixed_period"
     cyclic-verbs?
     (fn [statements]
       (let [counts (group-statements statements
                                      :day-of-month
                                      :zero-indexed? false)]
         (and
          (= 23 (get counts 0)) ; 24 / 1 = 24
          (= 12 (get counts 1)) ; 24 / 2 = 12
          (=  8 (get counts 2)) ; 24 / 3 = 8
          (=  6 (get counts 3)) ; 24 / 4 = 6
          (=  5 (get counts 4)) ; 24 / 5 = 4.8
          (=  4 (get counts 5)) ; 24 / 6 = 4
          (=  4 (get counts 6)) ; 24 / 7 = 3.428571...
          (=  3 (get counts 7)) ; 24 / 8 = 3
          ))))
    (test-temporal
     "8e_days_fixed_period"
     cyclic-verbs?
     (fn [statements]
       (let [counts (group-statements statements
                                      :month-of-year
                                      :zero-indexed? false)]
         (and
          (= 30 (get counts 0)) ; 31 / 1 = 31
          (= 14 (get counts 1)) ; 28 / 2 = 14
          (= 11 (get counts 2)) ; 31 / 3 = 10.33...
          (=  7 (get counts 3)) ; 30 / 4 = 7.5
          (=  7 (get counts 4)) ; 31 / 5 = 6.2
          (=  5 (get counts 5)) ; 30 / 6 = 5
          (=  4 (get counts 6)) ; 31 / 7 = 4.428571...
          (=  4 (get counts 7)) ; 30 / 8 = 3.75
          ))))
    (test-temporal
     "8f_weeks_fixed_period"
     cyclic-verbs?
     (fn [statements]
       (let [counts (group-statements statements
                                      :month-of-year
                                      :num-slots 4
                                      :zero-indexed? false)]
         (and
          (= 4 (get counts 0)) ; 31 / 07 = 4.428571...
          (= 2 (get counts 1)) ; 28 / 14 = 2
          (= 2 (get counts 2)) ; 31 / 21 = 1.476190...
          (= 1 (get counts 3)) ; 30 / 28 = 1.07142857...
          ))))
    ;; Outer + inner bounds
    (test-temporal
     "9a_outer_bound_larger"
     not-empty?
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      hour (t/as timestamp :hour-of-day)]
                  (<= 6 hour 12)))))
    (test-temporal
     "9b_outer_bound_smaller"
     not-empty?
     (partial every?
              (fn [statement]
                (let [{:keys [timestamp]} (meta statement)
                      hour (t/as timestamp :hour-of-day)]
                  (<= 6 hour 12)))))
    ;; Different boundRestart levels
    (test-temporal
     "10a_restart_current_pattern"
     not-empty?
     cyclic-verbs?
     (fn [statements]
       ;; Even hours allow for all verbs, but (other than the first) will
       ;; always start on Pattern B2 because there will be a forced restart
       ;; in the previous hour (regardless if that previous hour is odd or
       ;; even).
       ;; Odd hours only allow for verbs 1 to 4, and will terminate early due
       ;; to the forced repeat on Pattern B2.
       (->> statements
            group-hour-statements
            (map-indexed
             (fn [idx stmts]
               (cond
                 (zero? idx)
                 (= verb-1
                    (-> stmts first get-verb))
                 (even? idx)
                 (= verb-5
                    (-> stmts first get-verb))
                 :else
                 (or (empty? stmts)
                     (and (every? (comp #{verb-1 verb-2 verb-3 verb-4} get-verb)
                                  stmts)
                          (>= 10
                              (-> stmts
                                  last
                                  meta
                                  :timestamp
                                  (t/as :minute-of-hour))))))))
            (every? true?))))
    (test-temporal
     "10b_restart_parent_pattern"
     not-empty?
     cyclic-verbs?
     (fn [statements]
       ;; Even hours start with verb 1 and end with verb 8, since even hour
       ;; generation is triggered by a Pattern A restart.
       ;; The last hour is the exception, since there is no next even
       ;; available for a Pattern A restart, so it simply terminates early.
       ;; Odd hours have no generated statements, since Pattern A is forced
       ;; to restart in the next even hour.
       (->> statements
            group-hour-statements
            butlast
            (map-indexed
             (fn [idx stmts]
               (if (even? idx)
                 (and (= verb-1
                         (-> stmts first get-verb))
                      (= verb-8
                         (-> stmts last get-verb)))
                 (empty? stmts))))
            (every? true?))))
    (test-temporal
     "10c_restart_all_pattern"
     not-empty?
     cyclic-verbs?
     (fn [statements]
       (let [hour-statements (group-hour-statements statements)]
         ;; Should behave the same as test case 9b, since it is always the
         ;; topmost pattern in boundRestarts that is restarted.
         (->> hour-statements
              butlast
              (map-indexed
               (fn [idx stmts]
                 (if (even? idx)
                   (and (= verb-1
                           (-> stmts first get-verb))
                        (= verb-8
                           (-> stmts last get-verb)))
                   (empty? stmts))))
              (every? true?)))))
    (test-temporal
     "10d_restart_child_pattern"
     not-empty?
     cyclic-verbs?
     (fn [statements]
       ;; Even hours can contain any verb.
       ;; Odd hours should only contain verbs 1 to 4, and upon reaching
       ;; verb 5 should force a restart of Pattern C3 in the next even hour.
       (->> statements
            group-hour-statements
            (map-indexed
             (fn [idx stmts]
               (or (even? idx)
                   (empty? stmts)
                   (and
                    (every? (comp #{verb-1 verb-2 verb-3 verb-4} get-verb)
                            stmts)
                    (>= 10
                        (-> stmts
                            last
                            meta
                            :timestamp
                            (t/as :minute-of-hour)))))))
            (every? true?))))
    (test-temporal
     "10e_restart_child_template"
     not-empty?
     cyclic-verbs?
     (fn [statements]
       ;; Should behave the same as test case 9d, since the C-level Patterns
       ;; are one-to-one with the Statement Templates.
       (->> statements
            group-hour-statements
            (map-indexed
             (fn [idx stmts]
               (or (even? idx)
                   (empty? stmts)
                   (and
                    (every? #(#{verb-1 verb-2 verb-3 verb-4}
                              (get-in % ["verb" "id"]))
                            stmts)
                    (>= 10
                        (-> stmts
                            last
                            meta
                            :timestamp
                            (t/as :minute-of-hour)))))))
            (every? true?))))
    (test-temporal
     "11a_end_current_pattern"
     not-empty?
     (comp not cyclic-verbs?)
     repeating-verbs?)
    (test-temporal
     "11b_end_parent_pattern"
     not-empty?
     (comp not cyclic-verbs?)
     repeating-verbs?)
    (test-temporal
     "11c_end_all_pattern"
     not-empty?
     (comp not cyclic-verbs?)
     repeating-verbs?)
    (test-temporal
     "11d_end_child_pattern"
     not-empty?
     (comp not cyclic-verbs?)
     repeating-verbs?)
    (test-temporal
     "11e_end_child_template"
     not-empty?
     (comp not cyclic-verbs?)
     repeating-verbs?)))
