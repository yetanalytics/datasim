(ns com.yetanalytics.datasim.model.temporal
  (:require [clojure.spec.alpha     :as s]
            [clojure.spec.gen.alpha :as sgen]
            [java-time.api          :as t]
            [com.yetanalytics.datasim.math.random            :as random]
            [com.yetanalytics.datasim.input.model.alignments :as align])
  (:import [java.time LocalDateTime]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- sorted-entries?
  [coll]
  (first
   (reduce (fn [[_ prev] curr]
             (if (< prev curr)
               [true curr]
               (reduced [false curr])))
           [true (first coll)]
           (rest coll))))

(s/def ::year pos-int?)

(s/def ::month (s/int-in 1 13))

(s/def ::day (s/int-in 1 32))

(s/def ::day-of-month ::day)

(s/def ::day-of-week (s/int-in 0 6))

(s/def ::hour (s/int-in 0 24))

(s/def ::minute (s/int-in 0 60))

(s/def ::second (s/int-in 0 60))

(s/def ::years
  (s/coll-of ::year :distinct true :min-count 1))

(s/def ::months
  (s/coll-of ::month :distinct true :min-count 1))

(s/def ::days
  (s/coll-of ::day :distinct true :min-count 1))

(s/def ::days-of-month
  (s/coll-of ::day-of-month :distinct true :min-count 1))

(s/def ::days-of-week
  (s/coll-of ::hour :distinct true :min-count 1))

(s/def ::hours
  (s/coll-of ::hour :distinct true :min-count 1))

(s/def ::minutes
  (s/coll-of ::minute :distinct true :min-count 1))

(s/def ::seconds
  (s/coll-of ::second :distinct true :min-count 1))

(def ^:private ranges-spec
  (s/keys :opt-un [::years
                   ::months
                   ::days
                   ::hours
                   ::minutes
                   ::seconds]))

(s/def ::ranges
  (s/with-gen (s/and ranges-spec
                     (s/map-of keyword? (s/and seq? sorted-entries?)))
    (fn [] (sgen/fmap #(update-vals % sort)
                      (s/gen ranges-spec)))))

(def ^:private sets-spec
  (s/keys :opt-un [::years
                   ::months
                   ::days-of-month
                   ::days-of-week
                   ::hours
                   ::minutes
                   ::seconds]))

(s/def ::sets
  (s/with-gen (s/and sets-spec
                     (s/map-of keyword? set?))
    (fn [] (sgen/fmap #(update-vals % set)
                      (s/gen sets-spec)))))

(s/def ::bounds
  (s/keys :req-un [::ranges ::sets]))

(s/def ::min nat-int?)

(s/def ::mean pos-int?)

(s/def ::period
  (s/keys :req-un [::min ::mean]))

(s/def ::periods
  (s/every ::period))

(s/def ::date-time
  #(instance? LocalDateTime %))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef time-map
  :args (s/cat :date-time ::date-time)
  :ret (s/keys :req-un [::year
                        ::month
                        ::day-of-month
                        ::day-of-week
                        ::hour
                        ::minute
                        ::second]))

(defn time-map
  "Return a map of different times from the LocalDateTime `date-time`."
  [date-time]
  (let [[year month day-month day-week hour minute second]
        (t/as date-time
              :year
              :month-of-year
              :day-of-month
              :day-of-week
              :hour-of-day
              :minute-of-hour
              :second-of-minute)]
    {:year         year
     :month        month
     :day-of-month day-month
     :day-of-week  day-week
     :hour         hour
     :minute       minute
     :second       second}))

(s/fdef leap-year?
  :args (s/cat :year ::year)
  :ret boolean?)

(defn leap-year?
  "Is `year` a leap year?"
  [year]
  (and (zero? ^long (mod year 4))
       (not (and (zero? ^long (mod year 100))
                 (not (zero? ^long (mod year 400)))))))

(s/fdef day-of-month?
  :args (s/cat :year  ::year
               :month ::month
               :day   ::day)
  :ret boolean?)

(defn day-of-month?
  "Is `day` a valid day within each month? i.e. `28` is valid for `2`
   (i.e. February) but not `30`. `29` is valid for `2` if `year` is a
   leap year, but not otherwise."
  [year month ^long day]
  (cond
    (<= day 28) true
    (= 29 day)  (or (not (= 2 month))
                    (leap-year? year))
    (= 30 day)  (not (= 2 month))
    (= 31 day)  (not (#{2 4 6 9 11} month))
    :else       false))

;; Algorithm used is Gauss's algorithm for day-of-week computation for
;; the Gregorian calendar:
;; https://en.wikipedia.org/wiki/Determination_of_the_day_of_the_week#Gauss's_algorithm

(def ^:private month-offset
  [0 3 3 6 1 4 6 2 5 0 3 5])

(def ^:private month-offset-leap
  [0 3 4 0 2 5 0 3 6 1 4 6])

(defn- january-day-of-week
  [year*]
  (mod (+ 1
          (* 5 ^long (mod year* 4))
          (* 4 ^long (mod year* 100))
          (* 6 ^long (mod year* 400)))
       7))

(s/fdef day-of-week
  :args (s/cat :year  ::year
               :month ::month
               :day   ::day)
  :ret ::day-of-week)

(defn day-of-week
  "Return the day of week, from `0` (Sunday) to `6` (Saturday),
   of the date `year`/`month`/`day`."
  [year month day]
  (let [year*   (dec ^long year)
        month*  (dec ^long month)
        day*    (dec ^long day)
        jan-dow (january-day-of-week year*)
        mon-off (if (leap-year? year)
                  (get month-offset-leap month*)
                  (get month-offset month*))]
    (mod (+ ^long jan-dow
            ^long mon-off
            ^long day*)
         7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Temporal Bounds

(def string->int-map
  (merge align/day-of-week-map align/month-of-year-map))

(defn- string->int
  [s]
  (get string->int-map s s))

(defn- csk-unit
  [unit]
  (case unit
    :daysOfWeek  :days-of-week
    :daysOfMonth :day-of-month
    unit))

(defn- reduce-values
  [vs* v]
  (if (coll? v)
    (let [[start end] v
          start* (string->int start)
          end*   (string->int end)
          vrange (range start* (inc end*))]
      (into vs* vrange))
    (let [v* (string->int v)]
      (conj vs* v*))))

(defn- convert-values [vs]
  (reduce reduce-values [] vs))

(defn- convert-unit
  [[unit vs]]
  [(csk-unit unit) (convert-values vs)])

(defn- convert-bound
  [bound]
  (reduce (fn [bound* [unit vs]]
            (-> bound*
                (assoc-in [:sets unit] (set vs))
                (assoc-in [:ranges unit] (sort (distinct vs)))))
          {:sets {} :ranges {}}
          (map convert-unit bound)))

(s/fdef convert-bounds
  :args (s/cat :bounds ::align/bounds)
  :ret ::bounds)

(defn convert-bounds
  [bounds]
  (mapv convert-bound bounds))

;; Temporal Periods

(def ms-per-second
  1000)

(def ms-per-minute
  60000)

(def ms-per-hour
  3600000)

(def ms-per-day
  86400000)

(def ms-per-week
  604800000)

(defn- convert-time
  "Convert time `t` into milliseconds based on the time `unit`. Coerces
   any doubles into integers."
  [t unit]
  (long (case unit
          :millis  t
          :seconds (* t ms-per-second)
          :minutes (* t ms-per-minute)
          :hours   (* t ms-per-hour)
          :days    (* t ms-per-day)
          :weeks   (* t ms-per-week))))

(defn- convert-period
  [{:keys [min mean unit bounds]}]
  (let [unit*   (or (some-> unit keyword) :minute)
        mean*   (or (some-> mean (convert-time unit*)) ms-per-minute)
        min*    (or (some-> min (convert-time unit*)) 0)
        bounds* (or (some-> bounds convert-bounds) [])]
    {:min    min*
     :mean   mean*
     :bounds bounds*}))

(s/fdef convert-periods
  :args (s/cat :periods ::align/periods)
  :ret ::periods)

(defn convert-periods
  [periods]
  (mapv convert-period periods))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bounded Time?

(defn- in-bound-unit?
  [unit-set unit]
  (or (nil? unit-set)
      (contains? unit-set unit)))

(defn- in-bound?
  [{{:keys [years months days-of-month days-of-week hours minutes]} :sets}
   date-time]
  (let [{:keys [year month day-of-month day-of-week hour minute]}
        (time-map date-time)]
    (and (in-bound-unit? years year)
         (in-bound-unit? months month)
         (in-bound-unit? days-of-month day-of-month)
         (in-bound-unit? days-of-week day-of-week)
         (in-bound-unit? hours hour)
         (in-bound-unit? minutes minute))))

(s/fdef bounded-time?
  :args (s/cat :bounds    ::bounds
               :date-time ::date-time)
  :ret boolean?)

(defn bounded-time?
  "Is `date-time` within any of the `bounds`? Returns vacuously `true` if
   `bounds` is empty."
  [bounds date-time]
  (or (empty? bounds)
      (boolean (some (fn [bound] (in-bound? bound date-time)) bounds))))

;; Next Bounded Time

(defn- current-t?
  [inst-time t current-interval?]
  (and current-interval?
       (= inst-time t)))

(defn- future-t?
  [inst-time t future-interval? current-interval?]
  (or future-interval?
      (and current-interval?
           (< inst-time t))))

(defn- next-bounded-times
  "Returns a sequence of the next LocalDateTime that are within `bounds`."
  [{{:keys [years months days-of-month hours minutes seconds]
     :or {months        (range 1 13)
          days-of-month (range 1 32)
          hours         (range 0 24)
          minutes       (range 0 60)
          seconds       (range 0 60)}}
    :ranges
    {:keys [days-of-week]}
    :sets}
   date-time]
  (let [{inst-yr  :year
         inst-mon :month
         inst-day :day-of-month
         inst-hr  :hour
         inst-min :minute
         inst-sec :second}
        (time-map date-time)
        day-of-week?
        (if (empty? days-of-week)
          (constantly true)
          (fn [y m d] (contains? days-of-week (day-of-week y m d))))]
    (for [;; Years
          year  years
          :let  [current-yr? (= inst-yr year)
                 future-yr?  (< inst-yr year)]
          :when (or current-yr? future-yr?)
          ;; Months
          month months
          :let  [current-mo? (current-t? inst-mon month current-yr?)
                 future-mo?  (future-t? inst-mon month future-yr? current-yr?)]
          :when (or current-mo? future-mo?)
          ;; Days of Month
          day   days-of-month
          :let  [current-dy?   (current-t? inst-day day current-mo?)
                 future-dy?    (future-t? inst-day day future-mo? current-mo?)
                 day-of-month? (day-of-month? year month day)
                 day-of-week?  (day-of-week? year month day)]
          :when (and (or current-dy? future-dy?)
                     day-of-month?
                     day-of-week?)
          ;; Hours
          hour  hours
          :let  [current-hr? (current-t? inst-hr hour current-dy?)
                 future-hr?  (future-t? inst-hr hour future-dy? current-dy?)]
          :when (or current-hr? future-hr?)
          ;; Minutes
          min   minutes
          :let  [current-min? (current-t? inst-min min current-hr?)
                 future-min?  (future-t? inst-min min future-hr? current-hr?)]
          :when (or current-min? future-min?)
          ;; Seconds
          sec   seconds
          :let  [current-sec? (current-t? inst-sec sec current-min?)
                 future-sec?  (future-t? inst-sec sec future-min? current-min?)]
          :when (or current-sec? future-sec?)]
      (t/local-date-time year month day hour min sec))))

(s/fdef next-bounded-time
  :args (s/cat :bound     ::bounds
               :date-time ::date-time)
  :ret ::date-time)

(defn next-bounded-time
  [bounds date-time]
  (first (next-bounded-times bounds date-time)))

;; Next Time

(def min-ms 60000.0) ; The amount of milliseconds in one minute

(defn- generate-period
  [rng {:keys [mean min]
        :or {mean min-ms
             min  0}}]
  (let [rate   (/ 1.0 mean)
        t-diff (long (random/rand-exp rng rate))]
    (+ min t-diff)))

(defn- add-period
  [date-time rng period]
  (t/plus date-time (t/millis (generate-period rng period))))

(s/fdef add-periods
  :args (s/cat :date-time ::date-time
               :rng       ::rng
               :periods   ::periods)
  :ret ::date-time)

(defn add-periods
  "Add a random amount of milliseonds to `date-time` based on the first map of
   valid parameter map in `periods`. A valid map either has no time bounds or
   bounds that satisfy `date-time`. The millis amount is an exponentially
   distributed random variable with `period` parameters `:mean` and `:min`.
   The generated sequence that uses these periodic date-times will thus occur
   as a Poisson random process."
  [date-time rng periods]
  (let [some-bound (fn [{:keys [bounds] :as period}]
                     (when (bounded-time? bounds date-time) period))]
    (->> periods
         (some some-bound)
         (add-period date-time rng))))

(comment
  (def the-time (t/local-date-time (t/instant) "UTC"))
  (def the-rng (random/seed-rng 100))
  (def converted-periods
    (convert-periods
     [{:mean 1
       :unit "hours"
       :bounds [{:years [2020 #_2023]}]}
      {:mean 1
       :unit "seconds"}]))
  
  (bounded-time? (get-in converted-periods [0 :bounds])
                 the-time)
  
  the-time
  (add-periods the-time
               the-rng
               converted-periods)
  )
