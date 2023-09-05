(ns com.yetanalytics.datasim.model.temporal
  (:require [clojure.spec.alpha :as s]
            [java-time.api      :as t]
            [com.yetanalytics.datasim.math.random :as random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day-of-week-map
  {"Sunday"    0
   "Monday"    1
   "Tuesday"   2
   "Wednesday" 3
   "Thursday"  4
   "Friday"    5
   "Saturday"  6})

(def month-of-year-map
  {"January"   1
   "February"  2
   "March"     3
   "April"     4
   "May"       5
   "June"      6
   "July"      7
   "August"    8
   "September" 9
   "October"   10
   "November"  11
   "December"  12})

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

(s/def ::day-of-week (s/int-in 0 6))

(s/def ::hour (s/int-in 0 24))

(s/def ::minute (s/int-in 0 60))

(s/def ::years
  (s/and (s/coll-of ::year :distinct true)
         sorted-entries?))

(s/def ::months
  (s/and (s/coll-of ::month :distinct true)
         sorted-entries?))

(s/def ::days
  (s/and (s/coll-of ::day :distinct true)
         sorted-entries?))

(s/def ::days-of-week
  (s/coll-of ::hour :distinct true))

(s/def ::hours
  (s/and (s/coll-of ::hour :distinct true)
         sorted-entries?))

(s/def ::minutes
  (s/and (s/coll-of ::minute :distinct true)
         sorted-entries?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn- convert-day-of-week
  [d]
  (if (string? d) (get day-of-week-map d) d))

(defn- convert-month
  [m]
  (if (string? m) (get month-of-year-map m) m))

(defn- reduce-values
  [vs* v]
  (if (coll? v)
    (let [[start end] v
          vrange (range start (inc end))]
      (into vs* vrange))
    (conj vs* v)))

(defn- convert-values [vs]
  (reduce reduce-values [] vs))

(defn- convert-unit
  [[unit vs]]
  (let [unit*
        (case unit
          :daysOfWeek :days-of-week
          :daysOfMonth :day-of-month
          unit)
        vs*
        (case unit
          :daysOfWeek
          (->> vs convert-values (map convert-day-of-week))
          :months
          (->> vs convert-values (map convert-month))
          ;; else
          (->> vs convert-values))]
    [unit* vs*]))

(defn- convert-bound
  [bound]
  (let [bound* (map convert-unit bound)]
    (reduce (fn [bound** [unit vs]]
              (-> bound**
                  (assoc-in [:sets unit] (set vs))
                  (assoc-in [:ranges unit] (sort (distinct vs)))))
            {:sets {} :ranges {}}
            bound*))
  (->> bound (map convert-unit) (into {})))

(defn convert-bounds
  [bounds]
  (mapv convert-bound bounds))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn time-map
  "Return a map of different times from the Instant `timestamp` and a
   `timezone` string."
  [timestamp timezone]
  (let [[year month day-month day-week hour minute]
        (t/as (t/zoned-date-time timestamp timezone)
              :year
              :month-of-year
              :day-of-month
              :day-of-week
              :hour-of-day
              :minute-of-hour)]
    {:year         year
     :month        month
     :day-of-month day-month
     :day-of-week  day-week
     :hour         hour
     :minute       minute}))

(defn- in-bound-unit?
  [unit-set unit]
  (or (nil? unit-set)
      (contains? unit-set unit)))

(defn- in-bound?
  [{{:keys [years months days-of-month days-of-week hours minutes]} :sets}
   date-time]
  (let [{:keys [year month day-of-month day-of-week hour minute]}
        (t/as date-time
              :year
              :month-of-year
              :day-of-month
              :day-of-week
              :hour-of-day
              :minute-of-hour)]
    (and (in-bound-unit? years year)
         (in-bound-unit? months month)
         (in-bound-unit? days-of-month day-of-month)
         (in-bound-unit? days-of-week day-of-week)
         (in-bound-unit? hours hour)
         (in-bound-unit? minutes minute))))

(defn bounded-time?
  "Is `date-time` within any of the `bounds`? Returns vacuously `true` if
   `bounds` is empty."
  [bounds date-time]
  (or (empty? bounds)
      (boolean (some (fn [bound] (in-bound? bound date-time)) bounds))))

(defn- current-t?
  [inst-time t current-interval?]
  (and current-interval?
       (= inst-time t)))

(defn- future-t?
  [inst-time t future-interval? current-interval?]
  (or future-interval?
      (and current-interval?
           (< inst-time t))))

(defn next-bounded-times
  "Returns a sequence of the next LocalDateTime that are within `bounds`."
  [{{:keys [years months days-of-month hours minutes]
     :or {months        (range 1 13)
          days-of-month (range 1 32)
          hours         (range 0 24)
          minutes       (range 0 60)}}
    :ranges
    {:keys [days-of-week]}
    :sets}
   date-time]
  (let [[inst-yr
         inst-mon
         inst-day
         inst-hr
         inst-min]
        (t/as date-time
              :year
              :month-of-year
              :day-of-month
              :hour-of-day
              :minute-of-hour)
        day-of-week?
        (if (empty? days-of-week)
          (constantly true)
          (fn [y m d] (contains? days-of-week (day-of-week y m d))))]
    (for [;; Years
          year  years
          :let  [current-yr? (current-t? inst-yr year true)
                 future-yr?  (future-t? inst-yr year true true)]
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
          :when (or current-min? future-min?)]
      (t/local-date-time year month day hour min))))

(def min-ms 60000.0) ; The amount of milliseconds in one minute

(defn increment-period
  "Generate a new millisecond time value that to be added upon the prev time.
   The time difference is an exponentially-distributed random variable
   with `mean`; the `min` paramter also adds a fixed minimum
   time to the value, for a new mean `mean + min`. This ensures that the
   events occur as a Poisson random process. Note that this assumes the
   millisecond as the basic unit of time."
  [rng {:keys [mean min]
        :or {mean min-ms
             min  0}}]
  (let [rate   (/ 1.0 mean)
        t-diff (long (random/rand-exp rng rate))]
    (+ min t-diff)))
