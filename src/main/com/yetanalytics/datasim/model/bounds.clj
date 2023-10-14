(ns com.yetanalytics.datasim.model.bounds
  (:require [clojure.spec.alpha     :as s]
            [clojure.spec.gen.alpha :as sgen]
            [java-time.api          :as t]
            [com.yetanalytics.datasim.input.model.alignments :as align]))

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

(s/def ::day-of-month (s/int-in 1 32))

(s/def ::day-of-week (s/int-in 0 6))

(s/def ::hour (s/int-in 0 24))

(s/def ::minute (s/int-in 0 60))

(s/def ::second (s/int-in 0 60))

(s/def ::years
  (s/coll-of ::year :distinct true :min-count 1))

(s/def ::months
  (s/coll-of ::month :distinct true :min-count 1))

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

(def ^:private bound-spec
  (s/keys :opt-un [::years
                   ::months
                   ::days-of-month
                   ::days-of-week
                   ::hours
                   ::minutes
                   ::seconds]))

(s/def ::ranges
  (s/with-gen (s/and bound-spec
                     (s/map-of keyword? (s/and seq? sorted-entries?)))
    (fn [] (sgen/fmap #(update-vals % sort)
                      (s/gen bound-spec)))))

(s/def ::sets
  (s/with-gen (s/and bound-spec
                     (s/map-of keyword? set?))
    (fn [] (sgen/fmap #(update-vals % set)
                      (s/gen bound-spec)))))

(s/def ::bounds
  (s/every (s/keys :req-un [::ranges ::sets])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef time-map
  :args (s/cat :date-time t/local-date-time?)
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
     :day-of-week  (mod day-week 7) ; need to convert Sunday from 7 to 0
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
               :day   ::day-of-month)
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
               :day   ::day-of-month)
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
    :daysOfMonth :days-of-month
    unit))

(defn- values->range
  [start end ?step]
  (let [start* (string->int start)
        end*   (inc (string->int end))]
    (if ?step
      (range start* end* ?step)
      (range start* end*))))

(defn- reduce-values
  [vs* v]
  (cond
    (map? v)
    (let [{:keys [start end step]} v
          vrange (values->range start end step)]
      (into vs* vrange))
    (coll? v)
    (let [[start end step] v
          vrange (values->range start end step)]
      (into vs* vrange))
    :else
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bounded Time?

(defn- in-bound-unit?
  [unit-set unit]
  (or (nil? unit-set)
      (contains? unit-set unit)))

(defn- in-bound?
  [{{:keys [years months days-of-month days-of-week hours minutes seconds]} :sets}
   date-time]
  (let [{:keys [year month day-of-month day-of-week hour minute second]}
        (time-map date-time)]
    (and (in-bound-unit? years year)
         (in-bound-unit? months month)
         (in-bound-unit? days-of-month day-of-month)
         (in-bound-unit? days-of-week day-of-week)
         (in-bound-unit? hours hour)
         (in-bound-unit? minutes minute)
         (in-bound-unit? seconds second))))

(s/fdef bounded-time?
  :args (s/cat :bounds    ::bounds
               :date-time t/local-date-time?)
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
  "Returns a sequence of the next LocalDateTime that are within the bound."
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
        years
        (or years
            (range inst-yr Integer/MAX_VALUE))
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
               :date-time t/local-date-time?)
  :ret t/local-date-time?)

(defn next-bounded-time
  "Returns the next timestamp after `date-time` within any of the `bounds`."
  [bounds date-time]
  (some (fn [bound]
          (first (next-bounded-times bound date-time)))
        bounds))
