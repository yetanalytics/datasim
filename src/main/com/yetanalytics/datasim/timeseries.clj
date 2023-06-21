(ns com.yetanalytics.datasim.timeseries
  "Timeseries namespaces; all timeseries are lazy, potentially infinite
   sequences of numeric values."
  (:require [clojure.spec.alpha :as s]
            [java-time          :as t]
            [com.yetanalytics.datasim.random     :as random]
            [com.yetanalytics.datasim.util.maths :as maths]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARMA (AutoRegressive Moving Average) Sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Specs

(s/def ::safe-double
  (s/double-in :infinite? false
               :NaN? false))

(s/def ::phi
  (s/coll-of ::safe-double
             :into []))

(s/def ::theta
  (s/coll-of ::safe-double
             :into []))

(s/def ::std
  ::safe-double)

(s/def ::c
  ::safe-double)

(s/def ::seed
  int?)

(s/def ::ar
  (s/keys :req-un [::phi
                   ::std
                   ::c
                   ::seed]))

(s/def ::ma
  (s/keys :req-un [::theta
                   ::std
                   ::c
                   ::seed]))

(s/def ::arma
  (s/merge ::ar ::ma))

(s/fdef arma-seq
  :args (s/cat :arma-model ::arma)
  :ret (s/every ::safe-double))

;; ARMA Function

(defn arma-seq
  "ARMA - AutoRegressive-Moving-Average - sequence generation.
   
   An ARMA model describes a stochastic process in terms of two polynomials:
   the autogregression term and the moving average term. The model is
   written as such:
   ```
   X_t = epsilon_t + SUM(phi_i X_t-i, i=1, p) + SUM(theta_i epsilon_t-i, i=1, q)
   ```
   where `X_t` is the `t`-th value, `epsilon_i` are white noise parameters and
   `phi_i` and `theta_i` are the central parameters for the AR and MA models,
   respectively.

   Besides `:phi` and `:theta`, which are colls of `p` and `q` double values
   respectively, the `arma-model` option map also has these additional params:
   - `:std`, the standard deviation of the Gaussian distribution from which each
     `epsilon_t` is sampled from (the mean is fixed at zero)
   -  `:seed`, the seed to create the `epsilon_t`-generating RNG with.
   - `:c`, a constant to add to each result `X_t`
   
   Returns an infinite lazy seq of ARMA values."
  ([{:keys [std phi theta c seed] :as arma-model}]
   (let [rng (random/seed-rng seed)
         arma-seq*
         (fn arma-seq* [prev-xs prev-epsilons]
           (lazy-seq
            (let [epsilon (random/rand-gauss rng 0.0 std)
                  sum-ar  (->> (map * phi prev-xs)
                               (reduce + 0.0))
                  sum-ma  (->> (map * theta prev-epsilons)
                               (reduce + 0.0))
                  x       (+ c epsilon sum-ar sum-ma)]
              (cons x (arma-seq* (cons x prev-xs)
                                 (cons epsilon prev-epsilons))))))]
     (with-meta (arma-seq* [] [])
       {::seed seed
        ::arma arma-model}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Temporal Sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constants

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

(def minute-per-day
  "The fraction one minute makes up of a day."
  (/ ms-per-minute ms-per-day))

;; Helper Functions

(defn- time-seq
  "Generate a sequence of epoch milliseconds starting at `t-zero` ms, skipping
   over `step` milliseconds with "
  [t-zero ?sample-n step]
  (cond->> (range t-zero Long/MAX_VALUE step)
    ?sample-n
    (take (quot ?sample-n step))))

(defn- time-of-time-seq
  "Given `time-seq` of epoch milliseconds, convert it into a cyclic sequence
   described by `of-keyword`, with `zone` provided to compensate for the local
   date time. Valid values of `as-keyword` include `:hour-of-day`,
   `:day-of-week`, `:day-of-month`, and `:day-of-year`."
  [time-seq zone of-keyword]
  (map (fn [t]
         (t/as (t/local-date-time t zone) of-keyword))
       time-seq))

(defn- mod-seq->day-night-seq
  "Convert `min-of-day-seq` into a cosine wave sequence, where 0 (midnight)
   becomes 1 and 720 (noon) becomes -1."
  [min-of-day-seq]
  (map (fn [min-of-day]
         (-> min-of-day
             (* minute-per-day) ; fraction of day
             (* 2.0 Math/PI)    ; cosine wave
             Math/cos))
       min-of-day-seq))

;; Specs

(defn- lazy-seq? [coll] (instance? clojure.lang.LazySeq coll))

(defmacro int-mod [divisor]
  `(s/and int? (fn [n#] (zero? (mod n# ~divisor)))))

(s/def ::t-zero int?)

(s/def ::sample-n int?)

(s/def ::zone
  (s/with-gen (s/and string? #(try (t/zone-id %) true
                                   (catch Exception _ false)))
    (fn [] "UTC")))

(s/def ::t-seq
  (s/every int? :kind lazy-seq?))

(s/def ::sec-seq
  (s/every (int-mod ms-per-second) :kind lazy-seq?))

(s/def ::min-seq
  (s/every (int-mod ms-per-minute) :kind lazy-seq?))

(s/def ::hour-seq
  (s/every (int-mod ms-per-hour) :kind lazy-seq?))

(s/def ::day-seq
  (s/every (int-mod ms-per-day) :kind lazy-seq?))

(s/def ::week-seq
  (s/every (int-mod ms-per-week) :kind lazy-seq?))

(s/def ::moh-seq
  (s/every (s/int-in 0 60) :kind lazy-seq?))

(s/def ::hod-seq
  (s/every (s/int-in 0 24) :kind lazy-seq?))

(s/def ::dow-seq
  (s/every (s/int-in 0 7) :kind lazy-seq?))

(s/def ::dom-seq
  (s/every (s/int-in 0 31) :kind lazy-seq?))

(s/def ::doy-seq
  (s/every (s/int-in 0 366) :kind lazy-seq?))

(s/def ::day-night-seq
  (s/every (s/double-in -1.0 1.0) :kind lazy-seq?))

(s/fdef time-seqs
  :args (s/keys :opt-un [::t-zero ::sample-n ::zone])
  :ret (s/keys :req-un [::t-seq
                        ::sec-seq
                        ::min-seq
                        ::hour-seq
                        ::day-seq
                        ::week-seq
                        ::moh-seq
                        ::hod-seq
                        ::dow-seq
                        ::dom-seq
                        ::doy-seq
                        ::day-night-seq]))

;; Time seq function

(defn time-seqs
  "Given a `t-zero` (simulation start), an upper bound of `:sample-n`
   milliseconds and an optional local time `zone`, return a map of useful
   lazy time sequences. (Note that since these are lazy seqs, we do not
   waste performance overhead on unused sequences.)
   
   Time sequences in the map:

   | Sequence Key     | Description
   | ---              | ---
   | `:t-seq`         | Sequence of epoch milliseconds since `t-zero` (e.g. `(0 1 2 ...)`)
   | `:sec-seq`       | Sequence of epoch ms with an interval of one second (e.g. `(0 1000 ...)`)
   | `:min-seq`       | Sequence of epoch ms with an interval of one minute (e.g. `(0 60000 ...)`)
   | `:hour-seq`      | Sequence of epoch ms with an interval of one hour (e.g. `(0 3600000 ...)`)
   | `:day-seq`       | Sequence of epoch ms with an interval of one day (e.g. `(0 86400000 ...)`)
   | `:week-seq`      | Sequence of epoch ms with an interval of one week (e.g. `(0 604800000 ...)`)
   | `:moh-seq`       | Cyclic sequence of minutes per hour (e.g. `(0 1 ... 59 0 ...)`)
   | `:hod-seq`       | Cyclic sequence of hours per day (e.g. `(0 1 ... 23 0 ...)`)
   | `:dow-seq`       | Cyclic sequence of days per week (e.g. `(0 1 ... 6 0 ...)`)
   | `:dom-seq`       | Cyclic sequence of days per month (e.g. `(0 1 ... 30 0 ...)`)
   | `:doy-seq`       | Cyclic sequence of days per year (e.g. `(0 1 ... 355 0 ...)`)
   | `:day-night-seq` | Sinusoidal sequence where negative numbers denote daytime and positive numbers nighttime. Range is from -1 (at noon) to 1 (at midnight)."
  [& {:keys [t-zero
             sample-n
             ^java.time.ZoneRegion zone]
      :or {t-zero 0
           zone ^java.time.ZoneRegion (t/zone-id "UTC")}}]
  (let [;; Primary
        t-seq    (time-seq t-zero sample-n 1)
        sec-seq  (time-seq t-zero sample-n ms-per-second)
        min-seq  (time-seq t-zero sample-n ms-per-minute)
        hour-seq (time-seq t-zero sample-n ms-per-hour)
        day-seq  (time-seq t-zero sample-n ms-per-day)
        week-seq (time-seq t-zero sample-n ms-per-week)
        ;; Secondary/Local
        moh-seq (time-of-time-seq min-seq zone :minute-of-hour)
        mod-seq (time-of-time-seq min-seq zone :minute-of-day)
        hod-seq (time-of-time-seq hour-seq zone :hour-of-day)
        dow-seq (time-of-time-seq day-seq zone :day-of-week)
        dom-seq (time-of-time-seq day-seq zone :day-of-month)
        doy-seq (time-of-time-seq day-seq zone :day-of-year) 
        day-night-seq (mod-seq->day-night-seq mod-seq)]
    {;; Primary
     :t-seq    t-seq
     :sec-seq  sec-seq
     :min-seq  min-seq
     :hour-seq hour-seq
     :day-seq  day-seq
     :week-seq week-seq
     ;; Secondary/local
     :moh-seq moh-seq
     :mod-seq mod-seq
     :hod-seq hod-seq
     :dow-seq dow-seq
     :dom-seq dom-seq
     :doy-seq doy-seq
     :day-night-seq day-night-seq}))

(comment
  ;; Incanter namespaces are dev-only
  (require '[incanter.core :refer [view]]
           '[incanter.charts :refer [time-series-plot]])

  (time
   (let [sim-seed 42
         ;; Create a master RNG for the sim. This is used only to generate other seeds 
         sim-rng (random/seed-rng sim-seed)

         ;; the start of the sim, in ms since epoch
         t-zero 0;; (System/currentTimeMillis)
         ;; the amount of time, in MS, this sim covers
         sample-n (:whole (t/convert-amount 7 :days
                                            :millis))

         ;; a local timezone
         timezone (t/zone-id "America/New_York")

         ;; Build useful time seqs, only get eval'd if used!
         {:keys [min-seq
                 mod-seq
                 day-night-seq]} (time-seqs :t-zero t-zero
                                            :sample-n sample-n
                                            :zone timezone)

         ;; in our model, a timeseries for a given actor is measured against
         ;; a composite timeseries representing abstract challenge/diversity.
         ;; This is a combination of a stochastic series representing
         ;; unpredictable factors that apply to the group, higher is harder.
         ;; this series is max'd with a day night cycle (day is easier, night is
         ;; harder). Think of this combined series as a mask.

         ;; When an actor's series is greater than the challenge, the difference
         ;; between the two is the probability (from 0.0 to 1.0) that an event
         ;; will happen at that time.

         ;; random stochastic settings can (and probably should) be shared.
         common-arma {:phi [0.5 0.2]
                      :theta []
                      :std 0.25
                      :c 0.0}

         ;; Generate a seed for the group
         group-seed (.nextLong sim-rng)

         ;; create a stochastic seq for the group
         group-arma (arma-seq (merge common-arma
                                     {:seed group-seed}))

         ;; Create a periodic seq for the lunch hour break
         lunch-hour-seq (map
                         (fn [x]
                           (if (<= 720 x 780)
                             1.0
                             -1.0))
                         mod-seq)
         ;; form a mask for the group + day-night + lunch
         mask (map max
                   group-arma
                   day-night-seq
                   lunch-hour-seq)

         ;; create a seed for Bob's seq
         bob-arma-seed (.nextLong sim-rng)

         ;; create a stochastic seq for bob
         bob-arma (arma-seq
                   (merge common-arma
                          {:seed bob-arma-seed}))

         ;; Bob's activity probability
         bob-prob (map (fn [a b]
                         (double
                          (maths/min-max 0.0 (/ (- a b) 2) 1.0)))
                       bob-arma
                       mask)
         ;; to keep it deterministic, give bob another seeded RNG to take with him.
         bob-rng (random/seed-rng (.nextLong sim-rng))

         ;; Compose the time (in minute increments), bob's probability
         ;; and his RNG and you have everything you need to generate events for
         ;; bob. Here the RNG is used to generate a sequence for demonstration,
         ;; in practice it would get handed off to a thread or some such.
         bob-seq (map (fn [t prob rand-long]
                        {:t t
                         :prob prob
                         :r rand-long})
                      min-seq
                      bob-prob
                      (repeatedly #(random/rand-long bob-rng)))]
     
     (view (time-series-plot
            (map :t bob-seq)
            (map :prob bob-seq)))
     (view (time-series-plot
            (map :t bob-seq)
            (map :r bob-seq))))))
