(ns com.yetanalytics.datasim.timeseries
  "Timeseries namespaces; all timeseries are lazy, potentially infinite
   sequences of numeric values."
  (:require [clojure.spec.alpha     :as s]
            [clojure.spec.gen.alpha :as sgen]
            [java-time              :as t]
            [com.yetanalytics.datasim.random :as random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARMA (AutoRegressive Moving Average) Sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Specs

;; These values can be any non-inf double, but we limit possible gen values so
;; that we don't get infinite result values (e.g. with phi values that result
;; in a non-stationary ARMA seq)
(s/def ::safe-double
  (s/with-gen (s/double-in :infinite? false
                           :NaN? false)
    #(sgen/double* {:min -1.0 :max 1.0})))

(s/def ::phi
  (s/coll-of ::safe-double :into []))

(s/def ::theta
  (s/coll-of ::safe-double :into []))

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

;; The return value can't be `::safe-double` in case arma-seq is not stationary,
;; in which case the sequence will blow up to infinity.
(s/fdef arma-seq
  :args (s/cat :arma-model ::arma)
  :ret (s/every double?))

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

(def minute-day-fraction
  (/ ms-per-minute ms-per-day))

(def hour-day-fraction
  (/ ms-per-hour ms-per-day))

;; Helper Functions

(defn- time-seq
  "Generate a sequence of epoch milliseconds starting at `t-zero` ms, skipping
   over `step` milliseconds with and optionally limiting samples up to
   `?sample-n` ms."
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

(defn- time-of-day-seq->day-night-seq
  "Convert `time-of-day-seq` into a cosine wave sequence, where 0 (midnight)
   becomes 1 and `day-percent = 1/2` (noon) becomes -1."
  [time-of-day-seq day-percent]
  (map (fn [t]
         (-> t
             (* day-percent) ; fraction of day
             (* 2.0 Math/PI) ; cosine wave
             Math/cos))
       time-of-day-seq))

;; Specs

(defn- lazy-seq? [coll]
  (or (instance? clojure.lang.LazySeq coll)
      ;; `range` returns these types instead of LazySeq
      (instance? clojure.lang.LongRange coll)
      (instance? clojure.lang.Range coll)))

(s/def ::t-zero int?)

(s/def ::sample-ms (s/and int? (comp not zero?)))

(s/def ::zone
  (s/with-gen t/zone-id?
    #(sgen/fmap (fn [[sign hr]]
                  (t/zone-id (format "UTC%s%d" sign hr)))
                (sgen/tuple (sgen/elements ["-" "+"])
                            (sgen/choose 0 18)))))

(s/def ::milliseconds-seq
  (s/every int? :kind lazy-seq?))

(s/def ::second-ms-seq
  (s/every int? :kind lazy-seq?))

(s/def ::minute-ms-seq
  (s/every int? :kind lazy-seq?))

(s/def ::hour-ms-seq
  (s/every int? :kind lazy-seq?))

(s/def ::day-ms-seq
  (s/every int? :kind lazy-seq?))

(s/def ::week-ms-seq
  (s/every int? :kind lazy-seq?))

(s/def ::minute-of-hour-seq
  (s/every (s/int-in 0 60) :kind lazy-seq?))

(s/def ::minute-of-day-seq
  (s/every (s/int-in 0 1440) :kind lazy-seq?))

(s/def ::hour-of-day-seq
  (s/every (s/int-in 0 24) :kind lazy-seq?))

(s/def ::day-of-week-seq
  (s/every (s/int-in 1 8) :kind lazy-seq?))

(s/def ::day-of-month-seq
  (s/every (s/int-in 1 32) :kind lazy-seq?))

(s/def ::day-of-year-seq
  (s/every (s/int-in 1 367) :kind lazy-seq?))

(s/def ::minute-day-night-seq
  (s/every (s/double-in :min -1.0 :max 1.0) :kind lazy-seq?))

(s/def ::hour-day-night-seq
  (s/every (s/double-in :min -1.0 :max 1.0) :kind lazy-seq?))

(s/fdef time-seqs
  :args (s/cat :kwargs (s/keys* :opt-un [::t-zero ::sample-ms ::zone]))
  :ret (s/keys :req-un [::milliseconds-seq
                        ::second-ms-seq
                        ::minute-ms-seq
                        ::hour-ms-seq
                        ::day-ms-seq
                        ::week-ms-seq
                        ::minute-of-hour-seq
                        ::minute-of-day-seq
                        ::hour-of-day-seq
                        ::day-of-week-seq
                        ::day-of-month-seq
                        ::day-of-year-seq
                        ::minute-day-night-seq
                        ::hour-day-night-seq]))

;; Time seq function

(defn time-seqs
  "Given a `:t-zero` (simulation start), an exclusive upper bound of
   `:sample-ms` milliseconds and an optional local time `zone`, return a map
   of useful lazy time sequences. (Note that since these are lazy seqs, we do
   not waste performance overhead on unused sequences.)

   There are three kinds of sequences: sequences of milliseconds (intervals
   being equal to the time unit used); cyclic sequences of time with relation
   to the larger unit; and sinusoidal sequences of time with relation to the
   day-night cycle, where 1.0 represents midnight, -1.0 represents noon,
   positive numbers nighttime, and negative numbers daytime.
   
   Time sequences in the returned map:

   | Sequence Key            | Description
   | ---                     | ---
   | `:milliseconds-seq`     | Sequence of epoch milliseconds since `t-zero` (e.g. `(0 1 2 ...)`)
   | `:second-ms-seq`        | Sequence of epoch ms with an interval of one second (e.g. `(0 1000 ...)`)
   | `:minute-ms-seq`        | Sequence of epoch ms with an interval of one minute (e.g. `(0 60000 ...)`)
   | `:hour-ms-seq`          | Sequence of epoch ms with an interval of one hour (e.g. `(0 3600000 ...)`)
   | `:day-ms-seq`           | Sequence of epoch ms with an interval of one day (e.g. `(0 86400000 ...)`)
   | `:week-ms-seq`          | Sequence of epoch ms with an interval of one week (e.g. `(0 604800000 ...)`)
   | `:minute-of-hour-seq`   | Cyclic sequence of minutes per hour (e.g. `(0 1 ... 59 0 ...)`)
   | `:minute-of-day-seq`    | Cyclic sequence of minutes per day (e.g. `(0 1 ... 1440 0 ...)`)
   | `:hour-of-day-seq`      | Cyclic sequence of hours per day (e.g. `(0 1 ... 23 0 ...)`)
   | `:day-of-week-seq`      | Cyclic sequence of the day of the week (e.g. `(1 2 ... 7 1 ...)`)
   | `:day-of-month-seq`     | Cyclic sequence of the day of the month (e.g. `(1 2 ... 31 1 ...)`)
   | `:day-of-year-seq`      | Cyclic sequence of the day of the year (e.g. `(1 2 ... 365 1 ...)`)
   | `:minute-day-night-seq` | Sinusoidal sequence of milliseconds in relation to the day-night cycle.
   | `:hour-day-night-seq`   | Sinusoidal sequence of milliseconds in relation to the day-night cycle."
  [& {:keys [t-zero
             sample-ms
             ^java.time.ZoneRegion zone]
      :or {t-zero 0
           zone ^java.time.ZoneRegion (t/zone-id "UTC")}}]
  (let [;; Primary
        ms-seq   (time-seq t-zero sample-ms 1)
        sec-seq  (time-seq t-zero sample-ms ms-per-second)
        min-seq  (time-seq t-zero sample-ms ms-per-minute)
        hour-seq (time-seq t-zero sample-ms ms-per-hour)
        day-seq  (time-seq t-zero sample-ms ms-per-day)
        week-seq (time-seq t-zero sample-ms ms-per-week)
        ;; Secondary/Local
        moh-seq (time-of-time-seq min-seq zone :minute-of-hour)
        mod-seq (time-of-time-seq min-seq zone :minute-of-day)
        hod-seq (time-of-time-seq hour-seq zone :hour-of-day)
        dow-seq (time-of-time-seq day-seq zone :day-of-week)
        dom-seq (time-of-time-seq day-seq zone :day-of-month)
        doy-seq (time-of-time-seq day-seq zone :day-of-year)
        mdn-seq (time-of-day-seq->day-night-seq mod-seq minute-day-fraction)
        hdn-seq (time-of-day-seq->day-night-seq hod-seq hour-day-fraction)]
    {;; Primary
     :milliseconds-seq     ms-seq
     :second-ms-seq        sec-seq
     :minute-ms-seq        min-seq
     :hour-ms-seq          hour-seq
     :day-ms-seq           day-seq
     :week-ms-seq          week-seq
     ;; Secondary/local
     :minute-of-hour-seq   moh-seq
     :minute-of-day-seq    mod-seq
     :hour-of-day-seq      hod-seq
     :day-of-week-seq      dow-seq
     :day-of-month-seq     dom-seq
     :day-of-year-seq      doy-seq
     ;; Day-night cycle
     :minute-day-night-seq mdn-seq
     :hour-day-night-seq   hdn-seq}))
