(ns com.yetanalytics.datasim.timeseries
  (:require [clojure.spec.alpha :as s]
            [java-time          :as t]
            [com.yetanalytics.datasim.random     :as random]
            [com.yetanalytics.datasim.util.maths :as maths]))

;; Primitive seqs, just lazy seqs of numerics

;; ARMA, stochasic pseudorandom

(s/def ::safe-double
  (s/double-in
   :infinite? false
   :NaN? false))

(s/def ::phi
  (s/coll-of ::safe-double
             :into []))

(s/def ::std
  ::safe-double)

(s/def ::c
  ::safe-double)

(s/def ::seed
  int?)

(s/def ::ar
  (s/keys
   :req-un
   [::phi
    ::std
    ::c
    ::seed]))

(s/def ::theta
  (s/coll-of ::safe-double
             :into []))

(s/def ::ma
  (s/keys
   :req-un
   [::theta
    ::std
    ::c
    ::seed]))

(s/def ::arma
  (s/merge ::ar ::ma))

(s/def ::rng
  ::random/rng)

(s/def ::value
  ::safe-double)

(s/def ::epsilon
  ::safe-double)

(s/fdef arma-seq
  :args (s/cat :arma-model ::arma
               :recur-args (s/?
                            (s/cat :prev-value ::value
                                   :prev-epsilon ::epsilon
                                   :rng ::rng)))
  :ret (s/every ::safe-double))

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

(defn- local-seq-as
  [xs zone as]
  (map (fn [stamp]
         (t/as (t/local-date-time stamp zone)
               as))
       xs))

;; TODO: Uncomment commented-out time seq code; either return all or add an
;; arg to select which ones to return
(defn time-seqs
  "Given a t-zero (simulation start), an upper bound of sample-n milliseconds
  and an optional local timezone, return a map of useful lazy time sequences.
   
   Time sequences in the map:
   - `:t-seq`
   - `:sec-seq`
   - `:min-seq`
   - `:hour-seq`
   - `:day-seq`
   - `:week-seq`
   - `:moh-seq` (minute of hour)
   - `:hod-seq` (hour of day)
   - `:dow-seq` (day of week)
   - `:dom-seq` (day of month)
   - `:doy-seq` (day of year)
   - `:day-night-seq`"
  [& {:keys [t-zero
             sample-n
             ^java.time.ZoneRegion zone]
      :or {t-zero 0
           zone ^java.time.ZoneRegion (t/zone-id "UTC")}}]
  (let [t-seq (if sample-n
                (range t-zero sample-n)
                (range))
        r-partial (if sample-n
                    (fn [step]
                      (take (quot sample-n
                                  step)
                            (range t-zero Long/MAX_VALUE step)))
                    (partial range t-zero Long/MAX_VALUE))
        ;; Primary
        ;; sec-seq (r-partial 1000)
        min-seq (r-partial 60000)
        ;; hour-seq (r-partial 3600000)
        ;; week-seq (r-partial 604800000)
        ;; day-seq (r-partial 86400000)

        ;; secondary/local
        ;; moh-seq (local-seq-as min-seq
        ;;                       zone
        ;;                       :minute-of-hour)
        mod-seq (local-seq-as min-seq
                              zone
                              :minute-of-day)
        day-night-seq (map (comp
                            #(Math/cos ^Double %)
                            #(double (* 2 Math/PI (/ % 86400000)))
                            (partial * 60000))
                           mod-seq)

        ;; hod-seq (local-seq-as hour-seq
        ;;                       zone
        ;;                       :hour-of-day)


        ;; dow-seq (local-seq-as day-seq
        ;;                       zone
        ;;                       :day-of-week)

        ;; dom-seq (local-seq-as day-seq
        ;;                       zone
        ;;                       :day-of-month)
        ;; doy-seq (local-seq-as day-seq
        ;;                       zone
        ;;                       :day-of-year)
        ]
    {; :t-seq t-seq
     ; :sec-seq sec-seq
     :min-seq min-seq
     ;:hour-seq hour-seq
     ;:day-seq day-seq
     ;:week-seq week-seq
     ;:moh-seq moh-seq
     :mod-seq mod-seq
     :day-night-seq day-night-seq
     ;:hod-seq hod-seq
     ;:dow-seq dow-seq
     ;:dom-seq dom-seq
     ;:doy-seq doy-seq
     }))

(comment

  (use '(incanter core stats charts io))

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
         {:keys [week-seq
                 min-seq
                 t-seq
                 doy-seq
                 moh-seq
                 day-seq
                 sec-seq
                 dom-seq
                 hod-seq
                 hour-seq
                 dow-seq
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
                      (rand-seq :val-type :long
                                :rng bob-rng))]
     (view (time-series-plot
            (map :t bob-seq)
            (map :prob bob-seq)))
     (view (time-series-plot
            (map :t bob-seq)
            (map :r bob-seq)))))


  )
