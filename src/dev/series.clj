(ns series
  (:require [java-time       :as t]
            [incanter.core   :refer [view]]
            [incanter.charts :refer [histogram time-series-plot]]
            [incanter.stats  :as stats]
            [com.yetanalytics.datasim.util.maths      :as maths]
            [com.yetanalytics.datasim.math.random     :as random]
            [com.yetanalytics.datasim.math.timeseries :as ts]
            [com.yetanalytics.datasim.sim             :as sim]))

;; ARMA seq plot + sample mean plot
;; Try it with different values of phi and theta
;; In particular, setting both to empty vecs will result in simple white noise
;; See: https://en.wikipedia.org/wiki/Autoregressive_model#Graphs_of_AR(p)_processes
;; for examples of different parameters of phi and their effects
(comment
  (let [n        10000
        range-n  (range n)
        arma-seq (take n (ts/arma-seq {:phi   [0.3 0.3]
                                       :theta []
                                       :std   1
                                       :c     0
                                       :seed  35}))]
    (view (time-series-plot
           range-n
           arma-seq
           :x-label "N"
           :y-label "ARMA Value"))
    (view (time-series-plot
           range-n
           (stats/cumulative-mean arma-seq)
           :x-label "N"
           :y-label "ARMA Cumulative Sample Mean")))
  )

;; Plot of auto-correlation with respect to lag, i.e. the difference
;; between any two given times.
;; In this case, auto-correlation is also auto-covariance since the
;; mean of the arma seq is 0 (auto-cov = auto-cor - mean_t1 * mean_t2).
;; Note the sinusoidal pattern of the plot.
(comment
  (let [n         10000
        max-lag   100
        range-lag (range max-lag)
        arma-seq  (take n (ts/arma-seq {:phi   [0.3]
                                        :theta []
                                        :std   1
                                        :c     0
                                        :seed  4000}))]
    ;; Note that for lag = 0, the auto-cor = auto-cov = std^2 = 1
    #_(println (stats/auto-correlation arma-seq 0))
    (view (time-series-plot
           range-lag
           (map (partial stats/auto-correlation arma-seq) (range max-lag))
           :x-label "Time difference"
           :y-label "ARMA autocovariance")))
  )

;; Model a statement seq
(comment
  (let [;; Create a master RNG for the sim. This is used only to generate other seeds 
        sim-seed 42
        sim-rng (random/seed-rng sim-seed)
        ;; The start of the sim, in ms since epoch
        t-zero 0 ; (System/currentTimeMillis)
        ;; The amount of time, in MS, this sim covers
        sample-n (:whole (t/convert-amount 7 :days :millis))
        ;; A local timezone
        timezone (t/zone-id "America/New_York")
        ;; Build useful time seqs, only get eval'd if used!
        {:keys [minute-ms-seq
                minute-of-day-seq
                minute-day-night-seq]} (ts/time-seqs :t-zero t-zero
                                                     :sample-n sample-n
                                                     :zone timezone)
        ;; In our model, a timeseries for a given actor is measured against
        ;; a composite timeseries representing abstract challenge/diversity.
        ;; This is a combination of a stochastic series representing
        ;; unpredictable factors that apply to the group, higher is harder.
        ;; this series is max'd with a day night cycle (day is easier, night is
        ;; harder). Think of this combined series as a mask.
        
        ;; When an actor's series is greater than the challenge, the difference
        ;; between the two is the probability (from 0.0 to 1.0) that an event
        ;; will happen at that time.
        
        ;; Random stochastic settings can (and probably should) be shared.
        common-arma {:phi   [0.5 0.2]
                     :theta []
                     :std   0.25
                     :c     0.0}
        ;; Generate a seed for the group
        group-seed (.nextLong sim-rng)
        ;; Create a stochastic seq for the group
        group-arma (ts/arma-seq (merge common-arma
                                       {:seed group-seed}))
        ;; Create a periodic seq for the lunch hour break
        lunch-hour-seq (map
                        (fn [x]
                          (if (<= 720 x 780)
                            1.0
                            -1.0))
                        minute-of-day-seq)
        ;; Form a mask for the group + day-night + lunch
        mask (map max
                  group-arma
                  minute-day-night-seq
                  lunch-hour-seq)
        ;; Create a seed for Bob's seq
        bob-arma-seed (.nextLong sim-rng)
        ;; Create a stochastic seq for Bob
        bob-arma (ts/arma-seq
                  (merge common-arma
                         {:seed bob-arma-seed}))
        ;; Bob's activity probability
        bob-prob (map (fn [a b]
                        (double
                         (maths/min-max 0.0 (/ (- a b) 2) 1.0)))
                      bob-arma
                      mask)
        ;; To keep it deterministic, give Bob another seeded RNG to take with him.
        bob-rng (random/seed-rng (.nextLong sim-rng))
        ;; Compose the time (in minute increments), Bob's probability
        ;; and his RNG and you have everything you need to generate events for
        ;; bob. Here the RNG is used to generate a sequence for demonstration,
        ;; in practice it would get handed off to a thread or some such.
        bob-seq (map (fn [t prob rand-long]
                       {:t    t
                        :prob prob
                        :r    rand-long})
                     minute-ms-seq
                     bob-prob
                     (repeatedly #(random/rand-long bob-rng)))]
    ;; Plot Bob's probability as a function of time
    (view (time-series-plot
           (map :t bob-seq)
           (map :prob bob-seq)))
    ;; Plot Bob's random numbers as a function of time
    (view (time-series-plot
           (map :t bob-seq)
           (map :r bob-seq))))
  )

(comment
  (def time-seqs
    (ts/time-seqs :t-zero (.toEpochMilli (java.time.Instant/now))))

  (def prob-mask-arma-seq
    (ts/arma-seq {:phi   [0.5 0.2]
                  :theta []
                  :std   0.25
                  :c     0.0
                  :seed  100}))

  (def prob-mask-seq
    (map max
         prob-mask-arma-seq
         (:minute-day-night-seq time-seqs)
         (map (fn [min-of-day]
                (if (<= 720 min-of-day 780) 1.0 -1.0))
              (:minute-of-day-seq time-seqs))))

  (defn clamp-probability [n]
    (maths/min-max 0.0 n 1.0))

  (def prob-seq
    (map (fn [arma-val prob-mask-val]
           (-> (- arma-val prob-mask-val) ; higher mask val -> lower prob
               (/ 2) ; decrease general range from [-1, 1] to [-0.5, 0.5]
               clamp-probability
               double))
         (ts/arma-seq {:phi   [0.5 0.2]
                       :theta []
                       :std   0.25
                       :c     0.0
                       :seed  120})
         prob-mask-seq))

  ;; Graphs should show a approximately sinusoidal pattern; graphing
  ;; `prob-seq` should show how probabilities are zero during the night
  ;; and the lunch hour, while varying sinusoidally during the rest of
  ;; the day.
  (view
   (time-series-plot (range 2000)
                     (take 2000 prob-mask-seq)))
  (view
   (time-series-plot (range 2000)
                     (take 2000 prob-seq)))
  )

;; Generate statements and plot the frequency of statements vs timestamps
(comment
  ;; Requires :test alias to be active
  (require '[com.yetanalytics.datasim.test-constants :as const])
  
  (def statement-seq
    (-> const/simple-input
        (assoc-in [:parameters :end]
                  "2019-11-21T11:38:39.219768Z") ; 3 days
        sim/sim-seq))
  
  (def timestamp-seq
    (map (comp :timestamp-ms meta) statement-seq))
  
  (view
   (histogram
    timestamp-seq
    :nbins 12))
  )
