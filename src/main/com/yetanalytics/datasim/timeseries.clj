(ns com.yetanalytics.datasim.timeseries
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [incanter.interpolation :as interp]
            [incanter.stats :as stats]
            [incanter.core :as incanter]
            [com.yetanalytics.datasim.clock :as clock]
            [java-time :as t]
            [com.yetanalytics.datasim.util.maths :as maths])
  (:import [java.util Random]))

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
  (s/with-gen #(instance? Random %)
    (fn []
      (sgen/return (Random.)))))

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

(defn arma-seq-const
  "Find the value of a stochastic sequence after n runs with the given model."
  [{:keys [std phi theta c
           seed rng

           value
           epsilon] :as arma-model
    :or {phi []
         theta []
         value 0.0
         epsilon 0.0}}
   n]
  (let [^Random rng (or rng
                        (and seed
                             (Random. seed))
                        (Random.))]
    (loop [^Double v value
           ^Double e epsilon
           n' 0]
      (let [new-epsilon (* (.nextGaussian rng) std)
            sum-phi (reduce (fn [old nxt]
                              (+ old
                                 (* nxt
                                    v)))
                            0.0 phi)
            sum-theta (reduce (fn [old nxt]
                                (+ old
                                   (* nxt e)))
                              0.0
                              theta)
            ret (+ c new-epsilon sum-phi sum-theta)]
        (if (= n' (inc n))
          v
          (recur ret new-epsilon (inc n')))))))

(defn arma-seq
  "Given arma params, return an infinite lazy seq of values"
  ([{:keys [seed] :as arma-model}]
   (with-meta
     (arma-seq arma-model
               0.0
               0.0
               (Random. seed))
     {::seed seed
      ::arma arma-model}))
  ([{:keys [std phi theta c] :as arma-model
     :or {phi []
          theta []}}
    ^Double prev-value
    ^Double prev-epsilon
    ^Random rng]
   (let [new-epsilon (* (.nextGaussian rng) std)
         sum-phi (reduce (fn [old nxt]
                           (+ old
                              (* nxt
                                 prev-value)))
                         0.0 phi)
         sum-theta (reduce (fn [old nxt]
                             (+ old
                                (* nxt prev-epsilon)))
                           0.0
                           theta)
         ret (+ c new-epsilon sum-phi sum-theta)]
     (lazy-seq
      (cons ret
            (arma-seq arma-model
                      ret
                      new-epsilon
                      rng))))))

#_(let [model {:phi [0.5 0.2]
             :theta []
             :std 0.25
             :c 0.0
             :seed 42}]
  (= (arma-seq-const
      model
      1000)
     (nth (arma-seq model)
          1000)))

(defn constant-seq
  "Return an infinite sequence of the given constant value"
  [constant]
  (repeat constant))

(defn rand-seq
  [& {:keys [seed
             rng
             val-type
             gauss-mean
             gauss-sd]
      :or {val-type :long
           gauss-mean 0.0
           gauss-sd 1.0}}]
  (lazy-seq
   (let [^Random rng (or rng
                         (and seed
                              (Random. seed))
                         (Random.))]
     (cons (case val-type
             :long (.nextLong rng)
             :gauss (+ (* gauss-sd (.nextGaussian rng)) gauss-mean)
             :double (.nextDouble rng))
           (rand-seq :rng rng
                     :val-type val-type
                     :gauss-mean gauss-mean
                     :gauss-sd gauss-sd)))))

#_(take 10 (rand-seq :val-type :gauss :seed 42)) ;; => (1.1419053154730547 0.9194079489827879 -0.9498666368908959 -1.1069902863993377 0.2809776380727795 0.6846227956326554 -0.8172214073987268 -1.3966434026780434 -0.19094451307087512 1.4862133923906502)

#_(take 10 (rand-seq :val-type :gauss
                   :gauss-sd 100
                   :gauss-mean 500 :seed 42
                   )) ;; => (614.1905315473055 591.9407948982788 405.0133363109104 389.3009713600662 528.0977638072779 568.4622795632655 418.27785926012734 360.33565973219567 480.9055486929125 648.621339239065)

(defn interpolate-seq
  "Given a series of point tuples where x is time and y is a known value, return
   an interpolated sequence of y every step"
  [& {:keys [;; init args
             points
             interpolation-type
             ;; Recur args
             step
             x
             interpolator
             interpolate-opts]
      :or {step 1
           x 0
           interpolation-type :cubic-hermite
           interpolate-opts []}}]
  (lazy-seq
   (let [interpolator (or interpolator
                          (apply
                           interp/interpolate
                           points interpolation-type
                           interpolate-opts))]
     (cons (interpolator x)
           (interpolate-seq
            :step step
            :x (+ x step)
            :interpolator interpolator)))))

#_(take 10 (interpolate-seq :points [[0 0] [4 6] [8 3]])) ;;=> (0.0 1.7109375 3.5625 5.1328125 6.0 5.8828125 5.0625 3.9609375 3.0 2.6015625)

#_(defn continuize-seq
  "Return a lazy seq of interpolation functions for the input seq."
  [xs & {:keys [;; init args
                points
                interpolation-type
                ;; Recur args
                interpolate-opts]
         :or {step 1
              x 0
              interpolation-type :cubic-hermite
              interpolate-opts []}}]
  (map-indexed
   (fn [x [y z a b c]]
     (delay
       (apply interp/interpolate
              [[(- x 2) y]
               [(- x 1) z]
               [x       a]
               [(+ x 1) b]
               [(+ x 2) c]]
              interpolation-type
              interpolate-opts)))
   (partition 5 1 (concat (repeat 2 0.0)
                          xs)))
  )


(defn cycle-seq
  "Given a sequence, length and offset, return a seq that cycles forever over
  a portion of the seq."
  [xs & {:keys [length offset]
         :or {offset 0
              length 0}}]
  (->> xs
       (drop offset)
       (take length)
       cycle))

#_(take 10 (cycle-seq (range 10) :length 3)) ;; => (0 1 2 0 1 2 0 1 2 0)

(defn smooth-seq
  [xs & {:keys [n]
         :or {n 2}}]
  (map
   (fn [xs']
     (double
      (/ (reduce + xs')
         (count xs'))))
   (partition n 1 xs)))

(defn interval-seq
  "Return a seq representing the intervals of the input seq"
  [xs]
  (map (fn [[a b]]
         (- b a))
       (partition 2 1 xs)))


#_(interval-seq (range 10)) ;; => (1 1 1 1 1 1 1 1 1)
#_(interval-seq [1 5 7 9]) ;; => (4 2 2)

;; Primitive seq ops that yield other seqs
(defn op-seq
  "Perform actions on one or more seqs"
  [op seqs]
  (assert (<= 1 (count seqs)) "At least one seq is required")
  (apply map op seqs))

(defn sum-seq
  "Add together the values of any number of seqs"
  [& seqs]
  (op-seq + seqs))

(defn invert-seq
  "flip vals in a seq from positive to negative or visa versa"
  [xs]
  (map - xs))

(defn scale-seq
  "Given a seq and a scale, change the number of events to fit the scale"
  [xs scale]
  (assert (and (int? scale)
               (<= 1 scale)))
  (mapcat
   (partial repeat scale)
   xs))





;; Complex (composite) seqs

(defn overlap-seq
  "NOT USED, but instructive...
  Given two seqs a and b, for each period where a > b return
  the T (index) and length of the overlap."
  [a b & {:keys [comp-fn
                 extra-stats]
          :or {comp-fn >
               extra-stats false}}]
  (keep
   (fn [[[t a' b'] & _ :as chunk]]
     (when (comp-fn a' b')
       (merge
        {:t t
         :length (count chunk)}
        (when extra-stats
          (let [a-seq (map #(get % 1) chunk)
                [a-min a-max] (apply (juxt min max) a-seq)
                b-seq (map #(get % 2) chunk)
                [b-min b-max] (apply (juxt min max) b-seq)]
            {:a-seq a-seq
             :a-edges ((juxt first last) a-seq)
             :a-min a-min
             :a-max a-max
             :b-seq b-seq
             :b-edges ((juxt first last) b-seq)
             :b-min b-min
             :b-max b-max
             :min (min a-min b-min)
             :max (max a-max b-max)})))))
   (partition-by
    (fn [[_ a' b']]
      (comp-fn a' b'))
    (map vector
         (range) a b))))
#_(overlap-seq
   [1 2 3 4 5 6 5 4 3 2 1]
   [6 5 4 3 2 1 2 3 4 5 6]) ;; => ({:t 3, :length 5})

(defn take-sample
  "Take a sample of sample-millis from a time series.
  :from denotes the period of xs"
  [xs sample-millis & {:keys [from]
                  :or {from :millis}}]
  (take (quot sample-millis
              (case from
                :millis 1
                :seconds 1000
                :minutes 60000
                :hours 3600000
                :days 86400000))
        xs))

(defn- local-seq-as
  [xs zone as]
  (map (fn [stamp]
         (t/as (t/local-date-time stamp zone)
               as))
       xs))

(defn time-seqs
  "Given a t-zero (simulation start), an upper bound of sample-n milliseconds
  and an optional local timezone, return a map of useful lazy time sequences."
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
        sec-seq (r-partial 1000)
        min-seq (r-partial 60000)
        hour-seq (r-partial 3600000)
        week-seq (r-partial 604800000)
        day-seq (r-partial 86400000)

        ;; secondary/local
        moh-seq (local-seq-as min-seq
                              zone
                              :minute-of-hour)
        mod-seq (local-seq-as min-seq
                              zone
                              :minute-of-day)
        day-night-seq (map (comp
                            incanter/cos
                            #(*  2 Math/PI (/ % 86400000))
                            (partial * 60000))
                           mod-seq)

        hod-seq (local-seq-as hour-seq
                              zone
                              :hour-of-day)


        dow-seq (local-seq-as day-seq
                              zone
                              :day-of-week)

        dom-seq (local-seq-as day-seq
                              zone
                              :day-of-month)
        doy-seq (local-seq-as day-seq
                              zone
                              :day-of-year)]
    {:t-seq t-seq
     :sec-seq sec-seq
     :min-seq min-seq
     :hour-seq hour-seq
     :day-seq day-seq
     :week-seq week-seq
     :moh-seq moh-seq
     :mod-seq mod-seq
     :day-night-seq day-night-seq
     :hod-seq hod-seq
     :dow-seq dow-seq
     :dom-seq dom-seq
     :doy-seq doy-seq
     }))

(comment
  (use '(incanter core stats charts io))

  (time
   (let [sim-seed 42
         ;; Create a master RNG for the sim. This is used only to generate other seeds
         ^Random sim-rng (Random. sim-seed)

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
         mask (op-seq max
                      [group-arma
                       day-night-seq
                       lunch-hour-seq])

         ;; create a seed for Bob's seq
         bob-arma-seed (.nextLong sim-rng)

         ;; create a stochastic seq for bob
         bob-arma (arma-seq
                   (merge common-arma
                          {:seed bob-arma-seed}))

         ;; Bob's activity probability
         bob-prob (op-seq (fn [a b]
                            (double
                             (maths/min-max 0.0 (/ (- a b) 2) 1.0)))
                          [bob-arma mask])
         ;; to keep it deterministic, give bob another seeded RNG to take with him.
         ^Random bob-rng (Random. (.nextLong sim-rng))

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
