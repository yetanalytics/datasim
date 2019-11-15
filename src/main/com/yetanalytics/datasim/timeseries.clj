(ns com.yetanalytics.datasim.timeseries
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [incanter.interpolation :as interp]
            [com.yetanalytics.datasim.clock :as clock]
            [java-time :as t])
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
  "Given two seqs a and b, for each period where a > b return
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
  [xs sample-n & {:keys [in]
                  :or {in :ms}}]
  (take (quot sample-n
              (case in
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
        sec-seq (r-partial 1000)
        min-seq (r-partial 60000)

        moh-seq (local-seq-as min-seq
                              zone
                              :minute-of-hour)
        mod-seq (local-seq-as min-seq
                              zone
                              :minute-of-day)
        day-night-seq (map (comp
                            cos
                            #(*  2 Math/PI (/ % 86400000))
                            (partial * 60000))
                           mod-seq)
        hour-seq (r-partial 3600000)
        hod-seq (local-seq-as hour-seq
                              zone
                              :hour-of-day)

        day-seq (r-partial 86400000)
        dow-seq (local-seq-as day-seq
                              zone
                              :day-of-week)
        week-seq (r-partial 604800000)
        dom-seq (local-seq-as day-seq
                              zone
                              :day-of-month)
        doy-seq (local-seq-as day-seq
                              zone
                              :day-of-year)]
    {:t-seq t-seq
     :sec-seq sec-seq
     :min-seq min-seq
     :moh-seq moh-seq
     :mod-seq mod-seq
     :day-night-seq day-night-seq
     :hour-seq hour-seq
     :hod-seq hod-seq

     :day-seq day-seq
     :dow-seq dow-seq
     :dom-seq dom-seq
     :doy-seq doy-seq
     :week-seq week-seq}))






;; not figuring out how to pass in mean + sd for duration and cooldown yet.

(defn probs->events
  "Given probabilities at a given time regular interval in ms, a mean duration
  and a mean cooldown, generate events with a time + duration"
  [probs & {:keys [delay-seed
                   delay-mean
                   delay-sd
                   delay-min
                   delay-seq

                   duration-seed
                   duration-mean
                   duration-sd
                   duration-min
                   duration-seq

                   cooldown-seed
                   cooldown-mean
                   cooldown-sd
                   cooldown-min
                   cooldown-seq

                   seed
                   rng
                   t]
            :or {;; interval 60000 ;; 1 minute
                 delay-mean 30000 ;; 30 sec
                 delay-sd 25000
                 delay-min 0
                 duration-mean 3600000 ;; 1 hour
                 duration-sd 900000
                 duration-min 60000
                 cooldown-mean 900000 ;; 15 minues
                 cooldown-sd 600000
                 cooldown-min 60000
                 t 0}
            :as opts}]
  (lazy-seq
   (let [^Random rng (or rng
                         (and seed
                              (Random. seed))
                         (Random.))
         delay-seed    (or delay-seed
                           (.nextLong rng))
         duration-seed (or duration-seed
                           (.nextLong rng))
         cooldown-seed (or cooldown-seed
                           (.nextLong rng))
         ;; lag time
         delay-seq     (or delay-seq
                           (map (partial max
                                         delay-min)
                                (rand-seq :val-type :gauss
                                          :gauss-sd delay-sd
                                          :gauss-mean delay-mean
                                          :seed delay-seed
                                          )))
         ;; hang time
         duration-seq (or duration-seq
                          (map (partial max
                                        duration-min)
                               (rand-seq :val-type :gauss
                                         :gauss-sd duration-sd
                                         :gauss-mean duration-mean
                                         :seed duration-seed
                                         )))
         ;; chill time
         cooldown-seq (or cooldown-seq
                          (map (partial max
                                        cooldown-min)
                               (rand-seq :val-type :gauss
                                         :gauss-sd cooldown-sd
                                         :gauss-mean cooldown-mean
                                         :seed cooldown-seed
                                         )))
         ]
     (when-let [[idx start-t] (some
                               (fn [[idx t prob]]
                                 (when (and (< 0.0 prob)
                                            (< (.nextDouble rng) prob))
                                   [idx t]))
                               (map
                                vector
                                (range)
                                (range t Long/MAX_VALUE 60000)
                                probs))]
       (let [start-delay (long (first delay-seq))
             duration (long (first duration-seq))
             cooldown (long (first cooldown-seq))
             drop-n (+ (inc idx)
                       (quot (+ start-delay duration cooldown)
                             60000)
                       (if (rem (+ start-delay duration cooldown)
                                60000)
                         1
                         0))
             ]
         ;; TODO: figure out how to work the tail of the probability
         ;; spike to end the activity?
         (cons [(+ start-t start-delay) ;; x
                duration ;; y
                {:start-min start-t
                 :delay start-delay
                 :duration duration
                 :cooldown cooldown
                 ;; TODO: see if it's safe to use the same RNG
                 :seed (.nextLong rng) ;; unique seed

                 } ;; useful for evt. gen
                ]
               (when-let [rest-probs (seq
                                      (drop drop-n
                                            probs))]
                 (probs->events
                  rest-probs
                  :t (+ t (* drop-n 60000))
                  :delay-seed delay-seed
                  :delay-mean delay-mean
                  :delay-sd delay-sd
                  :delay-min delay-min
                  :delay-seq (rest delay-seq)

                  :duration-seed duration-seed
                  :duration-mean duration-mean
                  :duration-sd duration-sd
                  :duration-min duration-min
                  :duration-seq (rest duration-seq)

                  :cooldown-seed cooldown-seed
                  :cooldown-mean cooldown-mean
                  :cooldown-sd cooldown-sd
                  :cooldown-min cooldown-min
                  :cooldown-seq (rest cooldown-seq)
                  :rng rng
                  ))

               ))))))


#_(probs->events [0.0 0.7 0.0 0.0])



(comment
  (use '(incanter core stats charts io))

  (let [t-zero (System/currentTimeMillis)
        sim-seed 42
        sample-n  (* 1000 60 60 24 14) ;; 1 week in ms ;; => 604800000
        ;; Build sequences
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
                                           :zone (t/zone-id))

        ;; in our model, a timeseries for a given actor is measured against
        ;; a composite timeseries representing abstract challenge/diversity.
        ;; This is a combination of a stochastic series representing
        ;; unpredictable factors that apply to the group, higher is harder.
        ;; this series is max'd with a day night cycle (day is easier, night is
        ;; harder). Think of this combined series as a mask.

        ;; When an actor's series is greater than the challenge, the difference
        ;; between the two is the probability (from 0.0 to 1.0) that an event
        ;; will happen at that time.

        ;; a sequence representing every tick of our simulation (that's 1 ms)

        ;; random stochastic for the group
        group-arma (take-sample
                    (arma-seq {:phi [0.5]
                               :theta []
                               :std 0.25
                               :c 0.0
                               :seed sim-seed})
                    sample-n
                    :in :minutes)

        lunch-hour-seq (take-sample
                        (map
                         (fn [x]
                           (if (<= 720 x 780)
                             1.0
                             -1.0))
                         mod-seq)
                        sample-n
                        :in :minutes)
        ;; ;; form a mask for the group + day-night + lunch
        mask (op-seq max
                     [group-arma
                      day-night-seq
                      lunch-hour-seq])


        bob-arma (take-sample
                  (arma-seq {:phi [0.5]
                             :theta []
                             :std 0.25
                             :c 0.0
                             :seed (+ sim-seed 1)})
                  sample-n
                  :in :minutes)

        bob-prob (op-seq (fn [a b]
                           (double
                            (/ (max
                                (- a b)
                                0.0)
                               2)))
                         [bob-arma mask])
        bob-events (probs->events bob-prob
                                  :seed (+ sim-seed 2)
                                  :t t-zero)
        ]
    ;; inspect
    #_(map (fn [[x y z]]
           [(java.util.Date. x)
            (t/convert-amount y :millis :minutes)
            ])
           bob-events)
    ;; make sure we can't overlap
    #_(some (fn [[[idx0 [x0 y0]] [idx1 [x1 y1]] :as hit]]
            (when (t/before? (java.time.Instant/ofEpochMilli x1)
                             (java.time.Instant/ofEpochMilli (+ x0 y0)))
              [hit (t/convert-amount (- (+ x0 y0)
                                        x1)
                                     :millis :seconds)]))

            (partition 2 1 (map-indexed vector bob-events)))
    ;; graph it
    (view (reduce
           (fn [c [t dur]]
             (add-polygon c
                          [[t 0.5] [(+ t dur) 0.5]
                           [(+ t dur) 0.0] [t 0.0]]))
           (time-series-plot
            min-seq
            bob-prob)
           bob-events))

    )

  )
