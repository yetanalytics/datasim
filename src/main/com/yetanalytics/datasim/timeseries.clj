(ns com.yetanalytics.datasim.timeseries
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [incanter.interpolation :as interp]
            [com.yetanalytics.datasim.clock :as clock])
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
             val-type]
      :or {val-type :long}}]
  (lazy-seq
   (let [^Random rng (or rng
                         (and seed
                              (Random. seed))
                         (Random.))]
     (cons (case val-type
             :long (.nextLong rng)
             :gauss (.nextGaussian rng)
             :double (.nextDouble rng))
           (rand-seq :rng rng
                     :val-type val-type)))))

#_(take 10 (rand-seq :val-type :gauss :seed 42)) ;; => (1.1419053154730547 0.9194079489827879 -0.9498666368908959 -1.1069902863993377 0.2809776380727795 0.6846227956326554 -0.8172214073987268 -1.3966434026780434 -0.19094451307087512 1.4862133923906502)

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


(comment

  (require '[incanter.charts :as c])
  (use 'incanter.core
       )
  (let [sim-seed 42
        sample-n (* 60 24 7) ;; 1 week
        ;; Build sequences

        ;; in our model, a timeseries for a given actor is measured against
        ;; a composite timeseries representing abstract challenge/diversity.
        ;; This is a combination of a stochastic series representing
        ;; unpredictable factors that apply to the group, higher is harder.
        ;; this series is max'd with a day night cycle (day is easier, night is
        ;; harder). Think of this combined series as a mask.

        ;; When an actor's series is greater than the challenge, the difference
        ;; between the two is the probability (from 0.0 to 1.0) that an event
        ;; will happen at that time.

        ;; Day-night is a 24 hour cycle from 1.0 (midnight) to -1.0 (midday)
        day-night (-> (interpolate-seq
                       :points
                       (take 5
                             (map vector
                                  (iterate (partial + 720) 0)
                                  (cycle [-1.0 1.0]))))
                      (cycle-seq
                       ;; drop a day so we get a nice loop
                       :length (* 24 60)
                       :offset (* 12 60)))
        ;; random stochastic for the group
        group-arma (arma-seq {:phi [0.5]
                              :theta []
                              :std 0.25
                              :c 0.0
                              :seed sim-seed})

        ;; ;; form a mask for the group + day-night
        mask (op-seq max
                     [group-arma
                      day-night])


        bob-arma (arma-seq {:phi [0.5]
                            :theta []
                            :std 0.25
                            :c 0.0
                            :seed (+ sim-seed 1)})

        bob-prob (op-seq (fn [a b]
                           (double
                            (/ (max
                                (- a b)
                                0.0)
                               2)))
                         [bob-arma mask])
        ;; we check at pseudorandom intervals, so generate those
        ;; let's say lambda is 10 minutes
        bob-triggers (distinct
                      (reductions
                       (fn [t r]
                         (+ t
                            (long (* r 10))))
                       0
                       (rand-seq :val-type :double
                                 :seed (+ sim-seed 2))))

        bob-events (for [[t r] (map vector
                                    bob-triggers
                                    (rand-seq :val-type :double
                                              :seed (+ sim-seed 3)))
                         :while (< t sample-n)
                         :let [prob (nth bob-prob t)]
                         :when (< r prob)]
                     [t 1.0])
        t-fn #(clock/sim-instant
               %
               :step :minutes
               :as :long)

        xs (map t-fn
                (range sample-n))
        ]

    (view (c/add-points
           (c/time-series-plot xs (take sample-n bob-prob))
           (map (comp t-fn first) bob-events)
           (map second bob-events)))

    )

  )
