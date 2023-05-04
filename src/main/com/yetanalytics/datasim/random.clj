(ns com.yetanalytics.datasim.random
  "Seeded random number generation functions."
  (:require [com.yetanalytics.datasim.util.maths :as maths]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen])
  (:import [java.util UUID Random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::seed int?)

(s/def ::rng
  (s/with-gen #(instance? Random %)
    (fn []
      (sgen/fmap (fn [s]
                   (Random. s))
                 (s/gen ::seed)))))

(s/def ::sd
  (s/double-in :min 0.0 :infinite? false :NaN? false))

(s/def ::weight
  (s/double-in -1.0 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef seed-rng
  :args (s/cat :seed ::seed)
  :ret ::rng)

(defn seed-rng
  "Return a random number generator that is seeded using `seed`. The RNG can
   then be used in the other functions in this namespace."
  ^Random [^Long seed]
  (Random. seed))

(s/fdef rand*
  :args (s/cat :rng ::rng
               :n (s/? number?))
  :ret double?)

(defn rand*
  "Generate and return a uniformly distributed double value between 0 and `n`
   (or between 0 and 1 if `n` is not provided)."
  (^Double [^Random rng]
   (.nextDouble rng))
  (^Double [^Random rng n]
   (* n (rand* rng))))

(s/fdef rand-int*
  :args (s/cat :rng ::rng
               :n   (s/int-in Integer/MIN_VALUE Integer/MAX_VALUE))
  :ret number?)

(defn rand-int*
  "Generate and return a uniformly distributed integer value between 0
   (inclusive) and `n` (exclusive)."
  [rng n]
  (int (rand* rng n)))

(s/fdef rand-nth*
  :args (s/cat :rng  ::rng
               :coll (s/every any? :min-count 1))
  :ret any?)

(defn rand-nth*
  "Select and return a random element from `coll` along a uniform probability
   distribution."
  [rng coll]
  (nth coll (rand-int* rng (count coll))))

(s/fdef shuffle*
  :args (s/cat :rng  ::rng
               :coll (s/every any?)
               ;; FIXME: Currently disabled due to below bug
               ;; :n    (s/? nat-int?)
               )
  :ret #(instance? clojure.lang.LazySeq %)
  :fn (fn [{{:keys [coll]} :args ret :ret}]
        (= (set coll)
           (set ret))))

;; FIXME: If `n` is greater than `(count coll)`, then all elements after `n`
;; will be `nil`s.
;; TODO: Describe probability distribution characteristics, which is not
;; trivial given how shuffling is done (repeatedly choosing the split point
;; on a uniform distribution).
(defn shuffle*
  "Shuffle the contents of `coll` and return the first `n` elements, or all
   elements if `n` is not provided, as a lazy seq."
  ([rng coll]
   (shuffle* rng coll (count coll)))
  ([rng coll n]
   (lazy-seq
    (when (< 0 n)
      (let [[head [x & tail]] (split-at (rand-int* rng n) coll)]
        (cons x (shuffle* rng (concat head tail) (dec n))))))))

(s/fdef random-sample*
  :args (s/cat :rng     ::rng
               :prob    (s/double-in :min 0.0 :max 1.0 :NaN? false)
               :coll    (s/every any? :min-count 1)
               :weights (s/? (s/map-of any? double?)))
  :ret #(instance? clojure.lang.LazySeq %))

(defn random-sample*
  "Perform a random sampling on `coll`, in which each element has `prob`
   probability of being chosen. If a `weights` map where each `coll` element
   is mapped to a weight is provided, then the weight will be added to
   the overall `prob` (for a max element-specific probability of 1). Returns
   a lazy seq of the sampled elements. If `coll` is not provided, returns a
   transducer."
  ([rng prob]
   (filter (fn [_] (< (rand* rng) prob))))
  ([rng prob coll]
   (filter (fn [_] (< (rand* rng) prob)) coll))
  ([rng prob coll weights]
   (filter (fn [el]
             (< (rand* rng)
                (maths/min-max 0.0
                               (+ prob (get weights el 0.0))
                               1.0)))
           coll)))

(s/fdef rand-gauss
  :args (s/cat :rng  ::rng
               :mean double?
               :sd   double?)
  :ret double?)

(defn rand-gauss
  "Return a Gaussian distributed double value, where the Gaussian distribution
   has mean `mean` and standard deviation `sd`."
  [^Random rng mean sd]
  (+ mean
     (* sd (.nextGaussian rng))))

(s/fdef rand-long
  :args (s/cat :rng ::rng)
  :ret int?)

(defn rand-long
  "Return a uniformly distributed long value (note that longs are the default
   representation of Clojure integer values)."
  [^Random rng]
  (.nextLong rng))

(s/fdef rand-uuid
  :args (s/cat :rng ::rng)
  :ret string?)

(defn rand-uuid
  "Return a random UUID as a string.
   Derived from `clojure.test.check.generators/uuid`"
  [^Random rng]
  (let [x1 (-> (rand-long rng)
               (bit-and -45057)
               (bit-or 0x4000))
        x2 (-> (rand-long rng)
               (bit-or -9223372036854775808)
               (bit-and -4611686018427387905))]
    (.toString (UUID. x1 x2))))

(s/fdef choose
  :args (s/cat :rng     ::rng
               :weights (s/map-of any? (s/keys :req-un [::weight]))
               :coll    (s/every any? :min-count 1)
               :options (s/keys* :opt-un [::sd])))

(defn choose
  "Choose and return an element from `coll`. Each element is mapped to a
   normally distributed random value, with the mean
   ```clojure
   (->> coll count (/ 1) (+ (* sd weight)))
   ```
   and standard deviation `sd` (default `0.25`).

   A given element is chosen if that element's random value is greater than all
   the others. In the case where every element has the default weight of 0, the
   probability a given element is chosen is `1/n` for `n` elements.

   `weights` should be a mapping from each element to a map with a `:weights`
   key, whose value is the element weight. If an element is not provided
   with a weight, it defaults to `0.0`.

   If any element's `weights` is `<= -1`, then the probability that it
   is chosen as the element goes to 0 (unless every element's weight is
   `<= 1`, in which case the last element in `coll` is chosen)."
  [rng weights coll & {:keys [sd]
                       :or   {sd 0.25}}]
  (let [even-odds (/ 1 (count coll))
        elem-val  (fn [el]
                    (let [weight (get-in weights [el :weight] 0.0)
                          mean   (+ even-odds (* sd weight))]
                      (if (<= weight -1.0)
                        -1.0
                        (rand-gauss rng mean sd))))]
    (apply max-key elem-val coll)))

(comment
  (def rng (seed-rng 123))

  (choose rng {} [:a :b :c]))

(comment

  #_(use '(incanter core stats charts io))

  (let [rng (seed-rng 42)
        runs 10000]

    (reduce-kv
     (fn [m k v]
       (assoc m k (double (/ v runs))))
     {}
     (frequencies
      (repeatedly runs
                  #(choose rng
                           {:a 1.0 :b -1.0}
                           [:a :b :c :d :e :f])))))



  (let [rng (seed-rng 42)
        runs 1000]
    (apply max
           (repeatedly runs
            #(rand-gauss
              rng
              (/ 1 1000)
              (/ 1 1000)))))



  #_(let [rng (seed-rng 42)
        v [0 1 2 3 4]
        vc (count v)
        runs (int (Math/pow vc vc))
        ret
        (frequencies
         (repeatedly runs
                     #(shuffle* rng
                       v
                       )))]
    (view (histogram (vals ret)))
    )
  )
