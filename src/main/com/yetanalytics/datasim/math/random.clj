(ns com.yetanalytics.datasim.math.random
  "Random number generation and probabilistic operations."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [com.yetanalytics.datasim.util.maths :as maths])
  (:refer-clojure :exclude [rand rand-int])
  (:import [java.util UUID Random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::seed int?)

(s/def ::rng
  (s/with-gen #(instance? Random %)
    (fn [] (sgen/fmap (fn [s] (Random. s)) (s/gen ::seed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RNG Creation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef seed-rng
  :args (s/cat)
  :ret ::rng)

(defn rng
  "Create a pseudorandom RNG using an arbitrary seed value."
  ^Random []
  (Random.))

(s/fdef seed-rng
  :args (s/cat :seed ::seed)
  :ret ::rng)

(defn seed-rng
  "Create a seeded deterministic, pseudorandom RNG, in which two RNGs created
   using the same value of `seed` will output the same results."
  ^Random [^Long seed]
  (Random. seed))

;; Random Number Generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef rand
  :args (s/cat :rng ::rng
               :n (s/? number?))
  :ret double?)

(defn rand
  "Generate a pseudorando, uniformly distributed double value between 0 and
   `n` (both inclusive).
   
   See also: `clojure.core/rand`"
  (^Double [^Random rng]
   (.nextDouble rng))
  (^Double [^Random rng n]
   (* n (rand rng))))

(s/fdef rand-int
  :args (s/cat :rng ::rng
               :n (s/int-in Integer/MIN_VALUE Integer/MAX_VALUE))
  :ret int?)

(defn rand-int
  "Generate a pseudorandom, uniformly distributed integer value between 0
   (inclusive) and `n` (exclusive).
   
   See also: `clojure.core/rand-int`"
  [rng n]
  (long (rand rng n)))

(s/fdef rand-unbound-int
  :args (s/cat :rng ::rng)
  :ret int?)

(defn rand-unbound-int
  "Generate a pseudorandom, uniformly distributed integer value anywhere
   between `Integer/MIN_VALUE` and `Integer/MAX_VALUE`."
  [^Random rng]
  (.nextLong rng))

(s/fdef rand-gaussian
  :args (s/cat :rng ::rng
               :mean double?
               :sd double?)
  :ret double?)

(defn rand-gaussian
  "Generate a pseudorandom, normally distributed double value with mean
   `mean` and standard deviation `sd`."
  [^Random rng mean sd]
  (+ mean (* sd (.nextGaussian rng))))

;; Technically a UUID is a number, right?

(s/fdef rand-uuid
  :args (s/cat :rng ::rng)
  :ret string?)

(defn rand-uuid
  "Generate a pseudorandom UUID (as a string)."
  [^Random rng]
  ;; Derived from `clojure.test.check.generators/uuid`
  (let [x1 (-> (rand-unbound-int rng)
               (bit-and -45057)
               (bit-or 0x4000))
        x2 (-> (rand-unbound-int rng)
               (bit-or -9223372036854775808)
               (bit-and -4611686018427387905))]
    (.toString (UUID. x1 x2))))

;; Collection Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- validate-not-empty
  [coll]
  (when (empty? coll)
    (throw (ex-info "Attempted to select elements from an empty collection!"
                    {:type ::empty-coll}))))

(s/fdef rand-nth*
  :args (s/cat :rng  ::rng
               :coll (s/every any? :min-count 1))
  :ret any?)

(defn rand-nth*
  "Randomly select an element from `coll`. Each element has an equal
   probability of being selected.
   
   Will throw an `::empty-coll` exception on an empty `coll`."
  [rng coll]
  (validate-not-empty coll)
  (nth coll (rand-int rng (count coll))))

(s/fdef shuffle*
  :args (s/cat :rng  ::rng
               :coll (s/every any?))
  :ret coll?
  :fn (fn [{{:keys [coll]} :args ret :ret}]
        (= (set coll) (set ret))))

(defn shuffle*
  "Randomly shuffle `coll` and return a lazy sequence as the result."
  ([rng coll]
   (shuffle* rng coll (count coll)))
  ([rng coll cnt]
   (lazy-seq
    (when (< 0 cnt)
      (let [[head [x & tail]] (split-at (rand-int rng cnt) coll)]
        (cons x
              (shuffle* rng (concat head tail) (dec cnt))))))))

(s/fdef random-sample*
  :args (s/cat :rng     ::rng
               :prob    (s/double-in :min 0.0 :max 1.0)
               :coll    (s/every any? :min-count 1)
               :weights (s/? (s/map-of any? double?)))
  :ret coll?)

(defn random-sample*
  "Probabilistically sample elements from `coll`, where each element has
   `prob` probability of being selected. If `weights` are provided, then
   the element associated with a weight has `(+ prob weight)` probability
   (up to 1.0) of being selected. Returns a transducer when `coll` is not
   provided."
  ([rng prob]
   (filter (fn [_] (< (rand rng) prob))))
  ([rng prob coll]
   (filter (fn [_] (< (rand rng) prob)) coll))
  ([rng prob coll weights]
   (filter (fn [x]
             (< (rand rng)
                (maths/bound-probability (+ prob (get weights x 0.0)))))
           coll)))

(s/def ::sd
  (s/double-in :min 0.0 :infinite? false :NaN? false))

(s/def ::weight
  (s/double-in -1.0 1.0))

(s/fdef choose
  :args (s/cat :rng ::rng
               :weights (s/map-of any? (s/keys :req-un [::weight]))
               :coll (s/every any? :min-count 1)
               :options (s/keys* :opt-un [::sd])))

(defn choose
  "Probabilistically select one element from `coll`. The `weights` map
   should be a map of `coll` elements to map with a `:weight` value."
  [rng weights coll & {:keys [sd] :or {sd 0.25}}]
  (validate-not-empty coll)
  (let [even-odds (/ 1 (count coll))]
    (apply max-key
           (fn [el]
             (let [weight (get-in weights [el :weight] 0.0)]
               (if (<= weight -1.0)
                 -1.0
                 (rand-gaussian
                  rng
                  (+ even-odds (* sd weight))
                  sd))))
           coll)))

(comment
  (def the-rng (seed-rng 100))

  (rand-nth* the-rng [])
  
  (shuffle* the-rng [1 2 3])
  (choose rng {} []))

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
            #(rand-gaussian
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
