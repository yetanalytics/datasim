(ns com.yetanalytics.datasim.random
  "Seeded random functions"
  (:require [com.yetanalytics.datasim.util.maths :as maths]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen])
  (:import [java.util
            UUID Random]))

(s/def ::seed int?)

(s/def ::rng
  (s/with-gen #(instance? Random %)
    (fn []
      (sgen/fmap (fn [s]
                   (Random. s))
                 (s/gen ::seed)))))

(s/fdef seed-rng
  :args (s/cat :seed ::seed)
  :ret ::rng)

(defn seed-rng
  ^Random [^Long seed]
  (Random. seed))

(s/fdef rand*
  :args (s/cat :rng ::rng
               :n (s/? number?))
  :ret double?)

(defn rand*
  (^Double [^Random rng]
   (.nextDouble rng))
  (^Double [^Random rng n]
   (* n (rand* rng))))

(s/fdef rand-int*
  :args (s/cat :rng ::rng
               :n (s/int-in Integer/MIN_VALUE Integer/MAX_VALUE))
  :ret number?)

(defn rand-int*
  [rng n]
  (int (rand* rng n)))

(s/fdef rand-nth*
  :args (s/cat :rng ::rng
               :coll (s/every any?
                              :min-count 1))
  :ret any?)

(defn rand-nth*
  [rng coll]
  (nth coll (rand-int* rng (count coll))))

(s/fdef shuffle*
  :args (s/cat :rng ::rng
               :coll (s/every any?))
  :ret coll?
  :fn (fn [{{:keys [rng coll cnt]} :args ret :ret}]
        (= (set coll)
           (set ret))))

(defn shuffle*
  ([rng coll]
   (shuffle* rng coll (count coll)))
  ([rng coll cnt]
   (lazy-seq
    (when (< 0 cnt)
      (let [[head [x & tail]] (split-at
                               (rand-int*
                                rng
                                cnt)
                               coll)]
        (cons x
              (shuffle*
               rng
               (concat head tail)
               (dec cnt))))))))


(s/fdef random-sample*
  :args (s/cat :rng ::rng
               :prob (s/double-in :min 0.0 :max 1.0 :NaN? false)
               :coll (s/every any? :min-count 1)
               :weights (s/?
                         (s/map-of any? double?)))
  :ret coll?)

(defn random-sample*
  ([rng prob]
   (filter (fn [_] (< (rand* rng) prob))))
  ([rng prob coll]
   (filter (fn [_] (< (rand* rng) prob)) coll))
  ([rng prob coll weights]
   (filter (fn [el]
             (< (rand* rng)
                (maths/min-max 0.0
                               (+ prob
                                  (get weights el 0.0))
                               1.0)))
           coll)))

(s/fdef rand-gauss
  :args (s/cat :rng ::rng
               :mean double?
               :sd double?)
  :ret double?)

(defn rand-gauss
  [^Random rng mean sd]
  (+ mean
     (* sd (.nextGaussian rng))))

(s/fdef rand-long
  :args (s/cat :rng ::rng)
  :ret int?)

(defn rand-long
  [^Random rng]
  (.nextLong rng))

(s/fdef rand-uuid
  :args (s/cat :rng ::rng)
  :ret string?)

(defn rand-uuid
  "Produce a random uuid (as a string) for the rng.
  Derived from `clojure.test.check.generators/uuid`"
  [^Random rng]
  (let [x1 (-> (rand-long rng)
               (bit-and -45057)
               (bit-or 0x4000))
        x2 (-> (rand-long rng)
               (bit-or -9223372036854775808)
               (bit-and -4611686018427387905))]
    (.toString (UUID. x1 x2))))


(s/def ::sd
  (s/double-in :min 0.0 :infinite? false :NaN? false))

(s/fdef choose
  :args (s/cat :rng ::rng
               :weights (s/map-of any? (s/double-in -1.0 1.0))
               :coll (s/every any? :min-count 1)
               :options (s/keys*
                         :opt-un [::sd])))

(defn choose
  [rng weights coll
   & {:keys [sd]
      :or {sd 0.25}}]
  (let [even-odds (/ 1 (count coll))
        debug (clojure.pprint/pprint [rng weights coll])]
    (apply max-key
           (fn [el]
             (rand-gauss
              rng
              (+ even-odds (* sd (get-in weights [el :weight] 0.0)))
              sd))
           coll)))


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
