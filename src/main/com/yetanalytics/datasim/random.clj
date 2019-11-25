(ns com.yetanalytics.datasim.random
  "Seeded random functions"
  (:require [com.yetanalytics.datasim.util.maths :as maths])
  (:import [java.util Random]))

(defn seed-rng
  ^Random [^Long seed]
  (Random. seed))

(defn rand*
  (^Double [^Random rng]
   (.nextDouble rng))
  (^Double [^Random rng n]
   (* n (rand* rng))))

(defn rand-int*
  [rng n]
  (int (rand* rng n)))

(defn rand-nth*
  [rng coll]
  (nth coll (rand-int* rng (count coll))))

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


(defn random-sample*
  ([rng prob]
   (filter (fn [_] (< (rand rng) prob))))
  ([rng prob coll]
   (filter (fn [_] (< (rand rng) prob)) coll))
  ([rng prob coll weights]
   (filter (fn [el]
             (< (rand rng)
                (maths/min-max 0.0
                               (+ prob
                                  (get weights el 0.0))
                               1.0)))
           coll)))

(defn rand-gauss
  [^Random rng mean sd]
  (+ mean
     (* sd (.nextGaussian rng))))

(defn rand-long
  [^Random rng mean sd]
  (.nextLong rng))

(defn choose
  [rng weights coll
   & {:keys [sd]
      :or {sd 0.25}}]
  (let [even-odds (/ 1 (count coll))]
    (apply max-key
           (fn [el]
             (rand-gauss
              rng
              (+ even-odds (* sd (get weights el 0.0)))
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