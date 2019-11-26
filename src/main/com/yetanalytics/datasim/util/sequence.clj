(ns com.yetanalytics.datasim.util.sequence)

(defn seq-sort
  "Given a key-fn (that must return a number) and a vector of seqs, return the
  items from all seqs ordered by that, ascending.
  If incoming seqs are monotonic, the output is as well."
  [key-fn seqs]
  (lazy-seq
   (when-let [seqs' (not-empty
                     (into []
                           (keep not-empty
                                 seqs)))]
     (let [[idx [head & rest-seq]]
           (apply min-key (comp key-fn
                                first
                                second)
                  (map-indexed vector
                               seqs'))]
       (cons head
             (seq-sort
              key-fn
              (assoc seqs' idx rest-seq)))))))

(comment
  (require '[com.yetanalytics.datasim.random :as r])

  (defn rand-monotonic-seq
    [seed & [start]]
    (let [rng (r/seed-rng seed)]
      (rest (iterate #(+ % (r/rand* rng)) (or start 0)))))

  (let [ss (seq-sort
            identity
            (into []
                  (for [n (range 20)]
                    (take 10 (rand-monotonic-seq n)))))]
    (= (sort ss) ss)) ;; => true
)
