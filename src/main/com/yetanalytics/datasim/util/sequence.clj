(ns com.yetanalytics.datasim.util.sequence)

(defn seq-sort
  "Given a key-fn (that must return a number) and a vector of seqs, return the
  items from all seqs ordered by that, ascending.
  If incoming seqs are monotonic, the output is as well."
  [key-fn seqs]
  (lazy-seq
   (when-let [seqs' (->> seqs (keep not-empty) (into []) not-empty)]
     (let [[idx [head & rest-seq]]
           (apply min-key
                  (comp key-fn first second)
                  (map-indexed vector seqs'))]
       (->> rest-seq (assoc seqs' idx) (seq-sort key-fn) (cons head))))))

;; https://clojuredocs.org/clojure.core/chunk#example-5c9cebc3e4b0ca44402ef6ec
(defn re-chunk
  "takes a sequence (already chunked or not)
  and produces another sequence with different chunking size."
  [n xs]
  (lazy-seq
   (when-let [s (seq (take n xs))]
     (let [cb (chunk-buffer n)]
       (doseq [x s] (chunk-append cb x))
       (chunk-cons (chunk cb) (re-chunk n (drop n xs)))))))

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
