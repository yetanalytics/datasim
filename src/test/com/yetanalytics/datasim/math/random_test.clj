(ns com.yetanalytics.datasim.math.random-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.math :as math]
            [clojure.spec.test.alpha :as stest]
            [com.yetanalytics.datasim.math.random :as r]))

(deftest random-functions-test
  (testing "Generative tests"
    (let [results (stest/check
                   `#{r/rng
                      r/seed-rng
                      r/rand
                      r/rand-int
                      r/rand-unbound-int
                      r/rand-gaussian
                      r/rand-boolean
                      r/rand-exp
                      r/rand-uuid
                      r/rand-nth
                      r/shuffle
                      r/random-sample
                      r/choose
                      r/choose-map})
          {:keys [total
                  check-passed]} (stest/summarize-results results)]
      (is (= total check-passed)))))

(def choose-total 4000)

(def choose-coll [:foo :bar :baz :qux])

(def choose-map {:foo :FOO :bar :BAR :baz :BAZ :qux :QUX})

;; 4 standard deviations means that the bounds will only be exceeded in
;; 1 in 15,787 runs.
;; See: https://en.wikipedia.org/wiki/68–95–99.7_rule##Table_of_numerical_values
(def sd-limit 4)

;; The means and standard deviations are derived from interpreting the
;; event of choosing a particular kewyord as a binomial distribution.
;; See: https://en.wikipedia.org/wiki/Binomial_distribution
(defmacro test-equal-choose [weights]
  (let [prob#     0.25
        mean#     (* choose-total prob#)
        sd#       (math/sqrt (* choose-total prob# (- 1 prob#)))
        lo-bound# (- mean# (* sd-limit sd#))
        hi-bound# (+ mean# (* sd-limit sd#))]
    `(let [~'the-rng   (r/rng)
           ~'choose-fn (fn [] (r/choose ~'the-rng ~weights ~choose-coll))
           ~'results   (repeatedly ~choose-total ~'choose-fn)
           ~'freq-map  (frequencies ~'results)]
       (is (< ~lo-bound# (:foo ~'freq-map) ~hi-bound#))
       (is (< ~lo-bound# (:bar ~'freq-map) ~hi-bound#))
       (is (< ~lo-bound# (:baz ~'freq-map) ~hi-bound#))
       (is (< ~lo-bound# (:qux ~'freq-map) ~hi-bound#)))))

(deftest choose-test
  (testing "choose function: equal weights"
    (test-equal-choose {})
    (test-equal-choose {:unrelated-key-1 0.2 :unrelated-key 0.8})
    (test-equal-choose {:foo 0.5 :baz 0.5})
    (test-equal-choose {:foo 0.0 :bar 0.0 :baz 0.0 :qux 0.0})
    (test-equal-choose {:foo 0.2 :bar 0.2 :baz 0.2 :qux 0.2})
    (test-equal-choose {:foo 0.8 :bar 0.8 :baz 0.8 :qux 0.8})
    (test-equal-choose {:foo 1.0 :bar 1.0 :baz 1.0 :qux 1.0}))
  (testing "choose function: only one non-zero weight"
    (let [weights   {:foo 0 :bar 0 :baz 1 :qux 0}
          the-rng   (r/rng)
          choose-fn (fn [] (r/choose the-rng weights choose-coll))
          results   (repeatedly choose-total choose-fn)
          freq-map  (frequencies results)]
      (is (= choose-total (:baz freq-map)))
      (is (nil? (:foo freq-map)))
      (is (nil? (:bar freq-map)))
      (is (nil? (:qux freq-map)))))
  ;; The probabilities are derived from integrating over the area defined
  ;; by the joint uniform distributions where (< x2 (max x1 t2)), where
  ;; x1 and x2 are the independent (but not identical!) random variables
  ;; and t1 and t2 are the upper bounds of the uniform distributions.
  ;; In Clojure pseudocode:
  ;; (-> (/ 1 (* t1 t2))
  ;;     (integrate 0 (max x1 t2) dx2)
  ;;     (integrate 0 t1 dx1))
  ;; = (- 1 (/ t2 (* 2 t1))
  (testing "choose function: two different non-zero weights"
    (let [weights   {:foo 0 :bar 0 :baz 1 :qux 0.4}
          prob-1    0.8
          prob-2    0.2
          mean-1    (* choose-total prob-1)
          mean-2    (* choose-total prob-2)
          sd        (math/sqrt (* choose-total prob-1 prob-2))
          lo-1      (- mean-1 (* sd-limit sd))
          lo-2      (- mean-2 (* sd-limit sd))
          hi-1      (+ mean-1 (* sd-limit sd))
          hi-2      (+ mean-2 (* sd-limit sd))
          the-rng   (r/rng)
          choose-fn (fn [] (r/choose the-rng weights choose-coll))
          results   (repeatedly choose-total choose-fn)
          freq-map  (frequencies results)]
      (is (< lo-1 (:baz freq-map) hi-1))
      (is (< lo-2 (:qux freq-map) hi-2))
      (is (nil? (:foo freq-map)))
      (is (nil? (:bar freq-map)))))
  (testing "choose-map function works just like choose"
    (let [weights       {:foo 0.2 :bar 0.4 :baz 0.6 :qux 0.8}
          seed          (r/rand-unbound-int (r/rng))
          rng-1         (r/seed-rng seed)
          rng-2         (r/seed-rng seed)
          choose-fn     (fn [] (r/choose rng-1 weights choose-coll))
          choose-map-fn (fn [] (r/choose-map rng-2 weights choose-map))
          results-1     (repeatedly choose-total choose-fn)
          results-2     (repeatedly choose-total choose-map-fn)
          freq-map-1    (frequencies results-1)
          freq-map-2    (frequencies results-2)]
      (is (= (:foo freq-map-1)
             (:FOO freq-map-2)))
      (is (= (:bar freq-map-1)
             (:BAR freq-map-2)))
      (is (= (:baz freq-map-1)
             (:BAZ freq-map-2)))
      (is (= (:qux freq-map-1)
             (:QUX freq-map-2))))))
