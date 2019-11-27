(ns com.yetanalytics.datasim.xapi.statement.matchable
  (:require [clojure.set :as cset]
            [com.yetanalytics.datasim.random :as random]
            [com.yetanalytics.datasim.xapi.statement.helpers :as h]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple helper for normalization prior to fn call
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn any-all-helper
  "normalize coll then return if non-empty."
  [coll]
  (let [normalized (h/normalize-to-vec coll)]
    (when-some [_ (try (seq normalized)
                       (catch Exception e
                         (throw (ex-info "normalization is broken!"
                                         {:coll coll :normalized normalized}
                                         e))))]
      normalized)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Any, All, None fns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-any
  "selection of a value from the `coll` of possible values
   - will return nil if there are no values to select from!"
  [rng coll]
  (when-some [data (any-all-helper coll)]
    (random/rand-nth* rng data)))

(defn handle-all
  "return the first item in `coll` if its the only item, otherwise return all of `coll`
   - will return nil if `coll` is empty"
  [coll]
  (when-some [data (any-all-helper coll)]
    (if (= 1 (count data))
      ;; only care about the single value inside the vector
      (first data)
      ;; return normalized `coll` containing 2 or more items
      data)))

(defn handle-none
  "remove items from `possibilities` that are found in
   `none-coll` and returns the set of remaining items."
  [possibilities none-coll]
  (let [p-set (set possibilities)
        n-set (set none-coll)]
    (if (seq p-set)
      (cset/difference p-set n-set)
      (throw (ex-info "no possibilities were provided!"
                      {:possibilities possibilities
                       :none          none-coll})))))

(comment
  (= #{7 4 5}
     (handle-none [3 4 5 6 7] [1 2 3 6]))
  (= #{4 3 5}
     (handle-none [3 4 5] [])
     (handle-none [3 4 5] nil))
  (= "no possibilities were provided!"
     (try (handle-none [] [1 2 3])
          (catch Exception e (ex-message e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Combo of Any, All, None
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compound-logic
  "returns possible matchable value(s) based on `rule`"
  [rule rng]
  (let [{any-coll  :any
         all-coll  :all
         none-coll :none} rule]
    (if-some [remaining-set (try (handle-none any-coll none-coll)
                                 (catch Exception e nil))]
      (if (seq all-coll)
        ;; find the overlap between (`any` - `none`) and `all`
        (handle-all (cset/intersection remaining-set (set all-coll)))
        ;; pick one using `rng`
        (handle-any rng remaining-set))
      (if (seq all-coll)
        ;; ignore `none` and handle `all`
        (handle-all all-coll)
        ;; return a fn which expects a collection of possible values
        ;; - filtered down to valid values based on `none` and return one of them using `rng`
        (fn [possibilities]
          (->> none-coll
               (handle-none possibilities)
               (handle-any rng)))))))

(comment
  (= 2
     (compound-logic {:any [1 2 3]
                      :all [2]
                      :none [1 3]}
                     (random/seed-rng 123))
     (compound-logic {:any [1 2 3]
                      :all []
                      :none [1]}
                     (random/seed-rng 123))
     (compound-logic {:any [1 2 3]
                      :none [1]}
                     (random/seed-rng 123))
     (compound-logic {:any [2 3]}
                     (random/seed-rng 123))
     (compound-logic {:any [1 2 3 4]
                      :none [1 4]}
                     (random/seed-rng 123)))

  (= [1 2 3 4]
     (compound-logic {:all [1 2 3 4]} (random/seed-rng 123))
     ;; `none` ignored if no `any` but some `all`
     (compound-logic {:all [1 2 3 4] :none [1 2 3 4]} (random/seed-rng 123)))

  ;; filter coll of generated possibilities based on `none` then select using `rng`
  (= 3 ((compound-logic
         {:none [5 6 7 8]}
         (random/seed-rng 123)) [1 2 3 4])))
