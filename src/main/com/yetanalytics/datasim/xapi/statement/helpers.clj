(ns com.yetanalytics.datasim.xapi.statement.helpers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; normalization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn normalization-helper
  "convert non-nil `scalar-or-coll` to vector or return empty vector"
  [case-k scalar-or-coll]
  (if-some [_ (seq scalar-or-coll)]
    (case case-k
      :literal [scalar-or-coll]
      :fn      (vec scalar-or-coll)
      [])
    []))

(defn normalize-to-vec
  "conversion from collection -> vector if not already a vector.
   - scalar values + maps are returned wrapped in vec, ie. this -> [this]"
  [this]
  (if-not (vector? this)
    ;; check for non-nil
    (if-some [scalar-or-coll this]
      ;; maps + strings are special case where we want to check for emptyness
      ;; - don't want to return [{}] or [""]
      ;; - dont want to return ["/f" "/o" "/o"] or [["foo" "baz"] ["another key" "another val"]]
      (cond (or (map? scalar-or-coll) (string? scalar-or-coll))
            (normalization-helper :literal scalar-or-coll)
            ;; ^ returns ["foo"] or [{"foo" "baz" "another key" "another val"}]
            (coll? scalar-or-coll)
            ;; trust in rich
            (normalization-helper :fn scalar-or-coll)
            ;; avoid call to seq which will throw, return wrapped in literal
            :else [scalar-or-coll])
      ;; nil -> []
      [])
    ;; already a vector
    this))

(comment
  (= [{:foo "bar"}] (normalize-to-vec {:foo "bar"}))
  (= [true] (normalize-to-vec true))
  (= [false] (normalize-to-vec false))
  (= []
     (normalize-to-vec nil)
     (normalize-to-vec [])
     (normalize-to-vec (list))
     (normalize-to-vec {})
     (normalize-to-vec #{})
     (normalize-to-vec ""))
  (= [1 2 3]
     (normalize-to-vec (list 1 2 3))
     (normalize-to-vec [1 2 3])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove items from start of vector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn butfirst
  "returns subvec of everything except for the first item in `coll`
   - `coll` will be normalized to a vector if not already one.
   -- see `normalize-to-vec` for logic"
  [coll]
  (let [normalized (normalize-to-vec coll)]
    (if (seq normalized) (subvec normalized 1) normalized)))

(comment
  (= []
     (butfirst [])
     (butfirst ["a"])
     (butfirst {:foo "baz"})
     (butfirst {:foo "baz" :buz "bar"})
     (butfirst (list "a"))
     (butfirst "a")
     (butfirst true)
     (butfirst 1.0))
  (= ["b" "c"]
     (butfirst ["a" "b" "c"])
     (butfirst (list "a" "b" "c"))
     (butfirst (set ["a" "b" "c"]))
     (butfirst (for [each ["a" "b" "c"]] each)))
  (= [{:foo "baz"}]
     (butfirst [{"I'm" "removed"} {:foo "baz"}])))

(defn next-to-last
  "returns the second from last value within `coll`"
  [coll]
  (-> coll normalize-to-vec pop peek))

(comment
  (= 2 (next-to-last [1 2 3]))
  (= 1 (next-to-last [1 2]))
  (= nil (next-to-last [1])))

(defn replace-last
  [coll replacement]
  (-> coll normalize-to-vec pop (conj replacement)))

(comment
  (= (replace-last [1 2 3] 4) [1 2 4]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup from IRI map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn iri-lookup-attempt
  "query `iri-map` for more data about `maybe-iri`, defaults to `maybe-iri` + `fallback` if not found."
  [{:keys [maybe-iri iri-map]}]
  (get iri-map maybe-iri {:non-iri maybe-iri}))

(comment
  (= "bar"
     (iri-lookup-attempt
      {:maybe-iri "foo"
       :iri-map   {"foo" "bar"}}))
  (= {:non-iri "foo"}
     (iri-lookup-attempt
      {:maybe-iri "foo"
       :iri-map   {:foo "bar"}})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful preds for Vector and Map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn containsv?
  [coll v]
  (not= -1 (.indexOf coll v)))

(defn contains-many?
  [m & ks]
  (every? #(contains? m %) ks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quick testing fn for ensuring determinism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn test-n-times
  "super simple run test n times and ensure all values follow same pred"
  [n fn pred-fn]
  (every? pred-fn (repeatedly n fn)))
