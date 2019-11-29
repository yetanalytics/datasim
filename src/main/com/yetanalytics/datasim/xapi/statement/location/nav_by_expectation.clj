(ns com.yetanalytics.datasim.xapi.statement.location.nav-by-expectation
  (:require [com.yetanalytics.datasim.xapi.statement.helpers :as h]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dealing with :placeholder or an expectation of :placeholder
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-placeholder
  "identification of `:placeholder` which is used as a stand in
   for some array within a JSON Path string"
  [next-k-in-ks]
  (or (= next-k-in-ks :placeholder)
      (= next-k-in-ks "*")
      (string? next-k-in-ks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple expectation checker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn path-value-check
  "test to determine if the stmt path progresses as expected
   - compares the second to last value of `stmt-path` to `k`"
  [k stmt-path]
  (let [second-last (h/next-to-last stmt-path)
        the-test    (case k
                      :placeholder (handle-placeholder second-last)
                      (= k second-last))]
    {:test-result the-test
     :poped       (pop stmt-path)}))

(comment
  (= {:test-result true :poped ["verb"]}
     (path-value-check "verb" ["verb" "id"]))
  (= {:test-result false :poped ["verb"]}
     (path-value-check "Verb" ["verb" "id"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compound expectation checker with opts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn step-back-expected?
  "Advanced version of `path-value-check`, can return `poped` but defaults to returning `pred`
  
    - return of `poped` requires
      1) `return-poped?` is true
      2) `pred` is true
  
    - support for additional forms of testing expectations about `stmt-path`
      a) what should the rest of the stmt-path ks look like precisely
         - `expected-pop`
      b) what should the next key be exactly?
         - `next-key`"
  [first-key stmt-path & {:keys [next-key expected-pop return-poped?]}]
  (let [{poped       :poped
         test-result :test-result} (path-value-check first-key stmt-path)
        pred (if (or next-key expected-pop)
               (and test-result
                    (or (= expected-pop poped)
                        ;; `:placeholder` is standin for [...] in json path
                        (:test-result (path-value-check next-key poped) false)))
               test-result)]
    (if return-poped? (when pred poped) pred)))

(comment
  ;; return data when its expected
  (= ["baz"]
     (step-back-expected?
      "baz" ["baz" "foo"]
      :return-poped? true
      :expected-pop ["baz"]))
  (= ["bar" "baz"]
     (step-back-expected?
      "baz" ["bar" "baz" "foo"]
      :next-key "bar"
      :return-poped? true))
  ;; data only returned when the expectation was accurate
  (= nil
     (step-back-expected?
      "qux" ["baz" "foo"]
      :return-poped? true
      :expected-pop ["baz"])
     (step-back-expected?
      "qux" ["baz" "foo"]
      :return-poped? true
      :next-key "baz")
     (step-back-expected?
      "qux" ["baz" "foo"]
      :return-poped? true
      :next-key "bar"))
  ;; return bool for test by default
  (false? (step-back-expected?
           "qux" ["baz" "foo"]
           :expected-pop ["baz"]))
  (true? (step-back-expected?
          "baz" ["baz" "foo"]
          :expected-pop ["baz"]))
  (true? (step-back-expected?
          "baz" ["bar" "baz" "foo"]
          :next-key "bar")))
