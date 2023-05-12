(ns com.yetanalytics.datasim.random-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.spec.test.alpha :as stest]
            [com.yetanalytics.datasim.random :as r]))

(deftest random-functions-test
  (let [results (stest/check
                 `#{r/seed-rng
                    r/rand*
                    r/rand-int*
                    r/rand-nth*
                    r/shuffle*
                    r/random-sample*
                    r/rand-gauss
                    r/rand-long
                    r/rand-uuid
                    r/choose
                    r/select-replace
                    r/select-no-replace
                    ;; no gen for this since it returns an infinite seq
                    #_r/combinations})
        {:keys [total
                check-passed]} (stest/summarize-results results)]
    (is (= total check-passed))))

(comment
  (def the-rng (r/seed-rng 100))
  
  (dotimes [_ 1000]
    (r/select-replace the-rng [1] 10)))
