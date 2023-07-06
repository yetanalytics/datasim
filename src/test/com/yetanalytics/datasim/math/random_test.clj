(ns com.yetanalytics.datasim.math.random-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.spec.test.alpha :as stest]
            [com.yetanalytics.datasim.math.random :as r]))

(deftest random-functions-test
  (let [results (stest/check
                 `#{r/seed-rng
                    r/rand
                    r/rand-int
                    r/rand-nth*
                    r/shuffle*
                    r/random-sample*
                    r/rand-gaussian
                    r/rand-boolean
                    r/rand-unbound-int
                    r/rand-uuid
                    r/choose
                    r/choose-map})
        {:keys [total
                check-passed]} (stest/summarize-results results)]
    (is (= total check-passed))))
