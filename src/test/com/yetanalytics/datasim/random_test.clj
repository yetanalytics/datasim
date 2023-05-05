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
                    r/choose})
        {:keys [total
                check-passed]} (stest/summarize-results results)]
    (is (= total check-passed))))
