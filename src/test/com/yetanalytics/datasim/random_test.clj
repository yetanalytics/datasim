(ns com.yetanalytics.datasim.random-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.random :refer :all]
            [clojure.test.check :as tc]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer :all]))

(deftest random-functions-test
  (let [results (stest/check
                 `#{seed-rng
                    rand*
                    rand-int*
                    rand-nth*
                    shuffle*
                    random-sample*
                    rand-gauss
                    rand-long
                    rand-uuid
                    choose
                    })
        {:keys [total
                check-passed]} (stest/summarize-results results)]
    (is (= total check-passed))))
