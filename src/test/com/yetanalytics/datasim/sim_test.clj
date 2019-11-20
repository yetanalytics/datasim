(ns com.yetanalytics.datasim.sim-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.sim :refer :all]
            [com.yetanalytics.datasim.input :as input]
            [clojure.spec.alpha :as s]))

(deftest build-skeleton-test
  (is (s/valid? :com.yetanalytics.datasim.sim/skeleton
                (build-skeleton
                 (input/from-location
                  :input :json "dev-resources/input/simple.json")))))
