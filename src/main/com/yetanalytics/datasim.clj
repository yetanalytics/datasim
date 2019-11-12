(ns com.yetanalytics.datasim
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.input :as input]))

;; Inputs
;; (location | literal) -> read -> validate ->

;; Comprehensive Spec of all input data for a simulation
#_(s/def ::input
  (s/keys :req-un [::input/profiles]))



(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
