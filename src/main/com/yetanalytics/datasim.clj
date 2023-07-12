(ns com.yetanalytics.datasim
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.input :as input]
            [com.yetanalytics.datasim.sim   :as sim]))

(s/fdef read-input
  :args (s/cat :location string?)
  :ret ::input)

(defn read-input
  "Read input at `location` and return DATASIM input for use with
   other functions."
  [location]
  (input/from-location :input :json location))

;; TODO: Add fdefs for generation functions
;; TODO: Extract more implementation code from `sim` namespace

#_{:clj-kondo/ignore [:unused-binding]}
(defn generate-seq
  "Given `input`, produce a lazy sequence of statements in a synchronous
   fashion."
  [input & {:keys [select-agents] :as kwargs}]
  (sim/sim-seq input kwargs))

(defn generate-map
  "Given `input`, produce a map from actor IFIs to lazy sequences of statements
   that use those actors, all in a synchronous fashion."
  [input]
  (sim/build-skeleton input))

#_{:clj-kondo/ignore [:unused-binding]}
(defn generate-seq-async
  "Given `input`, produce a `core.async` channels that contains a generated
   sequence of simulated statements; this is for parallel generation."
  [input & {:keys [select-agents pad-chan-max] :as kwargs}]
  (sim/sim-chan input kwargs))

#_{:clj-kondo/ignore [:unused-binding]}
(defn generate-map-async
  "Given `input`, produce a map from actor IFIs to `core.async` channels,
   each with their own generated sequence of simulated statements; this
   is for parallel generation."
  [input & {:keys [select-agents pad-chan-max sort buffer-size] :as kwargs}]
  (sim/sim-chans input kwargs))
