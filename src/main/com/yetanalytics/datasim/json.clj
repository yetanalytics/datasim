(ns com.yetanalytics.datasim.json
  (:require [clojure.spec.alpha :as s]))

(s/def ::any
  (s/nilable
   (s/or :scalar
         (s/or :string
               string?
               :number
               (s/or :double
                     (s/double-in :infinite? false :NaN? false
                                  :max 1000.0 :min -1000.0)
                     :int
                     int?)
               :boolean
               boolean?)
         :coll
         (s/or :map
               (s/map-of
                string?
                ::any
                :gen-max 4)
               :vector
               (s/coll-of
                ::any
                :kind vector?
                :into []
                :gen-max 4)))))
