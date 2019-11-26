(ns com.yetanalytics.datasim.runtime
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.sim :as sim]
            [com.yetanalytics.datasim.input :as input]
            [com.yetanalytics.datasim.util.sequence :as su]
            [clojure.data.json :as json])
  (:import [java.time Instant]))

;; Simple, single-thread impl for now
;; Gets the statement seqs and combines them, writes to out
(s/fdef run-sim!
  :args (s/cat :input :com.yetanalytics.datasim/input)
  :ret nil?)

(defn run-sim! [input]
  (doseq [s (-> (sim/build-skeleton input)
                ;; take the actor statement seqs
                vals
                (->> (su/seq-sort
                      (fn [{timestamp-str "timestamp"
                            timestamp-key :timestamp
                            ;; TODO: remove, just dev
                            t :t}]
                        (.toEpochMilli
                         (Instant/parse (or timestamp-str
                                            timestamp-key
                                            t)))))))]
    (json/write s *out*
                :escape-slash false
                :escape-unicode false)
    (.write *out* "\n")
    (flush)))


(comment
  (def i
    (update (input/from-location :input :json "dev-resources/input/simple.json")
            :parameters
            merge
            {:start "2019-11-18T11:38:39.219768Z",
             :end "2019-11-19T11:38:39.219768Z"
             :timezone "America/New_York",
             :seed 42,
             :from "2019-11-18T11:38:39.219768Z"}))

  (run-sim! i)




  )
