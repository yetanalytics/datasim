(ns com.yetanalytics.datasim.onyx.seq
  "Feed datasim seqs into onyx"
  (:require [com.yetanalytics.datasim.sim :as sim]
            [com.yetanalytics.datasim.input :as input]
            [com.yetanalytics.datasim.onyx.util :as u]
            [clojure.walk :as w])
  (:import [java.io ByteArrayInputStream]))

(defn inject-seq [_ {input-json ::input-json
                     select-agents ::select-agents
                     strip-ids? ::strip-ids?
                     remove-refs? ::remove-refs?
                     ?take-n ::take-n
                     ?drop-n ::drop-n
                     batch-size ::batch-size
                     :or {strip-ids? false
                          remove-refs? false
                          batch-size 1}
                     :as lifecycle}]
  (let [seq-id (java.util.UUID/randomUUID)
        input (u/parse-input input-json)]
    {::id seq-id
     :seq/seq
     (take-while
      (comp seq :statements)
      (map-indexed
       (fn [idx statements]
         {:seq-id seq-id
          :chunk-idx idx
          :range [(-> statements
                      first
                      meta
                      :timestamp-ms)
                  (-> statements
                      last
                      meta
                      :timestamp-ms)]
          :statements (into []
                            (map
                             #(with-meta % nil)
                             statements))})
       (cond->> (sim/sim-seq
                 input
                 :select-agents select-agents)
         ?drop-n (drop ?drop-n)
         ?take-n (take ?take-n)
         strip-ids?
         (map
          #(dissoc % "id"))
         remove-refs?
         (remove
          #(= "StatementRef"
              (get-in % ["object" "objectType"])))
         ;; chop em up
         true (partition-all
               batch-size))))}))

(def in-calls
  {:lifecycle/before-task-start inject-seq})
