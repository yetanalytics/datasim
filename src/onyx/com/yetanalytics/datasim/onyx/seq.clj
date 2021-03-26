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
                     {:keys [batch-size]} ::lrs
                     :or {strip-ids? false
                          remove-refs? false}
                     :as lifecycle}]
  (let [id (java.util.UUID/randomUUID)
        input (u/parse-input input-json)]
    {::id id
     :seq/seq
     (map-indexed
      (fn [idx batch]
        {:id id
         :idx idx
         :agents select-agents
         :statements (into []
                           (map
                            #(with-meta % nil)
                            batch))})
      (partition-all batch-size
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
                           (get-in % ["object" "objectType"]))))))}))

(def in-calls
  {:lifecycle/before-task-start inject-seq})
