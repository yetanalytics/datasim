(ns com.yetanalytics.datasim.onyx.seq
  "Feed datasim seqs into onyx"
  (:require [com.yetanalytics.datasim.sim :as sim]
            [com.yetanalytics.datasim.input :as input]
            [cheshire.core :as json]
            [com.yetanalytics.datasim.onyx.util :as u])
  (:import [java.io ByteArrayInputStream]))

(defn inject-seq [_ {input-json ::input-json
                     select-agents ::select-agents
                     {:keys [endpoint batch-size]} ::lrs
                     :as lifecycle}]
  (let [input (u/parse-input input-json)]
    {:seq/seq
     (map (fn [statements]
            {:url (format "%s/statements" endpoint)
             :args
             {:headers {"X-Experience-API-Version" "1.0.3"
                        "Content-Type" "application/json"}
              :body (json/generate-string (into [] statements))
              :as :json}})
          (partition-all batch-size
                         (sim/sim-seq
                          input
                          :select-agents select-agents)))}))

(def in-calls
  {:lifecycle/before-task-start inject-seq})
