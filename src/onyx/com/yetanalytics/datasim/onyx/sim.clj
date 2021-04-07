(ns com.yetanalytics.datasim.onyx.sim
  "Feed datasim seqs into onyx"
  (:require [com.yetanalytics.datasim.sim :as sim]
            [com.yetanalytics.datasim.input :as input]
            [com.yetanalytics.datasim.onyx.util :as u]
            [onyx.plugin.protocols :as p]
            [clojure.core.async :as a]
            [com.yetanalytics.datasim.util.sequence :as su]
            [taoensso.timbre :refer [fatal infof debug warnf] :as timbre]))


(defn init-seq
  [input
   {:keys [select-agents
           strip-ids?
           remove-refs?
           take-n
           drop-n
           batch-size]
    :as args}]
  (lazy-seq
   (cond->> (if select-agents
              (sim/sim-seq
               input
               :select-agents select-agents)
              (sim/sim-seq
               input))
     take-n (take take-n)
     drop-n (drop (* drop-n batch-size))
     strip-ids?
     (map
      #(dissoc % "id"))
     remove-refs?
     (remove
      #(= "StatementRef"
          (get-in % ["object" "objectType"])))
     ;; chop em up
     batch-size
     (partition-all batch-size)
     batch-size
     (map-indexed
      (fn [idx statements]
        {:chunk-idx idx
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
                            statements))})))))

(defn inject-sim-input [_ {input-json ::input-json
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
  {:sim/input-json input-json
   :sim/args {:select-agents select-agents
              :strip-ids? strip-ids?
              :remove-refs? remove-refs?
              :take-n ?take-n
              :drop-n ?drop-n
              :batch-size batch-size}})

;; Onyx Plugin impl lets us do what we want
(defn plugin
  [{:keys [onyx.core/task-map
           sim/input-json
           sim/args] :as event}]
  (let [input (u/parse-input input-json)
        rst (volatile! nil)
        completed? (volatile! nil)
        offset (volatile! nil)]
    (reify
      p/Plugin
      (start [this event]
        this)

      (stop [this event]
        (vreset! rst nil)
        (vreset! completed? nil)
        (vreset! offset nil)
        this)

      p/Checkpointed
      (checkpoint [this]
        @offset)

      (recover! [this _ checkpoint]
        (if (nil? checkpoint)
          (do
            (infof "DATASIM Input starting up...")
            (vreset! rst (init-seq
                          input
                          args))
            (vreset! completed? false)
            (vreset! offset 0))
          (do
            (warnf "DATASIM recovering by dropping %d segments" checkpoint)
            (vreset! rst (init-seq
                          input
                          (assoc args :drop-n checkpoint)))
            (vreset! completed? false)
            (vreset! offset checkpoint)))
        this)

      (checkpointed! [this epoch]) ;; TODO: keep a running log of n segs for replay, clear here

      p/BarrierSynchronization
      (synced? [this epoch]
        true)

      (completed? [this]
        @completed?)

      p/Input
      (poll! [this _ timeout-ms]

        (if-let [seg (first @rst)]
          (do
            (vswap! rst rest)
            (vswap! offset inc)
            seg)
          (do (vreset! completed? true)
              nil))))))

(def in-calls
  {:lifecycle/before-task-start inject-sim-input})


(comment


  (clojure.pprint/pprint
   (time
    (p/poll!
     (plugin
      {:sim/input-json (slurp "dev-resources/input/mom.json")
       :sim/args {:select-agents
                  #{"mbox::mailto:agent_0@example.org"}
                  :strip-ids? nil
                  :remove-refs? nil
                  :take-n nil
                  :drop-n nil
                  :batch-size nil}})
     nil nil
     ))

   )

  (let [reader (plugin
                {:sim/input-json (slurp "dev-resources/input/mom.json")
                 :sim/args {:select-agents
                            #{"mbox::mailto:agent_0@example.org"}
                            :strip-ids? nil
                            :remove-refs? nil
                            :take-n nil
                            :drop-n nil
                            :batch-size nil
                            }})]
    (p/recover! reader nil nil)
    (time
     (dotimes [n 100000]
       (when-not (p/poll! reader nil nil)
         (print 'x))
       (when (zero? (rem n 10000))
         (println 'seg n 'checkpoint (p/checkpoint reader)))))
    )

  (def i (input/from-location :input :json "dev-resources/input/mom.json"))

  (first (init-seq i {:batch-size 10}))

 )
