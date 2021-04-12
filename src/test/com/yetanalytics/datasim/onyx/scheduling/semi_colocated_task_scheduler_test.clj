(ns com.yetanalytics.datasim.onyx.scheduling.semi-colocated-task-scheduler-test
  (:require [clojure.test :refer :all]
            [onyx.scheduling.common-job-scheduler :refer [reconfigure-cluster-workload]]
            [onyx.api]
            com.yetanalytics.datasim.onyx.scheduling.semi-colocated-task-scheduler))

;; From onyx.log.generators

(defn one-group [replica]
  (-> replica
      (assoc :groups [:g1])
      (assoc-in [:groups-index :g1] (into #{} (:peers replica)))
      ((fn [rep]
         (reduce
          #(assoc-in %1 [:groups-reverse-index %2] :g1)
          rep
          (:peers rep))))))

(defn generate-group-and-peer-ids
  ([groups peers]
   (generate-group-and-peer-ids 1 groups
                                1 peers))
  ([groups-low groups-high peers-low peers-high]
   (reduce
    (fn [r g]
      (let [g-id (keyword (str "g" g))
            peers (map
                   #(keyword (str (name g-id) "-p" %))
                   (range peers-low (+ peers-low peers-high)))]
        (assoc r g-id (into #{} peers))))
    {}
    (range groups-low (+ groups-low groups-high)))))

(defn gen-replica
  [groups peers]
  (let [group-map (generate-group-and-peer-ids groups peers)]
    {

     :peers
     (into []
           (mapcat
            seq
            (vals group-map))),
     :peer-tags
     (into {}
           (for [[g ps] group-map
                 p ps]
             [p []]))
     :groups-index group-map,
     :peer-sites
     (into {}
           (for [[g ps] group-map
                 p ps]
             [p
              {:address (format "%s-addr"
                                (name g)
                                )
               :port 40200}])),
     :groups (into [] (keys group-map)),
     :groups-reverse-index
     (into {}
           (for [[g ps] group-map
                 p ps]
             [p g])),
     :pairs (into {}
                  (mapcat identity
                          (for [[g0 _]  group-map
                                [g1 _]  group-map
                                :when (not= g0 g1)]
                            [[g0 g1]
                             [g1 g0]])
                          #_(for [[[g0 _] [g1 _]] (partition 2 group-map)]
                            [[g0 g1]
                             [g1 g0]]))),
     :orphaned-peers {},
     :output-tasks {},
     :grouped-tasks {},
     :job-scheduler :onyx.job-scheduler/balanced,
     :saturation {},
     :in->out {},
     :task-percentages {},
     :min-required-peers {},
     :state-tasks {},
     :task-slot-ids {},
     :accepted {},
     :aborted #{},
     :jobs [],
     :tasks {},
     :task-metadata {},
     :allocation-version {},
     :required-tags {},
     :flux-policies {},
     :messaging #:onyx.messaging{:impl :aeron},
     :coordinators {},
     :reduce-tasks {},
     :allocations {},
     :killed-jobs [],
     :prepared {},
     :percentages {},
     :input-tasks {},
     :message-short-ids {},
     :version 7,
     :completed-jobs [],
     :log-version "0.14.6-SNAPSHOT",
     :task-schedulers {},
     :left #{},
     :task-saturation {}
     }))


(deftest semi-colocated-colocates-workflow-edges
  (is
   (=
    {:j1 {:in-1 [:g1-p2],
          :out-1 [:g1-p1],
          :out-2 [:g2-p1],
          :in-2 [:g2-p2],
          :out-0 [:g3-p2],
          :in-0 [:g3-p1]}}
    (let [old (gen-replica
               3 ;; three physical
               2) ;; two vpeers each

          new (merge old
                     {:task-schedulers {:j1 :onyx.task-scheduler/semi-colocated}
                      :jobs [:j1]
                      :tasks {:j1 [:in-0 :out-0
                                   :in-1 :out-1
                                   :in-2 :out-2]}
                      :saturation {:j1 6}
                      :task-saturation {:j1 {:in-0 1
                                             :out-0 1
                                             :in-1 1
                                             :out-1 1
                                             :in-2 1
                                             :out-2 1}}
                      :in->out {:j1 {:in-0 #{:out-0}
                                     :in-1 #{:out-1}
                                     :in-2 #{:out-2}}}
                      })]
      (:allocations (reconfigure-cluster-workload new old))))))

(deftest semi-colocated-colocates-workflow-edges-extra-space
  (testing "Semi-colocated tasks can deploy to groups with more slots available
            than needed."
    (is
     (=
      {:j1 {:in-0 [:g3-p2],
            :out-0 [:g3-p1],
            :in-1 [:g1-p2],
            :out-1 [:g1-p1]
            :in-2 [:g2-p2],
            :out-2 [:g2-p1]}}
      (let [old (gen-replica
                 3 ;; three physical
                 3) ;; three vpeers each

            new (merge old
                       {:task-schedulers {:j1 :onyx.task-scheduler/semi-colocated}
                        :jobs [:j1]
                        :tasks {:j1 [:in-0 :out-0
                                     :in-1 :out-1
                                     :in-2 :out-2]}
                        :saturation {:j1 6}
                        :task-saturation {:j1 {:in-0 1
                                               :out-0 1
                                               :in-1 1
                                               :out-1 1
                                               :in-2 1
                                               :out-2 1}}
                        :in->out {:j1 {:in-0 #{:out-0}
                                       :in-1 #{:out-1}
                                       :in-2 #{:out-2}}}
                        })]
        (:allocations (reconfigure-cluster-workload new old)))))))

;; TODO: this can only deploy once per machine. We just need to reduce task groups over the groups!

#_(deftest semi-colocated-colocates-workflow-edges-mult-per-machine
  (testing "Semi-colocated tasks can deploy to groups multiple-times."
    (is
     (=
      {:j1 {:in-0 [:g1-p2],
            :out-0 [:g3-p1],
            :in-1 [:g1-p2],
            :out-1 [:g1-p1]
            :in-2 [:g2-p2],
            :out-2 [:g2-p1]}}
      (let [old (gen-replica
                 1 ;; three physical
                 9) ;; three vpeers each

            new (merge old
                       {:task-schedulers {:j1 :onyx.task-scheduler/semi-colocated}
                        :jobs [:j1]
                        :tasks {:j1 [:in-0 :out-0
                                     :in-1 :out-1
                                     :in-2 :out-2]}
                        :saturation {:j1 6}
                        :task-saturation {:j1 {:in-0 1
                                               :out-0 1
                                               :in-1 1
                                               :out-1 1
                                               :in-2 1
                                               :out-2 1}}
                        :in->out {:j1 {:in-0 #{:out-0}
                                       :in-1 #{:out-1}
                                       :in-2 #{:out-2}}}
                        })]
        (:allocations (reconfigure-cluster-workload new old)))))))
