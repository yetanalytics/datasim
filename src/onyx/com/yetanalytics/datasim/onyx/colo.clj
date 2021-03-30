(ns com.yetanalytics.datasim.onyx.colo
  (:require [onyx.scheduling.common-task-scheduler :as cts]
            [onyx.scheduling.common-job-scheduler :as cjs]
            [onyx.log.commands.common :as common]
            [onyx.messaging.protocols.messenger :as m]
            [onyx.extensions :as extensions]
            [onyx.scheduling.colocated-task-scheduler :as colo])
  (:import [org.btrplace.model.constraint Fence SplitAmong Ban]))

(defmethod cts/task-distribute-peer-count :onyx.task-scheduler/colocated
  [replica job-id n]
  (let [task-ids (get-in replica [:tasks job-id])
        capacity (count task-ids)
        site->peers-mapping (colo/large-enough-sites (colo/site->peers replica) capacity)
        n-candidate-peers (apply + (map count (vals site->peers-mapping)))
        lower-bound (colo/global-saturation-lower-bound replica job-id task-ids)
        upper-bound (int (/ n-candidate-peers capacity))]
    (zipmap task-ids (repeat 1 #_(min lower-bound upper-bound))))) ;; edit, this provides correct numbers


#_(defmethod cts/task-constraints :onyx.task-scheduler/colocated
  [replica jobs task-capacities peer->vm task->node no-op-node job-id]
  (let [task-ids (get-in replica [:tasks job-id])
        capacity (count task-ids)
        boxes (second (first task-capacities))
        n-peers (* capacity boxes)
        ;; _ (println job-id  boxes capacity n-peers)
        site->peers-mapping (colo/large-enough-sites (colo/site->peers replica) capacity)
        peers (mapcat second (into [] site->peers-mapping))
        {:keys [selected rejected]} (colo/select-peers site->peers-mapping n-peers capacity)
        ;; _ (println job-id 'cap capacity 'boxes boxes 'selected selected 'rejected rejected)
        unrestricted-tasks (conj (map task->node (colo/non-colocated-tasks replica jobs)) no-op-node)]
    (into
     (reduce
      (fn [result peer-ids]
        (conj result
              (SplitAmong.
               (map (comp vector peer->vm) peer-ids)
               (map #(vector (get task->node [job-id %])) task-ids))))
      []
      (partition capacity selected))
      (colo/ban-smaller-sites replica jobs peer->vm task->node site->peers-mapping rejected))))

#_(defmethod cts/assign-capacity-constraint? :onyx.task-scheduler/colocated
  [replica job-id]
  false)


#_(defmethod cts/choose-downstream-peers :onyx.task-scheduler/colocated
  [replica job-id peer-config this-peer downstream-peers]
  (let [candidates (colo/choose-candidates replica peer-config this-peer downstream-peers)]
    (fn [hash-group]
      (rand-nth candidates))))

#_(defmethod cts/choose-acker :onyx.task-scheduler/colocated
  [replica job-id peer-config this-peer ackers]
  (let [candidates (colo/choose-candidates replica peer-config this-peer ackers)]
    (if (not (seq candidates))
      (throw
       (ex-info
        (format
         "Job %s does not have an acker per machine, which is needed for the colocated task scheduler. Raise the limit via the job parameter :acker/percentage." job-id)
        {}))
      (fn []
        (rand-nth candidates)))))
