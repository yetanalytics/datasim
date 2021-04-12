(ns com.yetanalytics.datasim.onyx.scheduling.semi-colocated-task-scheduler
  (:require [onyx.scheduling.common-task-scheduler :as cts]
            [onyx.scheduling.common-job-scheduler :as cjs]
            [onyx.static.util :refer [index-of]]
            [onyx.log.commands.common :as common]
            [onyx.scheduling.colocated-task-scheduler :as colo]
            [clojure.set :as cset])
  (:import [org.btrplace.model.constraint Fence SplitAmong Ban]))

;; Only ever gives one peer to a task
(defmethod cts/task-distribute-peer-count :onyx.task-scheduler/semi-colocated
  [replica job n]
  (let [tasks (get-in replica [:tasks job])
        t (cjs/job-lower-bound replica job)]
    (if (< n t)
      (zipmap tasks (repeat 0))
      ;; one peer to a task
      (zipmap tasks (repeat 1)))))



(defn group-tasks
  "Greedily partition tasks by i/o"
  [in->out job-id]
  (reduce-kv
   (fn [groups in-task out-tasks]
     (if-let [applicable-group (some (fn [g]
                                       (when
                                           (or
                                            ;; in task in other groups (an branch task)
                                            (contains? g in-task)
                                            ;; out task in other groups
                                            (not-empty
                                             (cset/intersection
                                              out-tasks
                                              g)))
                                         g))
                                     groups)]
       ;; join
       (-> groups
           (disj applicable-group)
           (conj (into applicable-group
                       (cons in-task
                             out-tasks))))
       ;; new group
       (conj groups
             (into #{}
                   (cons in-task
                         out-tasks)))))
   #{}
   (get in->out job-id)))


;; All tasks should be bounded, or they get a sat of 1
(defmethod cts/task-constraints :onyx.task-scheduler/semi-colocated
  [{:keys [groups-index
           in->out
           saturation ;; total
           task-saturation ;; all jorbs
           ]
    :as replica} jobs task-capacities peer->vm task->node no-op-node job-id]
  (let [job-saturation (get saturation job-id)
        task-saturations (get task-saturation job-id)
        task-ids (get-in replica [:tasks job-id])
        ;; pull out physical groupings
        group->peer->vm
        (into {}
              (for [[g ps] groups-index]
                [g
                 (select-keys peer->vm ps)]))
        ;; greedily group task io into groups
        task-groups
        (group-tasks
         in->out job-id)
        ;; here this is the min group size to run an i/o group
        ;; if it's infinity this won't ever work
        capacity (apply max
                         0
                         (map (fn [task-group]
                                (reduce
                                 +
                                 (map
                                  #(get task-saturations % 1)
                                  task-group)))
                              task-groups))
        suitable-groups (into {}
                              (keep
                               (fn [[g ps]]
                                 (let [psc (count ps)]
                                   (when (<= capacity psc)
                                     [g
                                      (quot psc
                                            capacity)])))
                               groups-index))
        ret
        ;; if we can cover all tasks
        (if (<=
             (count task-groups)
             (count suitable-groups))
          (into []
                (map
                 (fn [[g pvm] task-group]
                   (SplitAmong.
                    [(take (count task-group) (vals pvm))]
                    [(mapv
                      #(get task->node [job-id %])
                      task-group)]))
                 group->peer->vm
                 task-groups))
          ;; Can't fit? BAN EVERYTHING
          (into []
                (Ban/newBan (vals peer->vm) (vals task->node))))]

    ret))


(defmethod cts/assign-capacity-constraint? :onyx.task-scheduler/semi-colocated
  [replica job-id]
  true)
