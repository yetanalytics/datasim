(ns com.yetanalytics.datasim.onyx.scratch
  "Scratch ns for playing with onyx"
  (:require
   [clojure.core.async :as a]
   [onyx.plugin.http-output :as http]
   [onyx.plugin.core-async :as ap]
   [onyx.plugin.seq]
   [onyx.api]
   [onyx.test-helper :as th]
   [com.yetanalytics.datasim.onyx.seq :as dseq]
   [com.yetanalytics.datasim.onyx.job :as job]
   [com.yetanalytics.datasim.onyx.config :as config]))

(defonce out-chan (a/chan 1000))

(defonce buffer (atom {}))

(defonce outputter
  (a/go-loop []
    (when-let [segment (a/<!! out-chan)]
      (print ".")
      (flush)
      (recur))))

(defn inject-out-ch [event lifecycle]
  {:core.async/chan out-chan})

(def out-calls
  {:lifecycle/before-task-start inject-out-ch})

(comment
  ;; Run things in an enclosed environment
  (let [id (java.util.UUID/randomUUID)
        {:keys [env-config peer-config]} (-> (config/get-config)
                                             (assoc-in [:env-config :onyx/tenancy-id] id)
                                             (assoc-in [:peer-config :onyx/tenancy-id] id))]
    (th/with-test-env
      [{:keys [n-peers
               env
               peer-group
               peers]
        :as test-env} [;; n-peers
                       8 ;; (64 % 16) * 2 ;;;;; 12 ;; max for procs on my macbook
                       env-config
                       peer-config
                       ]]
      (let [onyx-batch-size 10
            lrs-batch-size 1000
            concurrency 5
            ;; Submit the job
            submission (onyx.api/submit-job
                        peer-config
                        (-> (job/config-2
                             {:input-json (slurp "dev-resources/input/mom.json")
                              :batch-size onyx-batch-size
                              :concurrency concurrency
                              :lrs {
                                    ;; :endpoint "https://p7lbpgmvrb.execute-api.us-east-1.amazonaws.com/prod"
                                    ;; :x-api-key "7ArN6ATCXS9VkQCoejRkC202gUFVieRs7yBXG7qR"
                                    :endpoint "http://localhost:8080/xapi"

                                    :batch-size lrs-batch-size
                                    }
                              })
                            ;; don't do the http
                            #_(update :lifecycles into [{:lifecycle/task :out
                                                      :lifecycle/calls ::out-calls}
                                                     {:lifecycle/task :out
                                                      :lifecycle/calls :onyx.plugin.core-async/writer-calls}])
                            #_(update :catalog #(conj
                                               (into []
                                                     (butlast %))
                                               {:onyx/name :out
                                                :onyx/plugin :onyx.plugin.core-async/output
                                                :onyx/type :output
                                                :onyx/medium :core.async
                                                :onyx/max-peers 1
                                                :onyx/batch-size onyx-batch-size
                                                :onyx/doc "Writes segments to a core.async channel"}))))]

        ;; Wait for jorb to finish if you like
        (println 'started submission)
        (onyx.api/await-job-completion peer-config (:job-id submission))
        (println 'done submission)
        )))

  (+ 1 1
     )

  (defn- wait-for-all-jorbs
    [peer-config
     job-ids]
    (future
      (doall
       (for [job-id job-ids]
         (onyx.api/await-job-completion peer-config job-id)))))

  ;; colocated madness

  ;; respect colocated constraints?
  (require '[onyx.scheduling.common-task-scheduler :as cts])

  (defmethod cts/assign-capacity-constraint? :onyx.task-scheduler/colocated
    [_ _]
    true)

  (let [id (java.util.UUID/randomUUID)
        {:keys [env-config peer-config]} (-> (config/get-config)
                                             (assoc-in [:env-config :onyx/tenancy-id] id)
                                             (assoc-in [:peer-config :onyx/tenancy-id] id))]
    (th/with-test-env
      [{:keys [n-peers
               env
               peer-group
               peers]
        :as test-env} [;; n-peers
                       8 ;; (64 % 16) * 2 ;;;;; 12 ;; max for procs on my macbook
                       env-config
                       peer-config
                       ]]
      (let [onyx-batch-size 10
            lrs-batch-size 1000
            concurrency 4
            ;; make jobs
            configs (into []
                          (job/colo-configs
                           {:input-json (slurp "dev-resources/input/mom.json")
                            :batch-size onyx-batch-size
                            :concurrency concurrency
                            ;; :override-max 1000
                            :lrs {
                                  ;; :endpoint "https://p7lbpgmvrb.execute-api.us-east-1.amazonaws.com/prod"
                                  ;; :x-api-key "7ArN6ATCXS9VkQCoejRkC202gUFVieRs7yBXG7qR"
                                  :endpoint "http://localhost:8080/xapi"
                                  :batch-size lrs-batch-size
                                  }
                            }))
            ;; _ (clojure.pprint/pprint configs)
            ;; Submit the job
            submissions
            (doall
             (for [config configs]
               (onyx.api/submit-job
                peer-config
                (-> config
                    ;; don't do the http
                    #_(update :lifecycles into [{:lifecycle/task :out
                                                 :lifecycle/calls ::out-calls}
                                                {:lifecycle/task :out
                                                 :lifecycle/calls :onyx.plugin.core-async/writer-calls}])
                    #_(update :catalog #(conj
                                         (into []
                                               (butlast %))
                                         {:onyx/name :out
                                          :onyx/plugin :onyx.plugin.core-async/output
                                          :onyx/type :output
                                          :onyx/medium :core.async
                                          :onyx/max-peers 1
                                          :onyx/batch-size onyx-batch-size
                                          :onyx/doc "Writes segments to a core.async channel"}))))))]

        ;; Wait for jorb to finish if you like
        (println 'started submissions)

        (println 'done @(wait-for-all-jorbs peer-config (map :job-id submissions)))
        )))


  (let [cfg {:input-json (slurp "dev-resources/input/mom.json")
             :batch-size 10
             :concurrency 1
             ;; :override-max 1000
             :lrs {
                   ;; :endpoint "https://p7lbpgmvrb.execute-api.us-east-1.amazonaws.com/prod"
                   ;; :x-api-key "7ArN6ATCXS9VkQCoejRkC202gUFVieRs7yBXG7qR"
                   :endpoint "http://localhost:8080/xapi"
                   :batch-size 1000
                   }
             }]
    (clojure.pprint/pprint
     (clojure.data/diff
      (first (job/colo-configs
              cfg))
      (job/config cfg))
     )
    )


  )
