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
            concurrency 4
            ;; Submit the job
            submission (onyx.api/submit-job
                        peer-config
                        (-> (job/config
                             {:input-json (slurp "dev-resources/input/mom.json")
                              :batch-size onyx-batch-size
                              :concurrency concurrency
                              :lrs {
                                    :endpoint "http://localhost:8080/xapi"
                                    :batch-size lrs-batch-size
                                    }
                              })
                            ;; don't do the http
                            (update :lifecycles into [{:lifecycle/task :out
                                                      :lifecycle/calls ::out-calls}
                                                     {:lifecycle/task :out
                                                      :lifecycle/calls :onyx.plugin.core-async/writer-calls}])
                            (update :catalog #(conj
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
  )
