(ns com.yetanalytics.datasim.onyx.main
  (:gen-class)
  (:require [onyx.plugin.http-output :as http]
            [onyx.plugin.seq]
            [onyx.api]
            [com.yetanalytics.datasim.onyx.job :as job]
            [com.yetanalytics.datasim.onyx.seq :as dseq]))

(defn -main
  "Assuming the given tenancy + peer config is running, submit a sim + post job"
  [tenancy-id input-loc endpoint & rest-args]
  (let [ ;; Peer config: should be there at peer launch + job submission
        peer-config
        {:zookeeper/address "127.0.0.1:2188"
         :onyx/tenancy-id tenancy-id
         :onyx.peer/job-scheduler :onyx.job-scheduler/balanced
         :onyx.messaging/impl :aeron
         :onyx.messaging/peer-port 40200
         :onyx.messaging/bind-addr "localhost"}

        submission (onyx.api/submit-job
                    peer-config
                    (job/config
                     {:input-json (slurp input-loc)
                      :lrs {:endpoint endpoint
                            :batch-size 25}
                      }))
        ]
    (onyx.api/await-job-completion peer-config (:job-id submission))
    (println "job complete!")
    (clojure.pprint/pprint submission)
    (System/exit 0)))
