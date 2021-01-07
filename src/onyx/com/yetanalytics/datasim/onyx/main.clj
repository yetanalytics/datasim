(ns com.yetanalytics.datasim.onyx.main
  (:gen-class)
  (:require [onyx.plugin.http-output :as http]
            [onyx.plugin.seq]
            [onyx.api]
            [com.yetanalytics.datasim.onyx.job :as job]
            [com.yetanalytics.datasim.onyx.seq :as dseq]
            [com.yetanalytics.datasim.onyx.config :as config]))

(defn -main
  "Assuming the given tenancy + peer config is running, submit a sim + post job"
  [tenancy-id input-loc endpoint & [username password async?]]
  (let [{:keys [peer-config]} (-> (config/get-config)
                                  (assoc-in [:peer-config :onyx/tenancy-id] tenancy-id))
        submission (onyx.api/submit-job
                    peer-config
                    (job/config
                     {:input-json (slurp input-loc)
                      :lrs {:endpoint endpoint
                            :username username
                            :password password
                            :batch-size 25}}))]
    (when-not (= "true" async?)
      (onyx.api/await-job-completion peer-config (:job-id submission))
      (println "job complete!"))
    (clojure.pprint/pprint submission)
    (System/exit 0)))
