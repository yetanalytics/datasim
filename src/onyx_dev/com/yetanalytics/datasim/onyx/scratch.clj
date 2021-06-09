(ns com.yetanalytics.datasim.onyx.scratch
  "Scratch ns for playing with onyx"
  (:require
   [clojure.core.async :as a]
   com.yetanalytics.datasim.onyx.main ;; get all boot-up ns stuff
   [onyx.plugin.http-output :as http]
   [onyx.plugin.core-async :as ap]
   [onyx.plugin.seq]
   [onyx.api]
   [onyx.test-helper :as th]
   [com.yetanalytics.datasim.onyx.sim :as dsim]
   [com.yetanalytics.datasim.onyx.job :as job]
   [com.yetanalytics.datasim.onyx.config :as config]))

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
                       3 ;;
                       env-config
                       peer-config
                       ]]
      (let [;; Submit the job
            submission (onyx.api/submit-job
                        peer-config
                        (job/config
                         {:input-loc "dev-resources/input/mom64.json"
                          :gen-concurrency 1
                          :gen-batch-size 10
                          :post-concurrency 1
                          :override-max 100
                          :out-ratio 1
                          :out-mode :lrs
                          :lrs {:endpoint "http://localhost:8080/xapi"}}))]

        ;; Wait for jorb to finish if you like
        (println 'started submission)
        (onyx.api/await-job-completion peer-config (:job-id submission))
        (println 'done submission)
        )))

  )
