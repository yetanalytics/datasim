(ns com.yetanalytics.datasim.onyx.peer
  (:gen-class)
  (:require onyx.plugin.http-output
            onyx.plugin.seq
            onyx.api
            com.yetanalytics.datasim.onyx.seq
            com.yetanalytics.datasim.onyx.http))

;; Trying just the peer

(defn -main
  [& args]
  ;; VERY MUCH JUST DEV RIGHT NOW
  (let [;; DEV ENV config, should go away for prod things
        id (java.util.UUID/randomUUID)

        env-config
        {:zookeeper/address "127.0.0.1:2188"
         :zookeeper/server? true
         :zookeeper.server/port 2188
         :onyx/tenancy-id id}

        env (onyx.api/start-env env-config)

        ;; Peer config: should be there at peer launch + job submission

        peer-config
        {:zookeeper/address "127.0.0.1:2188"
         :onyx/tenancy-id id
         :onyx.peer/job-scheduler :onyx.job-scheduler/balanced
         :onyx.messaging/impl :aeron
         :onyx.messaging/peer-port 40200
         :onyx.messaging/bind-addr "localhost"}

        ;; start peer group
        peer-group (onyx.api/start-peer-group peer-config)

        n-peers 12

        v-peers (onyx.api/start-peers n-peers peer-group)]


    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. ^Runnable #(do
                                            (onyx.api/shutdown-peers v-peers)

                                            (onyx.api/shutdown-peer-group peer-group)

                                            (onyx.api/shutdown-env env))))))
