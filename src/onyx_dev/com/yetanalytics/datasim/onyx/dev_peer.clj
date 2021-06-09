(ns com.yetanalytics.datasim.onyx.dev-peer
  (:gen-class)
  (:require onyx.plugin.http-output
            onyx.plugin.seq
            onyx.plugin.s3-output
            onyx.api
            com.yetanalytics.datasim.onyx.sim
            com.yetanalytics.datasim.onyx.http
            [com.yetanalytics.datasim.onyx.config :as config]))

;; Trying just the peer

(defn -main
  [& args]
  ;; VERY MUCH JUST DEV RIGHT NOW
  (let [;; DEV ENV config, should go away for prod things
        id (java.util.UUID/randomUUID)
        {:keys [env-config peer-config]
         {:keys [n-vpeers]} :launch-config} (-> (config/get-config)
                                             (assoc-in [:env-config :onyx/tenancy-id] id)
                                             (assoc-in [:peer-config :onyx/tenancy-id] id))
        env (onyx.api/start-env env-config)

        ;; start peer group
        peer-group (onyx.api/start-peer-group peer-config)

        v-peers (onyx.api/start-peers n-vpeers peer-group)]


    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. ^Runnable #(do
                                            (onyx.api/shutdown-peers v-peers)

                                            (onyx.api/shutdown-peer-group peer-group)

                                            (onyx.api/shutdown-env env))))))
