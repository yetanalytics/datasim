(ns com.yetanalytics.datasim.onyx.peer
  (:gen-class)
  (:require onyx.plugin.http-output
            onyx.plugin.seq
            onyx.api
            com.yetanalytics.datasim.onyx.seq
            com.yetanalytics.datasim.onyx.http
            [com.yetanalytics.datasim.onyx.config :as config]))

(defn -main
  "Launch a peer"
  [& args]
  (let [{:keys [env-config
                peer-config
                launch-config]
         {:keys [n-vpeers]} :launch-config} (config/get-config)

        env (onyx.api/start-env env-config)
        ;; start peer group
        peer-group (onyx.api/start-peer-group peer-config)

        v-peers (onyx.api/start-peers n-vpeers peer-group)]


    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. ^Runnable #(do
                                            (onyx.api/shutdown-peers v-peers)
                                            (onyx.api/shutdown-peer-group peer-group)
                                            (onyx.api/shutdown-env env))))))
