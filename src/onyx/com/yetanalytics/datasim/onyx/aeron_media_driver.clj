(ns com.yetanalytics.datasim.onyx.aeron-media-driver
  (:require [clojure.core.async :refer [chan <!!]])
  (:import [io.aeron Aeron$Context]
           [io.aeron.driver MediaDriver MediaDriver$Context ThreadingMode]))

(defn start-driver! [& args]
  (let [ctx (doto (MediaDriver$Context.))
        media-driver (MediaDriver/launch ctx)]
    (println "Launched the Media Driver. Blocking forever...")
    (<!! (chan))))
