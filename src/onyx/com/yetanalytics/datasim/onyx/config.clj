(ns com.yetanalytics.datasim.onyx.config
  (:require [aero.core :as aero]
            [clojure.java.io :as io]))

(defn get-config []
  (let [profile (some-> (System/getenv "ONYX_PROFILE") keyword)]
    (aero/read-config (io/resource "onyx_config.edn")
                      {:profile (or profile :default)})))
