(ns com.yetanalytics.datasim.onyx.http)

(defn post-success?
  [{:keys [status error]}]
  (and (= 200 status)
       (not error)))
