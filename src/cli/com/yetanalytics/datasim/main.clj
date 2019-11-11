(ns com.yetanalytics.datasim.main
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(def cli-options
  [["-h" "--help"]])

(defn -main [& args]
  (let [{:keys [options
                arguments
                summary
                errors]
         :as parsed-opts} (parse-opts args cli-options)]

    (cond (seq errors)
          (do (doseq [e-msg errors]
                (println e-msg))
              (print summary))

          (:help options)
          (print summary)

          :else
          (pprint parsed-opts))))
