(ns com.yetanalytics.datasim.main
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as cs]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(def cli-options
  [["-p" "--profile URI" "xAPI Profile Location"
    :id :profile
    :desc "The location of an xAPI profile, can be used multiple times."
    :parse-fn (fn [x]
                (println x)
                x)
    :assoc-fn (fn [omap id v]
                (update omap
                        id
                        (fnil conj [])
                        v))]
   ["-h" "--help"]])

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
