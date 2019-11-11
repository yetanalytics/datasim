(ns com.yetanalytics.datasim.main
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as cs]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.input :as input]
            [com.yetanalytics.datasim.protocols :as p])
  (:gen-class))

(def cli-options
  [["-p" "--profile URI" "xAPI Profile Location"
    :id :profiles
    :desc "The location of an xAPI profile, can be used multiple times."
    :parse-fn (partial input/from-location :profile)
    :validate-fn (fn [x]
                   (when (nil? (p/validate x))
                     x))
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
          (let [input (input/map->Input options)]
            (if-let [spec-error (s/explain-data :com.yetanalytics.datasim/input input)]
              (do (println "spec error!")
                  (s/explain :com.yetanalytics.datasim/input input))
              (println "options look good!"))))))
