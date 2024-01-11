(ns com.yetanalytics.datasim.cli
  (:require [clojure.tools.cli  :as cli]
            [com.yetanalytics.datasim.cli.input    :as cli-input]
            [com.yetanalytics.datasim.cli.generate :as cli-gen]
            [com.yetanalytics.datasim.cli.util     :as u])
  (:gen-class))

(def top-level-options
  [["-h" "--help" "Display the top-level help guide."]])

(def top-level-summary
  (str "Usage: 'datasim <subcommand> <args>' or 'datasim [-h|--help]'.\n"
       "\n"
       "where the subcommand can be one of the following:\n"
       "  validate-input: Validate the input and create an input JSON file.\n"
       "  generate:       Generate statements from input and print to stdout.\n"
       "  generate-post:  Generate statements from input and POST them to an LRS.\n"
       "\n"
       "Run 'datasim <subcommand> --help' for more info on each subcommand."))

(defn -main [& args]
  (let [{:keys [options arguments summary errors]}
        (cli/parse-opts args top-level-options
                        :in-order true
                        :summary-fn (fn [_] top-level-summary))
        [subcommand & rest-args]
        arguments]
    (cond
      (:help options)
      (println summary)
      (not subcommand)
      (print "No subcommand entered.\n\n" summary)
      :else
      (let [results (case subcommand
                      "validate-input" (cli-input/validate-input! rest-args)
                      "generate"       (cli-gen/generate! rest-args)
                      "generate-post"  (cli-gen/generate-post! rest-args)
                      (u/bail! errors))]
        (when-some [errors (:errors results)]
          (u/bail! errors))))))
