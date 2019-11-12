(ns com.yetanalytics.datasim.main
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as cs]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.input :as input]
            [expound.alpha :as expound])
  (:gen-class))

(defn cli-options
  "Generate CLI options, skipping validation if `validate?` is false"
  [validate?]
  [["-p" "--profile URI" "xAPI Profile Location"
    :id :profiles
    :desc "The location of an xAPI profile, can be used multiple times."
    :parse-fn (partial input/from-location :profile)
    :validate (if validate?
                [input/validate-throw "Failed to validate profile."]
                [])
    :assoc-fn (fn [omap id v]
                (update omap
                        id
                        (fnil conj [])
                        v))]
   ["-h" "--help"]])

(defn bail!
  "Print error messages to std error and exit."
  [errors & {:keys [status]
             :or {status 1}}]
  (binding [*out* *err*]
    (doseq [e-msg errors]
      (println e-msg))
    (flush)
    (System/exit status)))

(defn -main [& args]
  (let [{:keys [options
                arguments
                summary
                errors]
         :as parsed-opts} (parse-opts args
                                      (cli-options
                                       ;; if the verb is "validate-input", we
                                       ;; skip tools.cli validation and do a
                                       ;; more in-depth one.
                                       (not= "validate-input"
                                             (last args))))
        [?command] arguments]
    (cond (seq errors)
          (bail! errors)

          (:help options)
          (println summary)

          :else
          ;; At this point, we have valid individual inputs. However, there may
          ;; be cross-validation that needs to happen, so we compose the
          ;; comprehensive spec from the options and check that.
          (let [input (input/map->Input options)]
            (if-let [spec-error (input/validate input)]
              (bail! [(binding [s/*explain-out* expound/printer]
                       (expound/explain-result-str spec-error))])
              (if ?command
                (case ?command
                  ;; Where the CLI will actually perform generation
                  "generate" (println "{}")
                  ;; If they just want to validate and we're this far, we're done.
                  "validate-input" (println "Input is valid."))
                (do (println "No command entered.")
                    (println summary))))))))
