(ns com.yetanalytics.datasim.main
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.input :as input]
            [expound.alpha :as expound]
            [com.yetanalytics.datasim.input.parameters :as params]
            [com.yetanalytics.datasim.runtime :as runtime]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(defn cli-options
  "Generate CLI options, skipping validation if `validate?` is false"
  [validate?]
  [["-p" "--profile URI" "xAPI Profile Location"
    :id :profiles
    :desc "The location of an xAPI profile, can be used multiple times."
    :parse-fn (partial input/from-location :profile :json)
    :validate (if validate?
                [input/validate-throw "Failed to validate profile."]
                [])
    :assoc-fn (fn [omap id v]
                (update omap
                        id
                        (fnil conj [])
                        v))]
   ["-a" "--actor-personae URI" "Actor Personae Location"
    :id :personae
    :desc "The location of an Actor Personae document indicating the actors in the sim."
    :parse-fn (partial input/from-location :personae :json)
    :validate (if validate?
                [input/validate-throw "Failed to validate personae."]
                [])]
   ["-l" "--alignments URI" "Actor Alignments Location"
    :id :alignments
    :desc "The location of an Actor Alignments Document."
    :parse-fn (partial input/from-location :alignments :json)
    :validate (if validate?
                [input/validate-throw "Failed to validate Alignments."]
                [])]
   ["-o" "--parameters URI" "Sim Parameters Location"
    :id :parameters
    :desc "The location of a Sim Parameters Document."
    :parse-fn (partial input/from-location :parameters :json)
    :validate (if validate?
                [input/validate-throw "Failed to validate Parameters."]
                [])
    ;; TODO: it looks like, when the validation is skipped, a simple empty
    ;; default doesn't work here, as the full input spec fails.
    ;; For now we just hack it by calling the defaults fn directly.
    :default (params/add-defaults {})]

   ["-i" "--input URI" "Combined Simulation input"
    :id :input
    :desc "The location of a JSON file containing a combined simulation input spec."
    :parse-fn (partial input/from-location :input :json)
    :validate (if validate?
                [input/validate-throw "Failed to validate input."]
                [])
    ]

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
        [?command & rest-args] arguments]
    (cond (seq errors)
          (bail! errors)

          (:help options)
          (println summary)

          :else
          ;; At this point, we have valid individual inputs. However, there may
          ;; be cross-validation that needs to happen, so we compose the
          ;; comprehensive spec from the options and check that.
          (let [input (or (:input options) (input/map->Input options))]
            (if-let [spec-error (input/validate input)]
              (bail! [(binding [s/*explain-out* expound/printer]
                       (expound/explain-result-str spec-error))])
              (if ?command
                (case ?command
                  ;; Where the CLI will actually perform generation
                  "generate" (runtime/run-sim! input)
                  ;; If they just want to validate and we're this far, we're done.
                  ;; Just return the input spec as JSON
                  "validate-input"
                  (let [[location] rest-args]
                    (if location
                      (do (input/to-file input :json location)
                          (println (format "Input specification written to %s" location)))
                      ;; TODO: Figure out why we get a stream closed error here
                      (input/to-out input :json))))
                (do (println "No command entered.")
                    (println summary))))))))
