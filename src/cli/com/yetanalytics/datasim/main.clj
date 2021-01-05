(ns com.yetanalytics.datasim.main
  (:require [clojure.tools.cli :as cli :refer [parse-opts]]
            [clojure.spec.alpha :as s]
            [clojure.core.async :as a]
            [com.yetanalytics.datasim.input :as input]
            [expound.alpha :as expound]
            [com.yetanalytics.datasim.input.parameters :as params]
            [com.yetanalytics.datasim.runtime :as runtime]
            [com.yetanalytics.datasim.sim :as sim]
            [com.yetanalytics.datasim.xapi.client :as http]
            [clojure.pprint :refer [pprint]])
  (:import [java.util Random])
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
   [nil "--seed SEED" "Override input seed"
    :id :override-seed
    :parse-fn #(Integer/parseInt %)
    :desc "An integer seed to override the one in the input spec. Use -1 for random."]
   ;; POST options
   ["-E" "--endpoint URI" "LRS Endpoint for POST"
    :id :endpoint
    :desc "The xAPI endpoint of an LRS to POST to, ex: https://lrs.example.org/xapi"]
   ["-U" "--username URI" "LRS Basic auth username"
    :id :username
    :desc "The basic auth username for the LRS you wish to post to"]
   ["-P" "--password URI" "LRS Basic auth password"
    :id :password
    :desc "The basic auth password for the LRS you wish to post to"]
   ["-B" "--batch-size SIZE" "LRS POST batch size"
    :id :batch-size
    :default 25
    :parse-fn #(Integer/parseInt %)
    :desc "The batch size for POSTing to an LRS"]
   ["-C" "--concurrency CONC" "LRS POST concurrency"
    :id :concurrency
    :default 4
    :parse-fn #(Integer/parseInt %)
    :desc "The max concurrency of the LRS POST pipeline"]
   ["-L" "--post-limit LIMIT" "LRS POST total statement limit"
    :id :post-limit
    :default 999
    :parse-fn #(Integer/parseInt %)
    :desc "The total number of statements that will be sent to the LRS before termination. Overrides sim params. Set to -1 for no limit."]
   ["-A" "--[no-]async" "Async operation. Use --no-async if statements must be sent to server in timestamp order."
    :id :async
    :default true]

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

          (or (empty? args) (:help options))
          (println summary)

          :else
          ;; At this point, we have valid individual inputs. However, there may
          ;; be cross-validation that needs to happen, so we compose the
          ;; comprehensive spec from the options and check that.
          (let [sim-options (select-keys options
                                         [:input
                                          :profiles
                                          :personae
                                          :parameters
                                          :alignments])
                {:keys [override-seed]} options
                input (cond-> (or (:input sim-options) (input/map->Input sim-options))
                        override-seed
                        (assoc-in [:parameters :seed]
                                  (if (= -1 override-seed)
                                    (.nextLong (Random.))
                                    override-seed)))]
            (if-let [spec-error (input/validate input)]
              (bail! [(binding [s/*explain-out* expound/printer]
                        (expound/explain-result-str spec-error))])
              (if ?command
                (case ?command
                  ;; Where the CLI will actually perform generation
                  "generate"
                  (if (= "post" (first rest-args))
                    ;; Attempt to post to an LRS
                    (let [{:keys [endpoint
                                  username
                                  password
                                  batch-size
                                  concurrency
                                  post-limit
                                  async]} options
                          ]
                      (if endpoint
                        (let [post-options (cond-> {:endpoint endpoint
                                                    :batch-size batch-size}
                                             (and username password)
                                             (assoc-in [:http-options :basic-auth] [username password]))]
                          (if async
                            ;; ASYNC Operation
                            (let [sim-chan (sim/sim-chan
                                            (cond-> input
                                              ;; when async, we just use the post
                                              ;; limit as the max
                                              (not= post-limit -1)
                                              (assoc-in [:parameters :max] post-limit)
                                              ))
                                  result-chan (http/post-statements-async
                                               post-options
                                               sim-chan
                                               :concurrency concurrency)]

                              (loop []
                                (if-let [[tag ret] (a/<!! result-chan)]
                                  (case tag
                                    :fail
                                    (let [{:keys [status error]} ret]
                                      (bail! [(format "LRS Request FAILED with STATUS: %d, MESSAGE:%s"
                                                      status (or (some-> error ex-message) "<none>"))]))
                                    :success
                                    (do
                                      (doseq [^java.util.UUID id ret]
                                        (printf "%s\n" (.toString ^java.util.UUID id))
                                        (flush))
                                      (recur)))
                                  (System/exit 0))))
                            ;; SYNC operation
                            (let [statements (cond->> (sim/sim-seq input)
                                               (not= post-limit -1)
                                               (take post-limit))
                                  {:keys [success ;; Count of successfully transacted statements
                                          fail ;; list of failed requests
                                          ]
                                   :as post-results} (http/post-statements
                                                      post-options
                                                      statements
                                                      :emit-ids-fn
                                                      (fn [ids]
                                                        (doseq [^java.util.UUID id ids]
                                                          (printf "%s\n" (.toString id))
                                                          (flush))))]
                              (if (not-empty fail)
                                (bail! (for [{:keys [status error]} fail]
                                         (format "LRS Request FAILED with STATUS: %d, MESSAGE:%s"
                                                 status (or (some-> error ex-message) "<none>"))))
                                (System/exit 0)))))
                        ;; Endpoint is required when posting
                        (bail! ["-E / --endpoint REQUIRED for post."])))
                    ;; Stdout
                    (runtime/run-sim! input))

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
