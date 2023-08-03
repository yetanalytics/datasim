(ns com.yetanalytics.datasim.main
  (:require [clojure.core.async :as a]
            [clojure.tools.cli  :as cli]
            [com.yetanalytics.datasim                  :as ds]
            [com.yetanalytics.datasim.client           :as client]
            [com.yetanalytics.datasim.input            :as input]
            [com.yetanalytics.datasim.input.parameters :as params]
            [com.yetanalytics.datasim.math.random      :as random]
            [com.yetanalytics.datasim.util.errors      :as errors]
            [com.yetanalytics.datasim.util.io          :as dio])
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLI Input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- conj-input
  "Conj the input (either a Profile or a personae) to return a
   vector of inputs, e.g. `-p profile-1 -p profile-2` becomes
   `[profile-1 profile-2]`."
  [opt-map id v]
  (update opt-map id (fnil conj []) v))

(defn- conj-param-input
  "Add a parameter named by id."
  [opt-map id v]
  (update-in opt-map [:parameters id] (fnil conj []) v))

(defn cli-options
  "Generate CLI options, skipping validation if `validate?` is false"
  [validate?]
  [["-p" "--profile URI" "xAPI Profile Location"
    :id :profiles
    :desc "The location of an xAPI profile, can be used multiple times."
    :parse-fn (partial input/from-location :profile :json)
    :validate (if validate?
                [(partial input/validate-throw :profile)
                 "Failed to validate profile."]
                [])
    :assoc-fn conj-input]
   ["-a" "--actor-personae URI" "Actor Personae Location"
    :id :personae-array
    :desc "The location of an Actor Personae document indicating the actors in the sim."
    :parse-fn (partial input/from-location :personae :json)
    :validate (if validate?
                [(partial input/validate-throw :personae)
                 "Failed to validate personae."]
                [])
    :assoc-fn conj-input]
   ["-m" "--models URI" "Persona Model Location"
    :id :models
    :desc "The location of an Persona Model document, to describe alignments and overrides for the personae."
    :parse-fn (partial input/from-location :models :json)
    :validate (if validate?
                [(partial input/validate-throw :models)
                 "Failed to validate Models."]
                [])]
   ["-o" "--parameters URI" "Sim Parameters Location"
    :id :parameters
    :desc "The location of a Sim Parameters Document."
    :parse-fn (partial input/from-location :parameters :json)
    :validate (if validate?
                [(partial input/validate-throw :parameters)
                 "Failed to validate Parameters."]
                [])
    :default (params/apply-defaults)]

   ["-i" "--input URI" "Combined Simulation input"
    :id :input
    :desc "The location of a JSON file containing a combined simulation input spec."
    :parse-fn (partial input/from-location :input :json)
    :validate (if validate?
                [(partial input/validate-throw :input)
                 "Failed to validate input."]
                [])]
   [nil "--seed SEED" "Override input seed"
    :id :override-seed
    :parse-fn parse-long
    :validate [int? "Seed is not an integer."]
    :desc "An integer seed to override the one in the input spec. Use -1 for random."]
   [nil "--actor AGENT_ID" "Select actor(s) by agent ID"
    :id :select-agents
    :multi true
    :update-fn (fnil conj #{})
    :desc "Pass an agent id in the format mbox::malto:bob@example.org to select actor(s)"]
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
    :parse-fn parse-long
    :validate [int? "Batch size is not an integer."]
    :desc "The batch size for POSTing to an LRS"]
   ["-C" "--concurrency CONC" "LRS POST concurrency"
    :id :concurrency
    :default 4
    :parse-fn parse-long
    :validate [int? "Concurrency is not an integer."]
    :desc "The max concurrency of the LRS POST pipeline"]
   ["-L" "--post-limit LIMIT" "LRS POST total statement limit"
    :id :post-limit
    :default 999
    :parse-fn parse-long
    :validate [int? "POST statement limit is not an integer."]
    :desc "The total number of statements that will be sent to the LRS before termination. Overrides sim params. Set to -1 for no limit."]
   ["-A" "--[no-]async" "Async operation. Use --no-async if statements must be sent to server in timestamp order."
    :id :async
    :default true]
   [nil "--gen-profile IRI" "Only generate based on primary patterns in the given profile. May be given multiple times to include multiple profiles."
    :id :gen-profiles
    :assoc-fn conj-param-input]
   [nil "--gen-pattern IRI" "Only generate based on the given primary pattern. May be given multiple times to include multiple patterns."
    :id :gen-patterns
    :assoc-fn conj-param-input]
   ;; Help
   ["-h" "--help"]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLI Run
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bail!
  "Print error messages to std error and exit."
  [errors & {:keys [status]
             :or {status 1}}]
  (dio/println-err-coll errors)
  (System/exit status))

(defn- sim-input [options]
  (let [sim-options (select-keys options [:input
                                          :profiles
                                          :personae-array
                                          :parameters
                                          :models])
        {:keys [override-seed]} options]
    (cond-> (or (:input sim-options)
                (dissoc sim-options :input))
      override-seed
      (assoc-in [:parameters :seed]
                (if (= -1 override-seed)
                  (random/rand-unbound-int (random/rng))
                  override-seed)))))

;; When this is called, we should have valid individual inputs. However, there
;; may be cross-validation that needs to happen, so we compose the
;; comprehensive spec from the options and check that.
(defn- assert-valid-input [input]
  (when-let [errors (not-empty (input/validate :input input))]
    (bail! (errors/map-coll->strs errors))))

;; POST to LRS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- post-async!
  [input post-options post-limit select-agents concurrency]
  (let [gen-input   (cond-> input
                      ;; when async, we just use the post
                      ;; limit as the max
                      (not= post-limit -1)
                      (assoc-in [:parameters :max] post-limit))
        sim-chan    (ds/generate-seq-async
                     gen-input
                     :select-agents select-agents)
        result-chan (client/post-statements-async
                     post-options
                     sim-chan
                     :concurrency concurrency)]
    (loop []
      (when-let [[tag ret] (a/<!! result-chan)]
        (case tag
          :fail
          (let [{:keys [status error]} ret]
            (bail! [(client/post-error-message status error)]))
          :success
          (do
            (dio/println-coll ret) ; Statement ID strings
            (recur)))))))

(defn- post-sync!
  [input post-options post-limit select-agents]
  (let [statements     (cond->> (ds/generate-seq
                                 input
                                 :select-agents select-agents)
                         (not= post-limit -1)
                         (take post-limit))
        {:keys [fail]} (client/post-statements post-options statements)]
    (when (not-empty fail)
      (bail! (for [{:keys [status error]} fail]
               (client/post-error-message status error))))))

(defn- post-sim!
  [input options]
  (let [{:keys [endpoint
                username
                password
                batch-size
                concurrency
                post-limit
                select-agents
                async]}
        options]
    ;; Endpoint is required when posting
    (when-not endpoint
      (bail! ["-E / --endpoint REQUIRED for post."]))
    ;; Endpoint present - OK
    (let [post-options {:endpoint   endpoint
                        :batch-size batch-size
                        :username   username
                        :password   password}]
      (if async
        (post-async! input post-options post-limit select-agents concurrency)
        (post-sync! input post-options post-limit select-agents)))))

;; Print sim to stdout ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- print-sim!
  "Generate statement seqs and writes them to stdout."
  [input {:keys [select-agents]}]
  (doseq [statement (ds/generate-seq input :select-agents select-agents)]
    (dio/write-json-stdout statement :key-fn? false)))

;; Write Input mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- write-input!
  ([input]
   (input/to-out input :json))
  ([input location]
   (input/to-file input :json location)
   (println (format "Input specification written to %s" location))))

;; Main function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main [& args]
  (let [;; If the verb is "validate-input", we
        ;; skip tools.cli validation and do a
        ;; more in-depth one.
        cli-opts
        (cli-options (not= "validate-input" (last args)))
        {:keys [options arguments summary errors]}
        (cli/parse-opts args cli-opts)
        [?command & rest-args]
        arguments]
    (cond
      ;; Invalid CLI input
      (seq errors)
      (bail! errors)
      ;; Help
      (or (empty? args) (:help options))
      (println summary)
      :else
      (let [input (sim-input options)]
        (assert-valid-input input)
        (case ?command
          ;; Where the CLI will actually perform generation
          "generate"
          (if (= "post" (first rest-args))
            (post-sim! input options)
            (print-sim! input options))
          ;; If they just want to validate and we're this far, we're done.
          ;; Just return the input spec as JSON.
          "validate-input"
          (if-some [location (first rest-args)]
            (write-input! input location)
            (write-input! input))
          ;; No command
          (do (println "No command entered.")
              (println summary)))))))
