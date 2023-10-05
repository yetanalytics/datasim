(ns com.yetanalytics.datasim.cli.generate
  "CLI options and functions for statement generation (including
   statement POSTing)."
  (:require [clojure.core.async :as a]
            [com.yetanalytics.datasim           :as ds]
            [com.yetanalytics.datasim.cli.util  :as u]
            [com.yetanalytics.datasim.cli.input :as cli-input]
            [com.yetanalytics.datasim.client    :as client]
            [com.yetanalytics.datasim.util.io   :as dio]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLI Input Options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def seed-desc
  "An integer seed to override the one in the input spec. Use -1 for random.")

(def select-agent-desc
  "Pass an agent IFI in the format 'mbox::malito:[email]' to select actor(s)")

(def gen-profiles-desc
  "Only generate based on primary patterns in the given profile. May be given multiple times to include multiple profiles.")

(def gen-patterns-desc
  "Only generate based on the given primary pattern. May be given multiple times to include multiple patterns.")

(def generate-options
  [[nil "--seed SEED" "Input seed"
    :id       :override-seed
    :parse-fn parse-long
    :validate [int? "Seed is not an integer."]
    :desc     seed-desc]
   [nil "--actor AGENT_IFI" "Selected Actor IFIs"
    :id        :select-agents
    :multi     true
    :update-fn (fnil conj #{})
    :desc      select-agent-desc]
   [nil "--gen-profile IRI" "Select Profile IRIs"
    :id      :gen-profiles
    :assoc-fn u/conj-param-input
    :desc     gen-profiles-desc]
   [nil "--gen-pattern IRI" "Select Pattern IRIs"
    :id       :gen-patterns
    :assoc-fn u/conj-param-input
    :desc     gen-patterns-desc]])

(def endpoint-desc
  "The xAPI endpoint of an LRS to POST to, ex: https://lrs.example.org/xapi")

(def endpoint-missing
  "[-E|--endpoint] argument is required for POST.")

(def username-desc
  "The Basic Auth username for the LRS.")

(def password-desc
  "The Basic Auth password for the LRS.")

(def batch-size-desc
  "The batch size, i.e. how many statements to send at a time, for POSTing.")

(def concurrency-desc
  "The max concurrency of the LRS POST pipeline.")

(def post-limit-desc
  "The total number of statements that will be sent to the LRS before termination. Overrides sim params. Set to -1 for no limit.")

(def post-options
  [["-E" "--endpoint URI" "LRS Endpoint for POST"
    :id      :endpoint
    :desc    endpoint-desc
    :missing endpoint-missing]
   ["-U" "--username URI" "LRS username"
    :id :username
    :desc username-desc]
   ["-P" "--password URI" "LRS password"
    :id :password
    :desc password-desc]
   ["-B" "--batch-size SIZE" "LRS POST batch size"
    :id       :batch-size
    :default  25
    :parse-fn parse-long
    :validate [int? "Batch size is not an integer."]
    :desc     batch-size-desc]
   ["-C" "--concurrency CONC" "LRS POST concurrency"
    :id       :concurrency
    :default  4
    :parse-fn parse-long
    :validate [int? "Concurrency is not an integer."]
    :desc     concurrency-desc]
   ["-L" "--post-limit LIMIT" "LRS POST total statement limit"
    :id       :post-limit
    :default  999
    :parse-fn parse-long
    :validate [int? "POST statement limit is not an integer."]
    :desc     post-limit-desc]
   ["-A" "--[no-]async" "Async operation. Use --no-async if statements must be sent to server in timestamp order."
    :id      :async
    :default true]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- print-sim!
  [input {:keys [select-agents]}]
  (doseq [statement (ds/generate-seq input :select-agents select-agents)]
    (dio/write-json-stdout statement :key-fn? false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate POST Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
            (u/bail! [(client/post-error-message status error)]))
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
      (u/bail! (for [{:keys [status error]} fail]
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
        options
        post-options
        {:endpoint   endpoint
         :batch-size batch-size
         :username   username
         :password   password}]
    (if async
      (post-async! input post-options post-limit select-agents concurrency)
      (post-sync! input post-options post-limit select-agents))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subcommands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generate
  "Generate statements based on simulation `args` and print them to stdout."
  [args]
  (u/exec-subcommand
   (concat cli-input/input-options
           generate-options
           [["-h" "--help"]])
   (fn [options]
     (let [input (cli-input/sim-input options)]
       (cli-input/assert-valid-input input)
       (print-sim! input options)))
   args))

(defn generate-post
  "Generate statements based on simulation `args` and POST them to an LRS
   (whose endpoint and other properties are also in `args`)."
  [args]
  (u/exec-subcommand
   (concat cli-input/input-options
           generate-options
           post-options
           [["-h" "--help"]])
   (fn [options]
     (let [input (cli-input/sim-input options)]
       (cli-input/assert-valid-input input)
       (post-sim! input options)))
   args))
