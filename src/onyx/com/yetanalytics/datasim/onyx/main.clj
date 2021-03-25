(ns com.yetanalytics.datasim.onyx.main
  (:gen-class)
  (:require [onyx.plugin.http-output :as http]
            onyx.plugin.seq
            onyx.api
            [com.yetanalytics.datasim.onyx.job :as job]
            [com.yetanalytics.datasim.onyx.seq :as dseq]
            [com.yetanalytics.datasim.onyx.config :as config]
            [com.yetanalytics.datasim.onyx.peer :as peer]
            [com.yetanalytics.datasim.onyx.aeron-media-driver :as driver]
            [com.yetanalytics.datasim.onyx.repl :as repl]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [nrepl.server :as nrepl]
            [cider.nrepl :refer [cider-nrepl-handler]]
            [clojure.pprint :refer [pprint]])
  (:import [java.net InetAddress]))

(def cli-options
  [;; Peer
   ["-n" "--n-vpeers N_VPEERS" "Number of VPEERS to launch. Overrides config value."
    :parse-fn #(Integer/parseInt %)]
   ;; Peer + Submit
   ["-t" "--tenancy-id TENANCY_ID" "Onyx Tenancy ID"]
   ;; Submit
   ["-i" "--input-loc INPUT_LOC" "DATASIM input location"]
   ["-c" "--concurrency CONCURRENCY" "Desired concurrency of job."
    :default 1
    :parse-fn #(Integer/parseInt %)]
   [nil "--lrs-batch-size LRS_BATCH_SIZE" "Statements per LRS POST"
    :default 500 ;; minimal effienct size for apiw + kinesis, at worst 260K for MOM profile
    :parse-fn #(Integer/parseInt %)]
   [nil "--onyx-batch-size ONYX_BATCH_SIZE" "Onyx internal batch size"
    :default 1 ;; we batch out own
    :parse-fn #(Integer/parseInt %)]
   [nil "--retry-base-sleep-ms RETRY_BASE_SLEEP_MS" "Backoff retry sleep time"
    :default 500 ;; real cool about it
    :parse-fn #(Integer/parseInt %)]
   [nil "--retry-max-sleep-ms RETRY_MAX_SLEEP_MS" "Backoff retry sleep time max per retry."
    :default 30000 ;; every 30 sec max
    :parse-fn #(Integer/parseInt %)]
   [nil "--retry-max-total-sleep-ms RETRY_MAX_TOTAL_SLEEP_MS" "Backoff retry sleep time max total."
    :default 3600000 ;; an hour, why not
    :parse-fn #(Integer/parseInt %)]

   ["-m" "--override-max OVERRIDE_MAX" "Override max statements"
    :parse-fn #(Integer/parseInt %)]
   ["-e" "--endpoint ENDPOINT" "xAPI LRS Endpoint like https://lrs.example.org/xapi"]
   ["-u" "--username USERNAME" "xAPI LRS BASIC Auth username"]
   ["-p" "--password PASSWORD" "xAPI LRS BASIC Auth password"]
   [nil "--x-api-key X_API_KEY" "API Gateway API key"]
   [nil "--[no-]strip-ids" "Strip IDs from generated statements" :default false]
   [nil "--[no-]remove-refs" "Filter out statement references" :default false]
   ["-b" "--[no-]block" "Block until the job is done" :default true]
   [nil "--nrepl-bind NREPL_BIND" "If provided on peer launch will start an nrepl server bound to this address"
    :default "0.0.0.0"]
   [nil "--nrepl-port NREPL_PORT" "If provided on peer launch will start an nrepl server on this port"
    :parse-fn #(Integer/parseInt %)]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["DATASIM Cluster CLI"
        ""
        "Usage: bin/onyx.sh [options] action"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  start-peer    Start an onyx peer"
        "  start-driver  Start an aeron media driver"
        "  submit-job    Submit a datasim input for submission to an LRS"
        "  repl          Start a local repl"
        ]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      ;; custom validation on arguments
      (and (= 1 (count arguments))
           (#{"start-peer"
              "start-driver"
              "submit-job"
              "repl"} (first arguments)))
      {:action (first arguments) :options options}
      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [action options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (case action
        "submit-job"
        (let [{:keys [tenancy-id
                      input-loc
                      endpoint
                      username
                      password
                      block
                      concurrency
                      lrs-batch-size
                      onyx-batch-size
                      strip-ids
                      remove-refs
                      x-api-key
                      override-max
                      retry-base-sleep-ms
                      retry-max-sleep-ms
                      retry-max-total-sleep-ms]} options]
          (println "Starting job...")
          (let [{:keys [peer-config]} (cond-> (config/get-config)
                                        tenancy-id (assoc-in [:peer-config :onyx/tenancy-id] tenancy-id))
                job-config (job/config
                            {:input-json (slurp input-loc)
                             :concurrency concurrency
                             :batch-size onyx-batch-size
                             :strip-ids? strip-ids
                             :remove-refs? remove-refs
                             :override-max override-max
                             :retry-params
                             {:base-sleep-ms retry-base-sleep-ms
                              :max-sleep-ms retry-max-sleep-ms
                              :max-total-sleep-ms retry-max-total-sleep-ms}
                             :lrs {:endpoint endpoint
                                   :username username
                                   :password password
                                   :x-api-key x-api-key
                                   :batch-size lrs-batch-size}})
                _ (pprint {:job-config (update job-config
                                               :lifecycles
                                               (fn [ls]
                                                 (mapv (fn [lc]
                                                         (if (:com.yetanalytics.datasim.onyx.seq/input-json lc)
                                                           (assoc lc :com.yetanalytics.datasim.onyx.seq/input-json "<json>")
                                                           lc))
                                                       ls)))})
                submission (onyx.api/submit-job
                            peer-config
                            job-config)]
            (println "submitted!")
            (clojure.pprint/pprint submission)
            (when block
              (println "blocking...")
              (flush)
              (println "job complete!"
                       (onyx.api/await-job-completion peer-config (:job-id submission))))

            (exit 0 "OK")))

        "start-peer"
        (let [{:keys [tenancy-id
                      n-vpeers
                      nrepl-port
                      nrepl-bind]} options]
          (println "Preparing to start peers...")
          (when n-vpeers
            (println "Overriding number of virtual peers to " n-vpeers))
          (when nrepl-port
            (println "Starting Nrepl on port " nrepl-port)
            (nrepl/start-server
             :bind nrepl-bind
             :port nrepl-port
             :handler cider-nrepl-handler))
          (peer/start-peer!
           ;; Config overrides
           (cond-> (config/get-config)
             tenancy-id
             (->
              (assoc-in [:env-config :onyx/tenancy-id] tenancy-id)
              (assoc-in [:peer-config :onyx/tenancy-id] tenancy-id))
             n-vpeers
             (assoc-in [:launch-config :n-vpeers] n-vpeers))))
        "start-driver"
        (driver/start-driver!)
        "repl"
        (repl/repl!)))))
