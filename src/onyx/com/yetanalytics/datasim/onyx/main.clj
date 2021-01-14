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
            [cider.nrepl :refer [cider-nrepl-handler]])
  (:import [java.net InetAddress]))

(def cli-options
  [;; Peer
   ["-n" "--n-vpeers N_VPEERS" "Number of VPEERS to launch. Overrides config value."
    :parse-fn #(Integer/parseInt %)]
   ;; Peer + Submit
   ["-t" "--tenancy-id TENANCY_ID" "Onyx Tenancy ID"]
   ;; Submit
   ["-i" "--input-loc INPUT_LOC" "DATASIM input location"]
   ["-e" "--endpoint ENDPOINT" "xAPI LRS Endpoint like https://lrs.example.org/xapi"]
   ["-u" "--username USERNAME" "xAPI LRS BASIC Auth username"]
   ["-p" "--password PASSWORD" "xAPI LRS BASIC Auth password"]
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
                      block]} options]
          (println "Starting job...")
          (let [{:keys [peer-config]} (-> (config/get-config)
                                          (assoc-in [:peer-config :onyx/tenancy-id] tenancy-id))
                submission (onyx.api/submit-job
                            peer-config
                            (job/config
                             {:input-json (slurp input-loc)
                              :lrs {:endpoint endpoint
                                    :username username
                                    :password password
                                    :batch-size 25}}))]
            (when block
              (println "blocking...")
              (flush)
              (onyx.api/await-job-completion peer-config (:job-id submission))
              (println "job complete!"))
            (clojure.pprint/pprint submission)
            (exit 0)))

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
