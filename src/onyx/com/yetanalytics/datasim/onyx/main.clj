(ns com.yetanalytics.datasim.onyx.main
  (:gen-class)
  (:require [clojure.core.async :as a :refer [chan]]
            [onyx.plugin.core-async :refer [take-segments!]]
            [onyx.plugin.http-output :as http]
            [onyx.api]

            [com.yetanalytics.datasim.sim :as sim]
            [com.yetanalytics.datasim.input :as input]
            [com.yetanalytics.datasim.util.xapi :as xapiu]
            [cheshire.core :as json]))

#_(defn my-inc [{:keys [n] :as segment}]
    (assoc segment :n (inc n)))

(defn partition-actors [{:keys [input] :as segment}]
  ;; Naive partition by actor
  (for [actor-id (map xapiu/agent-id
                       (get-in input [:personae :member]))]
    (assoc segment :actor-id actor-id)))

(defn gen [{:keys [input actor-id lrs]}]
  ;; Generate batches suitable for LRS POST
  (let [{:keys [endpoint batch-size]} lrs]
    (map (fn [statements]
           {:url endpoint
            :args
            {:headers {"X-Experience-API-Version" "1.0.3"
                       "Content-Type" "application/json"}
             :body (json/generate-string (into [] statements))
             :as :json}})
         (partition-all batch-size (sim/sim-seq input :select-agents [actor-id])))))

(defn post-success?
  [{:keys [status]}]
  (= 200 status))

(def workflow
  [[:in :partition-actors]
   [:partition-actors :gen]
   [:gen :out]])

(def capacity 1000)

(defonce input-chan (chan capacity))
(defonce input-buffer (atom {}))

(defonce output-chan (chan capacity))

(def batch-size 10)

(def catalog
  [{:onyx/name :in
    :onyx/plugin :onyx.plugin.core-async/input
    :onyx/type :input
    :onyx/medium :core.async
    :onyx/max-peers 1
    :onyx/batch-size batch-size
    :onyx/doc "Reads segments from a core.async channel"}

   {:onyx/name :partition-actors
    :onyx/fn ::partition-actors
    :onyx/type :function
    :onyx/batch-size batch-size}

   {:onyx/name :gen
    :onyx/fn ::gen
    :onyx/type :function
    :onyx/batch-size batch-size}

   {:onyx/name :out
    :onyx/plugin :onyx.plugin.http-output/output
    :onyx/type :output
    :onyx/medium :http
    :http-output/success-fn ::post-success?
    :http-output/retry-params {:base-sleep-ms 200
                               :max-sleep-ms 30000
                               :max-total-sleep-ms 3600000}
    :onyx/batch-size batch-size
    :onyx/doc "POST statements to http endpoint"}
   #_{:onyx/name :out
    :onyx/plugin :onyx.plugin.core-async/output
    :onyx/type :output
    :onyx/medium :core.async
    :onyx/max-peers 1
    :onyx/batch-size batch-size
    :onyx/doc "Writes segments to a core.async channel"}])

(defn inject-in-ch [event lifecycle]
  {:core.async/buffer input-buffer ;; TODO: figure out why this is an atom
   :core.async/chan input-chan})

#_(defn inject-out-ch [event lifecycle]
  {:core.async/chan output-chan})

(def in-calls
  {:lifecycle/before-task-start inject-in-ch})

#_(def out-calls
  {:lifecycle/before-task-start inject-out-ch})

(def lifecycles
  [{:lifecycle/task :in
    :lifecycle/calls :com.yetanalytics.datasim.onyx.main/in-calls}
   {:lifecycle/task :in
    :lifecycle/calls :onyx.plugin.core-async/reader-calls}
   #_{:lifecycle/task :out
    :lifecycle/calls :com.yetanalytics.datasim.onyx.main/out-calls}
   #_{:lifecycle/task :out
    :lifecycle/calls :onyx.plugin.core-async/writer-calls}])

#_(def submission)

(defn -main
  [& args]
  ;; VERY MUCH JUST DEV RIGHT NOW
  (let [;; DEV ENV config, should go away for prod things
        id (java.util.UUID/randomUUID)

        env-config
        {:zookeeper/address "127.0.0.1:2188"
         :zookeeper/server? true
         :zookeeper.server/port 2188
         :onyx/tenancy-id id}

        env (onyx.api/start-env env-config)

        ;; Peer config: should be there at peer launch + job submission

        peer-config
        {:zookeeper/address "127.0.0.1:2188"
         :onyx/tenancy-id id
         :onyx.peer/job-scheduler :onyx.job-scheduler/balanced
         :onyx.messaging/impl :aeron
         :onyx.messaging/peer-port 40200
         :onyx.messaging/bind-addr "localhost"}

        ;; start peer group
        peer-group (onyx.api/start-peer-group peer-config)

        n-peers (count (set (mapcat identity workflow)))

        v-peers (onyx.api/start-peers n-peers peer-group)]


    (a/>!! input-chan
           ;; input segments (for now) are an input + some params
           {:input (input/from-location :input :json "dev-resources/input/simple.json")
            :lrs {:endpoint "http://localhost:8080/xapi/statements"
                  :batch-size 25}}
           )
    (a/close! input-chan)

    (let [submission   (onyx.api/submit-job peer-config
                                            {:catalog catalog
                                             :workflow workflow
                                             :lifecycles lifecycles
                                             :task-scheduler :onyx.task-scheduler/balanced})
          _ (onyx.api/await-job-completion peer-config (:job-id submission))
          #_#_results (take-segments! output-chan 50)
          ]
      #_(clojure.pprint/pprint results))

    (doseq [v-peer v-peers]
      (onyx.api/shutdown-peer v-peer))

    (onyx.api/shutdown-peer-group peer-group)

    (onyx.api/shutdown-env env)))







(comment
  (clojure.pprint/pprint (input/from-location :input :json "dev-resources/input/simple.json"))

  )











(comment
  (ns com.yetanalytics.datasim.onyx.main
    (:gen-class)
    (:require [aero.core :refer [read-config]]
              [clojure.java.io :as io]
              [clojure.tools.cli :refer [parse-opts]]
              [lib-onyx.peer :as peer]
              [onyx.job]
              [onyx.api]
              [onyx.test-helper]
              ;; Load plugin classes on peer start
              [onyx.plugin [core-async]]
              ;; Load our tasks
              [com.yetanalytics.datasim.onyx.tasks [math]]
              ;; Load our jobs
              [com.yetanalytics.datasim.onyx.jobs [basic]]))

  (defn file-exists?
    "Check both the file system and the resources/ directory
  on the classpath for the existence of a file"
    [file]
    (let [f (clojure.string/trim file)
          classf (io/resource file)
          relf (when (.exists (io/as-file f)) (io/as-file f))]
      (or classf relf)))

  (defn cli-options []
    [["-c" "--config FILE" "Aero/EDN config file"
      :default (io/resource "onyx-config.edn")
      :default-desc "resources/onyx-config.edn"
      :parse-fn file-exists?
      :validate [identity "File does not exist relative to the workdir or on the classpath"
                 read-config "Not a valid Aero or EDN file"]]

     ["-p" "--profile PROFILE" "Aero profile"
      :parse-fn (fn [profile] (clojure.edn/read-string (clojure.string/trim profile)))]

     ["-h" "--help"]])

  (defn usage [options-summary]
    (->> ["Onyx Peer and Job Launcher"
          ""
          "Usage: [options] action [arg]"
          ""
          "Options:"
          options-summary
          ""
          "Actions:"
          "  start-peers [npeers]    Start Onyx peers."
          "  submit-job  [job-name]  Submit a registered job to an Onyx cluster."
          ""]
         (clojure.string/join \newline)))

  (defn error-msg [errors]
    (str "The following errors occurred while parsing your command:\n\n"
         (clojure.string/join \newline errors)))

  (defn exit [status msg]
    (println msg)
    (System/exit status))

  (defn assert-job-exists [job-name]
    (let [jobs (methods onyx.job/register-job)]
      (when-not (contains? jobs job-name)
        (exit 1 (error-msg (into [(str "There is no job registered under the name " job-name "\n")
                                  "Available jobs: "] (keys jobs)))))))

  (defn -main [& args]
    (let [{:keys [options arguments errors summary] :as pargs} (parse-opts args (cli-options))
          action (first args)
          argument (clojure.edn/read-string (second args))]
      (cond (:help options) (exit 0 (usage summary))
            (not= (count arguments) 2) (exit 1 (usage summary))
            errors (exit 1 (error-msg errors)))
      (case action
        "start-peers" (let [{:keys [env-config peer-config] :as config}
                            (read-config (:config options) {:profile (:profile options)})]
                        (peer/start-peer argument peer-config env-config))

        "submit-job" (let [{:keys [peer-config] :as config}
                           (read-config (:config options) {:profile (:profile options)})
                           job-name (if (keyword? argument) argument (str argument))]
                       (assert-job-exists job-name)
                       (let [job-id (:job-id
                                     (onyx.api/submit-job peer-config
                                                          (onyx.job/register-job job-name config)))]
                         (println "Successfully submitted job: " job-id)
                         (println "Blocking on job completion...")
                         (onyx.test-helper/feedback-exception! peer-config job-id)
                         (exit 0 "Job Completed"))))))
  )
