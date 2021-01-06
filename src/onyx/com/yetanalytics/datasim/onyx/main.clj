(ns com.yetanalytics.datasim.onyx.main
  (:gen-class)
  (:require [clojure.core.async :refer [chan >!! <!! close!]]
            [onyx.plugin.core-async :refer [take-segments!]]
            [onyx.api]))

(defn my-inc [{:keys [n] :as segment}]
  (assoc segment :n (inc n)))

(def workflow
  [[:in :inc]
   [:inc :out]])

(def capacity 1000)

(def input-chan (chan capacity))
(def input-buffer (atom {}))

(def output-chan (chan capacity))

(def batch-size 10)

(def catalog
  [{:onyx/name :in
    :onyx/plugin :onyx.plugin.core-async/input
    :onyx/type :input
    :onyx/medium :core.async
    :onyx/max-peers 1
    :onyx/batch-size batch-size
    :onyx/doc "Reads segments from a core.async channel"}

   {:onyx/name :inc
    :onyx/fn :com.yetanalytics.datasim.onyx.main/my-inc
    :onyx/type :function
    :onyx/batch-size batch-size}

   {:onyx/name :out
    :onyx/plugin :onyx.plugin.core-async/output
    :onyx/type :output
    :onyx/medium :core.async
    :onyx/max-peers 1
    :onyx/batch-size batch-size
    :onyx/doc "Writes segments to a core.async channel"}])

(def input-segments
  [{:n 0}
   {:n 1}
   {:n 2}
   {:n 3}
   {:n 4}
   {:n 5}])

(doseq [segment input-segments]
  (>!! input-chan segment))

(close! input-chan)

(def id (java.util.UUID/randomUUID))

(def env-config
  {:zookeeper/address "127.0.0.1:2188"
   :zookeeper/server? true
   :zookeeper.server/port 2188
   :onyx/tenancy-id id})

(def peer-config
  {:zookeeper/address "127.0.0.1:2188"
   :onyx/tenancy-id id
   :onyx.peer/job-scheduler :onyx.job-scheduler/balanced
   :onyx.messaging/impl :aeron
   :onyx.messaging/peer-port 40200
   :onyx.messaging/bind-addr "localhost"})

(def env (onyx.api/start-env env-config))

(def peer-group (onyx.api/start-peer-group peer-config))

(def n-peers (count (set (mapcat identity workflow))))

(def v-peers (onyx.api/start-peers n-peers peer-group))

(defn inject-in-ch [event lifecycle]
  {:core.async/buffer input-buffer
   :core.async/chan input-chan})

(defn inject-out-ch [event lifecycle]
  {:core.async/chan output-chan})

(def in-calls
  {:lifecycle/before-task-start inject-in-ch})

(def out-calls
  {:lifecycle/before-task-start inject-out-ch})

(def lifecycles
  [{:lifecycle/task :in
    :lifecycle/calls :com.yetanalytics.datasim.onyx.main/in-calls}
   {:lifecycle/task :in
    :lifecycle/calls :onyx.plugin.core-async/reader-calls}
   {:lifecycle/task :out
    :lifecycle/calls :com.yetanalytics.datasim.onyx.main/out-calls}
   {:lifecycle/task :out
    :lifecycle/calls :onyx.plugin.core-async/writer-calls}])

(def submission)

(defn -main
  [& args]
  (let [submission   (onyx.api/submit-job peer-config
                         {:catalog catalog
                          :workflow workflow
                          :lifecycles lifecycles
                          :task-scheduler :onyx.task-scheduler/balanced})
        _ (onyx.api/await-job-completion peer-config (:job-id submission))
        results (take-segments! output-chan 50)]
    (clojure.pprint/pprint results))

  (doseq [v-peer v-peers]
    (onyx.api/shutdown-peer v-peer))

  (onyx.api/shutdown-peer-group peer-group)

  (onyx.api/shutdown-env env))



















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
