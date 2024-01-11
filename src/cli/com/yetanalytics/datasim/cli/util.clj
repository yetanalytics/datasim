(ns com.yetanalytics.datasim.cli.util
  (:require [clojure.tools.cli :as cli]
            [com.yetanalytics.datasim.util.io :as dio]))

(defn conj-input
  "Conj the input (either a Profile or a personae) to return a
   vector of inputs, e.g. `-p profile-1 -p profile-2` becomes
   `[profile-1 profile-2]`."
  [opt-map id v]
  (update opt-map id (fnil conj []) v))

(defn conj-param-input
  "Add a parameter named by id."
  [opt-map id v]
  (update-in opt-map [:parameters id] (fnil conj []) v))

(defn bail!
  "Print error messages to standard error and exit."
  [errors & {:keys [status]
             :or {status 1}}]
  (dio/println-err-coll errors)
  (System/exit status))

(defn exec-subcommand
  "Execute `exec-fn` for a subcommand with arguments `args`, where the
   valid options are `cli-options`."
  [cli-options exec-fn args]
  (let [{:keys [options summary errors]}
        (cli/parse-opts args cli-options)
        {:keys [help]}
        options
        errors* (not-empty errors)]
    (cond
      help    (println summary)
      errors* (bail! errors*)
      :else   (exec-fn options))))
