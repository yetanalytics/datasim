(ns com.yetanalytics.datasim.test-containers
  (:require [clj-test-containers.core :as tc]))

(def server-validate-command
  ["/persephone/bin/server.sh" "validate"
   "-p" "dev-resources/profiles/cmi5/fixed.json"])

(def server-match-command
  ["/persephone/bin/server.sh" "match"
   "-p" "dev-resources/profiles/cmi5/fixed.json"])

(def container-map
  {:image-name    "yetanalytics/persephone:v0.9.1"
   :exposed-ports [8080]})

(def filesystem-map
  {:host-path      "dev-resources/"
   :container-path "/persephone/dev-resources/"
   :mode           :read-only})

(def validate-server-container
  (-> (assoc container-map :command server-validate-command)
      tc/create
      (tc/bind-filesystem! filesystem-map)))

(def match-server-container
  (-> (assoc container-map :command server-match-command)
      tc/create
      (tc/bind-filesystem! filesystem-map)))
