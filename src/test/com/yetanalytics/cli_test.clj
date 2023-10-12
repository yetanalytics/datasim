(ns com.yetanalytics.cli-test
  "Integration tests for the DATASIM CLI."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as cstr]
            [org.httpkit.client :as http]
            [clj-test-containers.core :as tc]
            [com.yetanalytics.datasim.input :as input]
            [com.yetanalytics.datasim.cli.input :as cli-input]
            [com.yetanalytics.datasim.cli.generate :as cli-gen]
            [com.yetanalytics.datasim.test-containers :as ds-tc]))

(def json-post-header
  {"X-Experience-Api-Version" "1.0.3"
   "Content-Type" "application/json"})

(deftest validate-input-test
  (testing "validate-input subcommand"
    (cli-input/validate-input!
     ["-p" "dev-resources/profiles/cmi5/fixed.json"
      "-a" "dev-resources/personae/simple.json"
      "-m" "dev-resources/models/simple.json"
      "-o" "dev-resources/parameters/simple.json"
      "-v" "dev-resources/input/simple2.json"])
    (let [input* (input/from-location
                  :input :json
                  "dev-resources/input/simple2.json")]
      (is (nil? (input/validate :input input*))))))

(deftest generate-test
  (let [cont (tc/start! ds-tc/validate-server-container)
        host (:host cont)
        port (get (:mapped-ports cont) 8080)]
    (testing "generate subcommand"
      (let [results
            (with-out-str
              (cli-gen/generate!
               ["-i" "dev-resources/input/simple.json"]))]
        (is (string? results))
        (is (every?
             (fn [stmt-str]
               (let [validate-res
                     #_{:clj-kondo/ignore [:unresolved-var]}
                     @(http/post
                       (format "http://%s:%d/statements" host port)
                       {:headers json-post-header
                        :body    stmt-str
                        :as      :stream})]
                 (= 204 (:status validate-res))))
             (take 25 (cstr/split-lines results))))))
    (testing "generate-post subcommand - sync"
      (let [results
            (cli-gen/generate-post!
             ["-i" "dev-resources/input/simple.json"
              "-E" (format "http://%s:%d" host port)
              "-B" "1"
              "-L" "1"
              "--no-async"])]
        ;; Errors would indicate 4xx response from Persephone server
        (is (nil? (:errors results)))))
    (testing "generate-post subcommand - async"
      (let [cont (tc/start! ds-tc/validate-server-container)
            host (:host cont)
            port (get (:mapped-ports cont) 8080)
            res  (cli-gen/generate-post!
                  ["-i" "dev-resources/input/simple.json"
                   "-E" (format "http://%s:%d" host port)
                   "-B" "1"
                   "-L" "1"
                   "--async"])]
        (is (nil? (:errors res)))))
    (tc/stop! cont)))

(deftest generate-test-2
  (testing "generate-post subcommand on match serever"
    (let [cont (tc/start! ds-tc/match-server-container)
          host (:host cont)
          port (get (:mapped-ports cont) 8080)
          res  (cli-gen/generate-post!
                ["-i" "dev-resources/input/simple.json"
                 "-E" (format "http://%s:%d" host port)
                 "-B" "25"
                 "-L" "25"
                 "--no-async"])]
      (is (nil? (:errors res)))
      (tc/stop! cont))))
