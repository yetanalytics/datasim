(ns com.yetanalytics.server-test
  "Integration tests for the DATASIM server."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as cstr]
            [io.pedestal.http :as http]
            [org.httpkit.client :as httpkit]
            [clj-test-containers.core :as tc]
            [com.yetanalytics.datasim.test-constants :as const]
            [com.yetanalytics.datasim.test-containers :as ds-tc]
            [com.yetanalytics.datasim.server :as server]))

(def server
  (server/create-server))

(def profile-string
  (format "[%s]" (slurp const/cmi5-profile-filepath)))

(def personaes-string
  (format "[%s]" (slurp const/simple-personae-filepath)))

(def models-string
  (slurp const/simple-models-filepath))

(def parameters-string
  (slurp const/simple-parameters-filepath))

(def post-header
  {"X-Experience-Api-Version" "1.0.3"
   "Content-Type" "multipart/form-data"})

(def json-post-header
  {"X-Experience-Api-Version" "1.0.3"
   "Content-Type" "application/json"})

(def multipart-content
  [{:name    "profiles"
    :content profile-string}
   {:name    "personae-array"
    :content personaes-string}
   {:name    "models"
    :content models-string}
   {:name    "parameters"
    :content parameters-string}])

(defn- multipart-post-content [lrs-endpoint api-key api-secret]
  (into multipart-content
        [{:name    "lrs-endpoint"
          :content lrs-endpoint}
         {:name    "api-key"
          :content api-key}
         {:name    "api-secret-key"
          :content api-secret}
         {:name    "send-to-lrs"
          :content "true"}]))

(deftest server-test
  (testing "server"
    (let [_       (http/start server)
          cont    (tc/start! ds-tc/validate-server-container)
          host    (:host cont)
          post    (get (:mapped-ports cont) 8080)
          lrs-url (format "http://%s:%d/statements" host post)]
      (testing "GET /health endpoint"
        (let [{:keys [status]}
              #_{:clj-kondo/ignore [:unresolved-var]}
              @(httpkit/get
                "http://0.0.0.0:9090/health")]
          (is (= 200 status))))
      (testing "POST /api/v1/generate endpoint"
        (let [{:keys [status body]}
              #_{:clj-kondo/ignore [:unresolved-var]}
              @(httpkit/post
                "http://0.0.0.0:9090/api/v1/generate"
                {:headers    post-header
                 :basic-auth ["username" "password"]
                 :multipart  multipart-content})]
          (is (= 200 status))
          (is (every?
               (fn [stmt-str]
                 (let [validate-res
                       #_{:clj-kondo/ignore [:unresolved-var]}
                       @(httpkit/post
                         lrs-url
                         {:headers json-post-header
                          :body    stmt-str
                          :as      :stream})]
                   (= 204 (:status validate-res))))
               (take 25 (rest (cstr/split-lines body)))))))
      (testing "POST /api/v1/generate endpoint w/ LRS"
        (let [endpoint  (format "http://%s:%d" host post)
              {:keys [status]}
              #_{:clj-kondo/ignore [:unresolved-var]}
              @(httpkit/post
                "http://0.0.0.0:9090/api/v1/generate"
                {:headers post-header
                 :basic-auth ["username" "password"]
                 :multipart (multipart-post-content endpoint "foo" "bar")})]
          (is (= 200 status))))
      (tc/stop! cont)
      (http/stop server))))
