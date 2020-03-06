(ns com.yetanalytics.datasim.server
  (:require [clojure.java.io                        :refer [writer]]
            [io.pedestal.http                       :as http]
            [io.pedestal.http.route                 :as route]
            [io.pedestal.http.ring-middlewares      :refer [multipart-params]]
            [io.pedestal.interceptor                :as interceptor]
            [io.pedestal.interceptor.chain          :as chain]
            [io.pedestal.interceptor.error          :refer [error-dispatch]]
            [ring.util.codec                        :as codec]
            [buddy.auth                             :as auth]
            [buddy.auth.backends                    :as backends]
            [buddy.auth.middleware                  :as middleware]
            [clojure.data.json                      :as json]
            [cheshire.core                          :as c]
            [clj-http.client                        :as client]
            [environ.core                           :refer [env]]
            [com.yetanalytics.datasim.sim           :as sim]
            [com.yetanalytics.datasim.util.sequence :as su])
  (:import [javax.servlet ServletOutputStream])
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datasim fns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-data
  "Given a map and a key, retrieve the content of the key and read the JSON into EDN."
  [m k]
  (-> m
      (get k)
      (json/read-str :key-fn keyword)))

(defn run-sim!
  "Returns a function that will accept an output stream to write to the client.
   Inside that function, a writer will open and the simulation will run.
   Each statement will be written to the stream."
  [input]
  ;; Anon fn that accepts the output stream for the response body.
  (fn [^ServletOutputStream os]
    ;; Uses multipart message for the request.
    ;; Read in each part of the input file, and convert into EDN
    (let [data           {:profiles   (read-data input "profiles")
                          :personae   (read-data input "personae")
                          :alignments (read-data input "alignments")
                          :parameters (read-data input "parameters")}
          send-to-lrs    (or (get input "send-to-lrs") false)
          endpoint       (get input "lrs-endpoint")
          api-key        (get input "api-key")
          api-secret-key (get input "api-secret-key")]
      (with-open [w (writer os)]
        ;; Iterate over the entire input skeleton, generate statements.
        ;; Write them wrapped in a list
        (.write w "[\n")
        (doseq [s (-> (sim/build-skeleton data)
                      vals
                      (->> (su/seq-sort
                            (comp :timestamp-ms
                                  meta))))]
          (try
            (when send-to-lrs
              ;; Stream statement to an LRS
              (client/post endpoint
                           {:basic-auth [api-key api-secret-key]
                            :headers    {"X-Experience-API-Version" "1.0.3"
                                         "Content-Type"             "application/json;"}
                            :body       (c/generate-string s)}))
            ;; Write each statement to the stream, pad with newline at end.
            (json/write s w
                        :escape-slash   false
                        :escape-unicode false)
            (.write w "\n")
            (catch Exception e
              (println "---- Error ----"))))
        (.write w "]")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auth data and fns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reach into the environment and grab all of the user credentials from there
;;  that will be used in basic auth.
(defonce users
  (let [credentials (env :credentials)
        users       (clojure.string/split credentials #";")]
    ;; Create a map of every allowed credential
    (reduce (fn [m cred]
              (let [[user pass] (clojure.string/split cred #":")]
                (assoc m
                       user
                       {:username user
                        :password pass})))
            {}
            users)))

(defn auth-fn
  "This function will ensure that the creds from Basic Auth are authenticated."
  [request {:keys [username password]}]
  (when-let [user (get users username)]
    (when (= password (:password user))
      username)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interceptors/Middleware
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A wrapper for Buddy to know what the authentication fn is
(def backend 
  (backends/basic {:realm  "MyApi"
                   :authfn auth-fn}))

(def authentication-interceptor
  "Port of buddy-auth's wrap-authentication middleware."
  (interceptor/interceptor
   {:name ::authenticate
    :enter (fn [ctx]
             (update ctx :request middleware/authentication-request backend))}))

(defn authorization-interceptor
  "Port of buddy-auth's wrap-authorization middleware."
  [backend]
  (error-dispatch [ctx ex]
                  [{:exception-type :clojure.lang.ExceptionInfo :stage :enter}]
                  (try
                    (assoc ctx
                           :response
                           (middleware/authorization-error (:request ctx)
                                                           ex
                                                           backend))
                    (catch Exception e
                      (assoc ctx ::chain/error e)))
                  :else (assoc ctx ::chain/error ex)))

(def generate
  {:name  :datasim.route/generate
   :enter (fn [context]
            ;; Generate response to stream simulation to the client.
            ;; Return a json file, and set it to be downloaded by the user.
            (if (auth/authenticated? (:request context))
              (assoc context
                     :response
                     (try
                       {:status  200
                        :headers {"Content-Type"        "application/json; charset=utf-8"
                                  "Content-Disposition" (format "attachment; filename=\"simulation.json\"")}
                        :body    (run-sim! (-> context
                                               :request
                                               :multipart-params))}
                       (catch Exception e
                         {:status 400
                          :body   (.getMessage e)})))
              (if (get (-> context :request :headers) "authorization")
                (assoc context
                       :response
                       {:status 403
                        :body   "Not Authenticated"})
                (assoc context
                       :response
                       {:status 401
                        :body   "No Authorization"}))))})

(def download-url
  {:name  :datasim.route/download-url
   :enter (fn [context]
            (assoc context
                   :response
                   (try
                     {:status 200
                      :body   (-> context
                                  :request
                                  :query-params
                                  :url
                                  codec/url-decode
                                  client/get
                                  :body)}
                     (catch java.net.MalformedURLException e
                       {:status 400
                        :body   "malformed"})
                     (catch org.apache.http.client.ClientProtocolException e
                       {:status 406
                        :body   "client"})
                     (catch java.net.UnknownHostException e
                       {:status 404
                        :body   "unknown"})
                     (catch Exception e
                       {:status 501
                        :body   "other"}))))})

(def common-interceptors
  [authentication-interceptor
   (authorization-interceptor backend)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Route implementations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn health
  "Route implementation for the health endpoint.
   Currently it just returns a 200 OK if the server is up and running."
  [request]
  {:status 200
   :body   "OK"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routes and server configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def routes
  (route/expand-routes
   #{["/health"
      :get        health
      :route-name :datasim.route/health]
     ["/api/v1/generate"
      :post (into common-interceptors
                  [(multipart-params)
                   generate])]
     ["/api/v1/download-url"
      :get  (into common-interceptors
                  [download-url])]}))

(defn create-server
  []
  (http/create-server
   {::http/routes          routes
    ::http/type            :immutant
    ::http/allowed-origins ["https://yetanalytics.github.io"
                            "http://localhost:9091"]
    ::http/host "0.0.0.0"
    ::http/port            9090
    ::http/join?           false}))

(defn start
  []
  (http/start (create-server)))

(defn -main
  [& args]
  (start))
