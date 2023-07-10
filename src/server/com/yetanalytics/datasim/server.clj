(ns com.yetanalytics.datasim.server
  (:require [environ.core                      :refer [env]]
            [clojure.string                    :as cstr]
            [clojure.java.io                   :as io]
            [cheshire.core                     :as json]
            [buddy.auth                        :as auth]
            [buddy.auth.backends               :as backends]
            [buddy.auth.middleware             :as middleware]
            [io.pedestal.log                   :as log]
            [io.pedestal.http                  :as http]
            [io.pedestal.http.route            :as route]
            [io.pedestal.http.ring-middlewares :as ring-mid]
            [io.pedestal.interceptor           :as interceptor]
            [io.pedestal.interceptor.chain     :as chain]
            [io.pedestal.interceptor.error     :as error]
            [com.yetanalytics.datasim          :as ds]
            [com.yetanalytics.datasim.input    :as input]
            [com.yetanalytics.datasim.client   :as client])
  (:import [javax.servlet ServletOutputStream])
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datasim fns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-stream
  "Given a map and a key, retrieve the content of the key as an input stream."
  [m k]
  (let [raw-bytes (.getBytes (get m k))]
    (io/input-stream raw-bytes)))

(defn run-sim!
  "Returns a function that will accept an output stream to write to the client.
   Inside that function, a writer will open and the simulation will run.
   Each statement will be written to the stream."
  [input]
  ;; Uses multipart message for the request.
  ;; Read in each part of the input file, and convert into EDN
  (let [sim-input      {:profiles       (input/from-location :profiles :json
                                                             (get-stream input "profiles"))
                        :personae-array (input/from-location :personae-array :json
                                                             (get-stream input "personae-array"))
                        :alignments     (input/from-location :alignments :json
                                                             (get-stream input "alignments"))
                        :parameters     (input/from-location :parameters :json
                                                             (get-stream input "parameters"))}
        send-to-lrs    (if-let [send-to-lrs (get input "send-to-lrs")]
                         (read-string send-to-lrs)
                         false)
        endpoint       (get input "lrs-endpoint")
        api-key        (get input "api-key")
        api-secret-key (get input "api-secret-key")]
    (if-some [spec-errors (input/validate :input sim-input)]
      ;; Return a coll of maps that are acceptable to the Datasim UI
      (mapv #(assoc % :visible true) spec-errors)
      ;; Anon fn that accepts the output stream for the response body.
      (fn [^ServletOutputStream os]
        (with-open [w (io/writer os)]
          ;; Iterate over the entire input skeleton, generate statements.
          ;; Write them wrapped in a list
          (.write w "[\n")
          (try
            (let [statements (ds/generate-seq sim-input)]

              (when send-to-lrs
                (let [post-options (cond-> {:endpoint endpoint
                                            :batch-size 20}
                                     (and api-key api-secret-key)
                                     (assoc-in [:http-options :basic-auth] [api-key api-secret-key]))
                      {:keys [fail]}
                      (client/post-statements
                       post-options
                       statements
                       :emit-ids-fn
                       (fn [ids]
                         (doseq [^java.util.UUID id ids]
                           (printf "%s\n" (.toString id))
                           (flush))))]
                  (when (not-empty fail)
                    (for [{:keys [status error]} fail]
                      (log/error :msg
                                 (format "LRS Request FAILED with STATUS: %d, MESSAGE:%s"
                                         status (or (some-> error ex-message) "<none>")))))))
              (doseq [s (ds/generate-seq sim-input)]
                (json/generate-stream s w)
                (.write w "\n")))
            (catch Exception e
              (log/error :msg "Error Building Simulation Skeleton"
                         :e   (.getMessage e)))
            (finally
              (log/info :msg "Finish Simulation")
              (.write w "]"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auth data and fns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reach into the environment and grab all of the user credentials from there
;;  that will be used in basic auth.
(defonce users
  (delay
    (let [credentials (env :credentials "")
          users'       (clojure.string/split credentials #",")]
      (if (not= [""] users')
        ;; Create a map of every allowed credential
        (reduce (fn [m cred]
                  (let [[user pass] (clojure.string/split cred #":")]
                    (assoc m
                           user
                           {:username user
                            :password pass})))
                {}
                users')
        (do
          (log/info :msg "No Basic-Auth Credentials were set.")
          {})))))

;; `request` is unused but is needed for `backends/basic`
(defn auth-fn
  "This function will ensure that the creds from Basic Auth are authenticated."
  [_request {:keys [username password]}]
  (when-let [user (get @users username)]
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
  #_{:clj-kondo/ignore [:unresolved-symbol]}
  (error/error-dispatch
   [ctx ex]
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
                       (let [response-body
                             (run-sim! (-> context
                                           :request
                                           :multipart-params))]
                         {:status  (if (fn? response-body)
                                     200
                                     400)
                          :headers {"Content-Type"        "application/json; charset=utf-8"}
                          :body    response-body})
                       (catch Exception e
                         {:status 500
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



(def common-interceptors
  [authentication-interceptor
   (authorization-interceptor backend)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Route implementations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn health
  "Route implementation for the health endpoint.
   Currently it just returns a 200 OK if the server is up and running."
  [_]
  {:status 200
   :body   "OK"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routes and server configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- assert-valid-root-path
  [root-path]
  (when (not-empty root-path)
    (when-not (cstr/starts-with? root-path "/")
      (throw (ex-info "API_ROOT_PATH must start with /"
                      {:type      ::invalid-root-path
                       :root-path root-path})))
    (when (cstr/ends-with? root-path "/")
      (throw (ex-info "API_ROOT_PATH must not end with /"
                      {:type      ::invalid-root-path
                       :root-path root-path})))))

(defn- env-config
  [env]
  (let [root-path (get env :api-root-path "")]
    (assert-valid-root-path root-path)
    {:root-path       root-path
     :host            (get env :api-host
                           "0.0.0.0")
     :port            (or (some-> env :api-port Long/parseLong)
                          9090)
     :allowed-origins (if-let [allowed-str (:api-allowed-origins env)]
                        (cstr/split allowed-str #",")
                        ["https://yetanalytics.github.io"
                         "http://localhost:9091"])}))

(defn create-server
  []
  (let [{:keys [root-path
                host
                port
                allowed-origins]} (env-config env)]
    (log/info :msg "Starting DATASIM API..."
              :root-path root-path
              :host host
              :port port
              :allowed-origins allowed-origins)
    (http/create-server
     {::http/routes          (route/expand-routes
                              #{[(str root-path "/health")
                                 :get        health
                                 :route-name :datasim.route/health]
                                [(str root-path "/api/v1/generate")
                                 :post (into common-interceptors
                                             [(ring-mid/multipart-params)
                                              generate])]})
      ::http/type            :jetty
      ::http/allowed-origins allowed-origins
      ::http/host            host
      ::http/port            port
      ::http/join?           false})))

(defn start
  []
  (http/start (create-server)))

(defn -main
  [& _args]
  (start))
