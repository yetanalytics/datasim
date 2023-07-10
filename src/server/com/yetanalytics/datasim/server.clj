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

(def batch-size 20)

;; TODO: Get rid of multipart form params and replace them with JSON params.

(defn- get-stream
  "Given an input map `input` and a key `k` into the input map, retrieve the
   content, which should be multipart form data, as an input stream. This
   input stream will then be read via `input/from-location`, which is weird
   but makes sense if you think of input streams as a location in memory."
  [input k]
  (let [raw-bytes (.getBytes (get input k))]
    (io/input-stream raw-bytes)))

(defn- sim-input
  [input]
  {:profiles       (input/from-location :profiles :json
                                        (get-stream input "profiles"))
   :personae-array (input/from-location :personae-array :json
                                        (get-stream input "personae-array"))
   :alignments     (input/from-location :alignments :json
                                        (get-stream input "alignments"))
   :parameters     (input/from-location :parameters :json
                                        (get-stream input "parameters"))})

(defn run-sim!
  "Returns a function that will accept an output stream to write to the client.
   Inside that function, a writer will open and the simulation will run.
   Each statement will be written to the stream."
  [input]
  ;; Uses multipart message for the request.
  ;; Read in each part of the input file, and convert into EDN
  (let [sim-input      (sim-input input)
        send-to-lrs    (if-some [send-to-lrs (get input "send-to-lrs")]
                         (read-string send-to-lrs)
                         false)
        endpoint       (get input "lrs-endpoint")
        api-key        (get input "api-key")
        api-secret-key (get input "api-secret-key")
        post-options   {:endpoint   endpoint
                        :batch-size batch-size
                        :username   api-key
                        :password   api-secret-key}]
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
              ;; We need to batch the generated statements before POSTing in
              ;; order to avoid keeping them all in memory.
              (doseq [statement-batch (partition-all batch-size statements)]
                (when send-to-lrs
                  (let [{:keys [fail]}
                        (client/post-statements post-options statement-batch)]
                    (when (not-empty fail)
                      (for [{:keys [status error]} fail]
                        (log/error :msg
                                   (client/post-error-message status error))))))
                (doseq [statement statement-batch]
                  (json/generate-stream statement w)
                  (.write w "\n"))))
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
;; that will be used in basic auth.
(defonce users
  (delay
    (let [credentials (env :credentials "")
          users'      (cstr/split credentials #",")]
      (if (not= [""] users')
        ;; Create a map of every allowed credential
        (reduce (fn [m cred]
                  (let [[user pass] (cstr/split cred #":")]
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

(def response-headers
  {"Content-Type" "application/json; charset=utf-8"})

(defn health-interceptor
  "Route implementation for the health endpoint.
   Returns a 200 OK if the server is up and running."
  [_]
  {:status 200
   :body   "OK"})

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
     (->> (middleware/authorization-error (:request ctx) ex backend)
          (assoc ctx :response))
     (catch Exception e
       (assoc ctx ::chain/error e)))
   :else
   (assoc ctx ::chain/error ex)))

(def common-interceptors
  [authentication-interceptor
   (authorization-interceptor backend)])

;; Generate response to stream simulation to the client.
;; Return a json file, and set it to be downloaded by the user.
(defn- generate-interceptor-response
  [{request-headers :headers
    input :multipart-params
    :as request}]
  (if (auth/authenticated? request)
    (try
      (let [response-body (run-sim! input)]
        {:status  (if (fn? response-body) 200 400)
         :headers response-headers
         :body    response-body})
      (catch Exception e
        {:status 500
         :body   (.getMessage e)}))
    (if (get request-headers "authorization")
      {:status 403
       :body   "Not Authenticated"}
      {:status 401
       :body   "No Authorization"})))

(def generate-interceptor
  {:name  :datasim.route/generate
   :enter (fn [{:keys [request] :as context}]
            (assoc context
                   :response
                   (generate-interceptor-response request)))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routes and server configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-api-host "0.0.0.0")

(def default-api-port 9090)

(def default-allowed-origins
  ["https://yetanalytics.github.io"
   "http://localhost:9091"])

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
     :host            (or (some-> env :api-host)
                          default-api-host)
     :port            (or (some-> env :api-port Long/parseLong)
                          default-api-port)
     :allowed-origins (or (some-> env :api-allowed-origins (cstr/split #","))
                          default-allowed-origins)}))

(defn- routes [root-path]
  (-> #{[(str root-path "/health")
         :get        health-interceptor
         :route-name :datasim.route/health]
        [(str root-path "/api/v1/generate")
         :post (into common-interceptors
                     [(ring-mid/multipart-params)
                      generate-interceptor])]}
      route/expand-routes))

(defn create-server
  []
  (let [{:keys [root-path
                host
                port
                allowed-origins]} (env-config env)]
    (log/info :msg "Starting DATASIM API..."
              :host host
              :port port
              :root-path root-path
              :allowed-origins allowed-origins)
    (http/create-server
     {::http/routes          (routes root-path)
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
