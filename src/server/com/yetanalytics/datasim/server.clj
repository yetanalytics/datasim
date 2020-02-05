(ns com.yetanalytics.datasim.server
  (:require [clojure.java.io                        :refer [writer]]
            [io.pedestal.http                       :as http]
            [io.pedestal.http.route                 :as route]
            [io.pedestal.http.body-params           :as body-params]
            [io.pedestal.http.ring-middlewares      :refer [multipart-params]]
            [clojure.data.json                      :as json]
            [cheshire.core                          :as c]
            [clj-http.client                        :as client]
            [com.yetanalytics.datasim.sim           :as sim]
            [com.yetanalytics.datasim.util.sequence :as su])
  (:import [javax.servlet ServletOutputStream])
  (:gen-class))

(defn health
  "Route implementation for the health endpoint.
   Currently it just returns a 200 OK if the server is up and running."
  [request]
  {:status 200
   :body   "OK"})

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

(def generate
  {:name  :datasim.route/generate
   :enter (fn [context]
            ;; Generate response to stream simulation to the client.
            ;; Return a json file, and set it to be downloaded by the user.
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
                        :body   (.getMessage e)}))))})

(def routes
  (route/expand-routes
   #{["/health"
      :get        health
      :route-name :datasim.route/health]
     ["/api/v1/generate"
      :post [(multipart-params)
             generate]]}))

(defn create-server
  []
  (http/create-server
   {::http/routes routes
    ::http/type   :immutant
    ::http/port   9090
    ::http/join?  false}))

(defn start
  []
  (http/start (create-server)))

(defn -main
  [& args]
  (start))
