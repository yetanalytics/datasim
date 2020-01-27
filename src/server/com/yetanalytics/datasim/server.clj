(ns com.yetanalytics.datasim.server
  (:require [clojure.java.io                        :refer [writer]]
            [io.pedestal.http                       :as http]
            [io.pedestal.http.route                 :as route]
            [io.pedestal.http.body-params           :as body-params]
            [clojure.data.json                      :as json]
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

(defn run-sim!
  "Returns a function that will accept an output stream to write to the client.
   Inside that function, a writer will open and the simulation will run.
   Each statement will be written to the stream."
  [input]
  ;; Anon fn that accepts the output stream for the response body.
  (fn [^ServletOutputStream os]
    (with-open [w (writer os)]
      ;; Iterate over the entire input skeleton, generate statements.
      (doseq [s (-> (sim/build-skeleton input)
                    vals
                    (->> (su/seq-sort
                          (comp :timestamp-ms
                                meta))))]
        ;; Write each statement to the stream, pad with newline at end.
        (json/write s w
                    :escape-slash   false
                    :escape-unicode false)
        (.write w "\n")))))

(def run-sim
  {:name  :datasim.route/run-sim
   :enter (fn [context]
            ;; Generate response to stream simulation to the client.
            ;; Return a json file, and set it to be downloaded by the user.
            (assoc context
                   :response
                   {:status  200
                    :headers {"Content-Type" "application/json"
                              "Content-Disposition" (format "attachment; filename=\"simulation.json\"")}
                    :body    (run-sim! (-> context
                                           :request
                                           :json-params))}))})

(def routes
  (route/expand-routes
   #{["/health"
      :get        health
      :route-name :datasim.route/health]
     ["/run-sim"
      :post [(body-params/body-params)
             run-sim]]}))

(defn create-server
  []
  (http/create-server
   {::http/routes routes
    ::http/type   :immutant
    ::http/port   9000
    ::http/join?  false}))

(defn start
  []
  (http/start (create-server)))

(defn -main
  [& args]
  (start))
