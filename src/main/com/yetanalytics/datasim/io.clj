(ns com.yetanalytics.datasim.io
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]))

(defn read-location
  "Reads in a file from the given location and parses it."
  [loc & {:keys [fmt]
          :or {fmt :json}
          :as opts}]
  (try (with-open [r (io/reader loc)]
         (try
           (case fmt
             :json (json/read r :key-fn keyword))
           (catch Exception e
             (throw (ex-info "Parse Error"
                             {:type ::parse-error
                              :location loc
                              :opts opts}
                             e)))))
       (catch java.io.IOException e
         (throw (ex-info "I/O Error"
                         {:type ::io-error
                          :location loc
                          :opts opts}
                         e)))))
