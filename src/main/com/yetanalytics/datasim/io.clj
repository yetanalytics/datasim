(ns com.yetanalytics.datasim.io
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.edn :as edn]))

(defn read-location
  "Reads in a file from the given location and parses it."
  [loc & {:keys [fmt
                 parser-opts]
          :or {fmt :json
               parser-opts []}
          :as opts}]
  (try (with-open [r (io/reader loc)]
         (try
           (case fmt
             :json (apply json/read r parser-opts))
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
