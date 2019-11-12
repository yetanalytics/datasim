(ns com.yetanalytics.datasim.io
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [com.yetanalytics.datasim.protocols :as p]))

(defn read-loc
  "Reads in a file from the given location and parses it."
  [record loc]
  (try (with-open [r (io/reader loc)]
         (try
           (p/deserialize record r)
           (catch Exception e
             (throw (ex-info "Parse Error"
                             {:type ::parse-error
                              :location loc}
                             e)))))
       (catch java.io.IOException e
         (throw (ex-info "I/O Error"
                         {:type ::io-error
                          :location loc}
                         e)))))
