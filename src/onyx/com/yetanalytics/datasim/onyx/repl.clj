(ns com.yetanalytics.datasim.onyx.repl
  "Cluster repl"
  (:require clojure.main
            rebel-readline.core
            rebel-readline.clojure.line-reader
            rebel-readline.clojure.service.local
            rebel-readline.clojure.main))

(defn repl!
  []
  (rebel-readline.core/with-line-reader
    (rebel-readline.clojure.line-reader/create
     (rebel-readline.clojure.service.local/create))
    (clojure.main/repl
     :prompt (fn []) ;; prompt is handled by line-reader
     :read (rebel-readline.clojure.main/create-repl-read))))
