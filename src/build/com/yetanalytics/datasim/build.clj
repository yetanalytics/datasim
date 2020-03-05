(ns com.yetanalytics.datasim.build
  (:gen-class)
  (:require [clojure.tools.deps.alpha.reader :as deps-reader]
            [badigeon.clean :as clean]
            [badigeon.classpath :as classpath]
            [badigeon.bundle :as bundle]
            [badigeon.compile :as compile]))


(defn -main [group-id
             artifact-id
             version]
  (let [cli-ns 'com.yetanalytics.datasim.main
        server-ns 'com.yetanalytics.datasim.server
        package-name (symbol group-id artifact-id)
        out-path (bundle/make-out-path
                  package-name
                  version)
        compile-path "target/classes"]
    (println "AOT Compiling CLI...")
    (compile/compile cli-ns
                     {:compile-path compile-path
                      :compiler-options {:disable-locals-clearing false
                                         :elide-meta [:doc :file :line :added]
                                         :direct-linking true}
                      :classpath (classpath/make-classpath
                                  {:aliases [:cli]})})
    (println "AOT Compiling Server...")
    (compile/compile server-ns
                     {:compile-path compile-path
                      :compiler-options {:disable-locals-clearing false
                                         :elide-meta [:doc :file :line :added]
                                         :direct-linking true}
                      :classpath (classpath/make-classpath
                                  {:aliases [:server]})})
    (println "Creating bundle...")
    (bundle/bundle
     out-path
     {;; A map with the same format than deps.edn. :deps-map is used to resolve the project dependencies.
      :deps-map (let [deps-map (deps-reader/slurp-deps "deps.edn")]
                  (-> deps-map
                      (select-keys [:deps :paths :mvn/repos])
                      (update :paths conj compile-path)
                      (update :deps merge
                              (reduce merge
                                      (keep
                                       #(get-in deps-map
                                                [:aliases
                                                 %
                                                 :extra-deps])
                                       [:server :cli])))))
      ;; :excluded-libs #{'org.clojure/clojure}
      :allow-unstable-deps? true
      ;; The path of the folder where dependencies are copied, relative to the output folder.
      :libs-path "lib"})
    (println "Creating start script for CLI...")
    ;; Create a start script for the application
    (bundle/bin-script out-path cli-ns
                       {;; Specify which OS type the line breaks/separators/file extensions should be formatted for.
                        :os-type bundle/posix-like
                        ;; The path script is written to, relative to the out-path.
                        :script-path "bin/run.sh"
                        ;; A header prefixed to the script content.
                        :script-header "#!/bin/sh\n"
                        ;; The java binary path used to start the application. Default to \"java\" or \"runtime/bin/java\" when a custom JRE runtime is found under the run directory.
                                        ;:command "runtime/bin/java"
                        ;; The classpath option used by the java command.
                        ;; We'll run from the bundle dir
                        :classpath ".:./lib/*"
                        ;; JVM options given to the java command.
                                        ;:jvm-opts ["-Xmx1g"]
                        ;; Parameters given to the application main method.
                                        ;:args ["some-argument"]
                        :args ["$@"]
                        })
    (println "Creating start script for Server...")
    ;; Create a start script for the server
    (bundle/bin-script out-path server-ns
                       {;; Specify which OS type the line breaks/separators/file extensions should be formatted for.
                        :os-type bundle/posix-like
                        ;; The path script is written to, relative to the out-path.
                        :script-path "bin/server.sh"
                        ;; A header prefixed to the script content.
                        :script-header "#!/bin/sh\n"
                        ;; The java binary path used to start the application. Default to \"java\" or \"runtime/bin/java\" when a custom JRE runtime is found under the run directory.
                                        ;:command "runtime/bin/java"
                        ;; The classpath option used by the java command.
                        ;; We'll run from the bundle dir
                        :classpath ".:./lib/*"
                        ;; JVM options given to the java command.
                                        ;:jvm-opts ["-Xmx1g"]
                        ;; Parameters given to the application main method.
                                        ;:args ["some-argument"]
                        :args ["$@"]
                        })
    (println "Done!")
    (System/exit 0)))
