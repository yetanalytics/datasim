(ns user
  (:require [clojure.repl :refer [source doc apropos]]
            [clojure.pprint :refer [pprint]]))

(set! *warn-on-reflection* true)


(comment


  (require '[clojure.java.io :as io]
           '[clojure.data.json :as json]
           '[com.yetanalytics.pan.objects.templates.rules :as r]
           '[clojure.spec.alpha :as s]
           '[com.yetanalytics.datasim.input :as input]
           '[com.yetanalytics.datasim.json.path :as json-path])

  (json-path/parse)



  (def p (input/from-location :profile :json "dev-resources/profiles/cmi5/fixed.json"))


  (-> p
      :templates)

  (doseq [{:keys [rules]} (:templates p)
          {:keys [location] :as rule} rules]
    (clojure.pprint/pprint (json-path/parse location)))




















  )
