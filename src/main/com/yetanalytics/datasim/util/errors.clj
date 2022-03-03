(ns com.yetanalytics.datasim.util.errors
  (:require [clojure.spec.alpha :as s]))

(defn explain-to-map-coll
  "Convert spec error data for personae, alignments, and parameters
   into a map coll acceptable to the Datasim UI."
  [spec-kw spec-ed]
  (mapv
   (fn [problem]
     (let [epath (into [spec-kw] (:in problem))
           estr  (-> problem
                     (assoc ::s/problems [problem])
                     s/explain-printer
                     with-out-str)]
       {:path epath
        :text estr
        :id   epath}))
   (::s/problems spec-ed)))
