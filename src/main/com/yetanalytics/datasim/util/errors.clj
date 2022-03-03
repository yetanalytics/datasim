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

(defn type-path-string-m->map-coll
  "Convert Pan's `type-path-string-map`, using `profile-id`, into a
   map coll acceptable to the Datasim UI. Returns `nil` if there are
   no errors."
  [profile-id type-path-string-map]
  (->> type-path-string-map
       (reduce-kv (fn [acc etype epath-estr-m]
                    (reduce-kv (fn [acc* epath estr]
                                 (conj acc* {:path (into [profile-id etype]
                                                         epath)
                                             :text estr
                                             :id   etype}))
                               acc
                               epath-estr-m))
                  [])
       not-empty))

(defn type-path-string-ms->map-coll
  "Convert Pan's `type-path-string-maps`, using `profile-ids`, into a
   map coll acceptable to the Datasim UI. Returns `nil` if there are
   not errors."
  [profile-ids type-path-string-maps]
  (->> type-path-string-maps
       (map (fn [profile-id etype-epath-estr-m]
              [profile-id etype-epath-estr-m])
            profile-ids)
       (reduce (fn [acc [prof-id etype-epath-estr-m]]
                 (reduce-kv (fn [acc* etype epath-estr-m]
                              (reduce-kv (fn [acc** epath estr]
                                           (let [epath* (into [prof-id etype]
                                                              epath)]
                                             (conj acc**
                                                   {:path epath*
                                                    :text estr
                                                    :id   epath*})))
                                         acc*
                                         epath-estr-m))
                            acc
                            etype-epath-estr-m))
               [])
       not-empty))
