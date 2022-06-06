(ns com.yetanalytics.datasim.util.errors
  (:require [clojure.spec.alpha :as s]))

(defn explain-to-map-coll
  "Convert spec error data for personae, alignments, and parameters
   into a map coll acceptable to the Datasim UI."
  [spec-kw spec-ed]
  (->> (::s/problems spec-ed)
       (map-indexed
        (fn [idx problem]
          (let [epath (into [spec-kw] (:in problem))
                estr  (-> problem
                          (assoc ::s/problems [problem])
                          s/explain-printer
                          with-out-str)
                eid   (str (name spec-kw) "-" idx)]
            {:path epath
             :text estr
             :id   eid})))
       vec))

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
                                             :text estr}))
                               acc
                               epath-estr-m))
                  [])
       (map-indexed (fn [idx emap]
                      (assoc emap :id (str "profile-" idx))))
       vec
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
                                                    :text estr})))
                                         acc*
                                         epath-estr-m))
                            acc
                            etype-epath-estr-m))
               [])
       (map-indexed (fn [idx emap]
                      (assoc emap :id (str "profiles-" idx))))
       vec
       not-empty))

(def bar
  (apply str (repeat 80 \=)))

(defn map-coll->strs
  "Form a list of CLI error strings from an error map coll."
  [map-coll]
  (for [{:keys [id path text]} map-coll]
    (format
     "%s\nINPUT ERROR: %s\n%s\npath: %s\n\n%s"
     bar
     id
     bar
     (pr-str path)
     text)))
