(ns com.yetanalytics.datasim.input
  "Comprehensive specification of input"
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.pan.objects.profile :as ps]
            [com.yetanalytics.pan.objects.pattern :as pat]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.input.profile :as profile]
            [com.yetanalytics.datasim.input.personae :as personae]
            [com.yetanalytics.datasim.input.alignments :as alignments]
            [com.yetanalytics.datasim.input.parameters :as params]
            [com.yetanalytics.datasim.io :as dio]
            [clojure.walk :as w]
            [com.yetanalytics.datasim.util :as u]))

(defn- profiles->pedges
  [profiles]
  (let [[templates patterns]
        (reduce (fn [[ts ps] {:keys [templates patterns]}]
                  [(concat ts templates) (concat ps patterns)])
                [[] []]
                profiles)]
    (pat/get-edges (pat/create-graph templates patterns))))

(s/def ::profiles
  (s/and
   (s/every ::ps/profile :min-count 1 :into [])
   ;; Validate that all edges with a Pattern src ends up at a Pattern or
   ;; Template dest that is also in the profile cosmos.
   (s/conformer profiles->pedges)
   ::pat/valid-edges))

(s/def ::personae
  ::personae/personae)

(s/def ::alignments
  ::alignments/alignments-input)

(s/def ::parameters
  ::params/parameters)

(s/def :com.yetanalytics.datasim/input
  ;; "Comprehensive input spec"
  (s/keys :req-un [::profiles
                   ::personae
                   ::alignments
                   ::parameters]))

(def subobject-constructors
  {:profile profile/map->Profile
   :profiles profile/map->Profile ;; hacky
   :personae personae/map->Personae
   :alignments alignments/map->Alignments
   :parameters params/map->Parameters})

(defn realize-subobjects
  "Make subobjects from JSON into records"
  [json-result]
  (reduce-kv
   (fn [m k v]
     (if-let [constructor (get subobject-constructors k)]
       (let [rec (constructor {})
             key-fn
             (partial
              p/read-key-fn
              rec)
             body-fn
             (cond->> (partial
                       p/read-body-fn
                       rec)
               ;; for profiles, it's a vector
               (= k :profiles)
               (partial mapv))]
         (assoc m
                k
                (body-fn
                 (w/postwalk
                  (fn [node]
                    ;; here we can be sure every prop is a key
                    ;; since we force it with read-key-fn on input
                    (if (keyword? node)
                      (let [nn (name node)]
                        (key-fn node))
                      node))
                  v))))
       (throw (ex-info (format "Unknown key %s" k)
                       {:type ::unknown-key
                        :key k
                        :json json-result}))))
   {}
   json-result))

(defn unrealize-subobjects
  "Make subobjects ready for writing to JSON"
  [input]
  (reduce-kv
   (fn [m k v]
     (if-let [constructor (get subobject-constructors k)]
       (let [rec (constructor {})
             key-fn
             (partial
              p/write-key-fn
              rec)
             body-fn
             (cond->> p/write-body-fn
               ;; for profiles, it's a vector
               (= k :profiles)
               (partial mapv))]
         (assoc m
                k
                (w/postwalk
                 (fn [node]
                   ;; Here we can't know it's a key unless we find it in a map
                   (if (map? node)
                     (reduce-kv
                      (fn [m' k' v']
                        (assoc m' (key-fn k') v'))
                      {}
                      node)
                     node))
                 (body-fn v))))
       (throw (ex-info (format "Unknown key %s" k)
                       {:type ::unknown-key
                        :key k
                        :input input}))))
   {}
   input))

(defrecord Input [profiles
                  personae
                  alignments
                  parameters]
  p/FromInput
  (validate [this]
    (s/explain-data :com.yetanalytics.datasim/input this))

  p/JSONRepresentable
  (read-key-fn [this k]
    (keyword nil k))
  (read-body-fn [this json-result]
    (map->Input
     (realize-subobjects json-result)))
  (write-key-fn [this k]
    (name k))
  (write-body-fn [this]
    (unrealize-subobjects this)))

(defn from-location
  [type-k fmt-k location]
  (if-let [constructor (if (= type-k :input)
                         map->Input
                         (get subobject-constructors type-k))]
    (case fmt-k
      ;; currently only JSON
      :json (if (= type-k :profiles)
              (dio/read-loc-array (constructor {}) location)
              (dio/read-loc-json (constructor {}) location)))
    (throw (ex-info (format "Unknown key %s" type-k)
                    {:type ::unknown-key
                     :key type-k}))))

(defn to-file
  [record fmt-k location]
  (case fmt-k
    ;; currently only JSON
    :json (dio/write-file-json record location)))

(defn to-out
  [record fmt-k]
  (case fmt-k
    ;; currently only JSON
    :json (dio/write-loc-json record *out*)))

(defn to-err
  [record fmt-k]
  (case fmt-k
    ;; currently only JSON
    :json (dio/write-loc-json record *err*)))

(defn validate
  "Validate input using the FromInput protocol. Does no handling on result"
  [input]
  (p/validate input))

(defn validate-throw
  "Validate input using the FromInput protocol. Throw an exception if the input
   isn't valid."
  [input]
  (if-let [spec-error (p/validate input)]
    (throw (ex-info (pr-str spec-error)
                    {:type ::invalid-input
                     :input input
                     :spec-error spec-error}))
    input))
