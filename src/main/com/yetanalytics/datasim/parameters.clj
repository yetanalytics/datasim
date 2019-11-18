(ns com.yetanalytics.datasim.parameters
  "Simulation global parameters"
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.protocols :as p]
            [xapi-schema.spec :as xs]
            [clojure.data.json :as json]
            [java-time :as t])
  (:import [java.time.zone ZoneRulesException]
           [java.time Instant]
           [java.util Random]))

;; all options are optional, but everything except `end` will get defaults

;; (optional) start of the simulation (inclusive), 8601 stamp
(s/def ::start
  ::xs/timestamp)

;; (optional) end of the simulation (exclusive), 8601 stamp
(s/def ::end
  (s/nilable ::xs/timestamp))

;; (optional) timezone, defaults to UTC
(s/def ::timezone
  (s/and string?
         not-empty
         (fn [s]
           (try (t/zone-id s)
                (catch clojure.lang.ExceptionInfo exi
                  (if (= (type (ex-cause exi))
                         ZoneRulesException)
                    false
                    (throw exi)))))))

;; Seed is required, but will be generated if not present
(s/def ::seed
  int?)

(s/def ::parameters
  (s/and
   (s/keys :req-un [::start
                    ::timezone
                    ::seed]
           :opt-un [::end])
   (fn [{:keys [start end]}]
     (if (and start end)
       (t/before? (t/instant start)
                  (t/instant end))
       true))))

(defn add-defaults
  "Generate defualts"
  [{:keys [start timezone seed] :as params}]
  (merge
   params
   {:start (or start (.toString (Instant/now)))
    :timezone (or timezone "UTC")
    :seed (or seed (.nextLong (Random.)))}))

(defrecord Parameters [start
                       end
                       timezone
                       seed]
  p/FromInput
  (validate [this]
    (s/explain-data ::parameters this))

  p/Serializable
  (deserialize [this r]
    (map->Parameters
     (add-defaults (json/read r :key-fn keyword))))
  (serialize [this w]))
