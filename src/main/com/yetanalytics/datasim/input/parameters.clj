(ns com.yetanalytics.datasim.input.parameters
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

;; (optional) start of the returned statements (if after ::start).
;; This lets us page through sims to later times. Defaults to ::start
(s/def ::from
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
           :opt-un [::end
                    ::from])
   (fn [{:keys [start from end]}]
     (when end
       (assert (t/before? (t/instant start)
                          (t/instant end))
               "Sim must start before it ends.")
       (when from
         (assert (t/before? (t/instant from)
                            (t/instant end))
                 "From must be before end.")))
     (when from
       (assert (or (= from start)
                   (t/before? (t/instant start)
                              (t/instant from)))
               "Sim start must be before or equal to from."))
     true)))

(defn add-defaults
  "Generate defualts"
  [{:keys [start from timezone seed] :as params}]
  (merge
   params
   (let [s (or start (.toString (Instant/now)))]
     {:start s
      :from (or from s)
      :timezone (or timezone "UTC")
      :seed (or seed (.nextLong (Random.)))})))

(defrecord Parameters [start
                       end
                       timezone
                       seed]
  p/FromInput
  (validate [this]
    (s/explain-data ::parameters this))

  p/JSONRepresentable
  (read-key-fn [this k]
    (keyword nil (name k)))
  (read-body-fn [this json-result]
    (map->Parameters
     (add-defaults json-result)))
  (write-key-fn [this k]
    (name k))
  (write-body-fn [this]
    (into {} this)))
