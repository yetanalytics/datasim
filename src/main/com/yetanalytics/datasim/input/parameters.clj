(ns com.yetanalytics.datasim.input.parameters
  "Parameter input specs and parsing."
  (:require [clojure.spec.alpha :as s]
            [java-time          :as t]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.pan.objects.profile :as prof]
            [com.yetanalytics.pan.objects.pattern :as pat]
            [com.yetanalytics.datasim.math.random :as random]
            [com.yetanalytics.datasim.util.errors :as errs])
  (:import [clojure.lang ExceptionInfo]
           [java.time.zone ZoneRulesException]
           [java.time Instant]
           [java.util Random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All options are optional, but everything except `end` will get defaults

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

(defn- timezone-string? [s]
  (try (t/zone-id s)
       (catch ExceptionInfo exi
         (if (= ZoneRulesException (type (ex-cause exi)))
           false
           (throw exi)))))

;; (optional) timezone, defaults to UTC
(s/def ::timezone
  (s/and string?
         not-empty
         timezone-string?))

;; Seed is required, but will be generated if not present
(s/def ::seed
  int?)

;; Max number of statements returned
(s/def ::max
  pos-int?)

;; Restrict Generation to these profile IDs
(s/def ::gen-profiles
  (s/every ::prof/id))

;; Restrict Generation to these pattern IDs
(s/def ::gen-patterns
  (s/every ::pat/id))

(defn- ordered-timestamps?
  "Are the `start`, `from`, and `end` timestamps ordered properly?"
  [{:keys [start from end]}]
  (let [start-t (t/instant start)
        ?from-t (some->> from t/instant)
        ?end-t  (some->> end t/instant)]
    (and (or (not ?end-t)
             (t/before? start-t ?end-t))
         (or (not ?end-t)
             (not ?from-t)
             (t/before? ?from-t ?end-t))
         (or (not ?from-t)
             (= ?from-t start-t)
             (t/before? start-t ?from-t)))))

(s/def ::parameters
  (s/and
   (s/keys :req-un [::start
                    ::timezone
                    ::seed]
           :opt-un [::end
                    ::from
                    ::max
                    ::gen-profiles
                    ::gen-patterns])
   ordered-timestamps?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn validate-parameters
  [parameters]
  (some->> (s/explain-data ::parameters parameters)
           (errs/explain-to-map-coll ::parameters)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn apply-defaults
  "Apply defaults to `params` with the current time and a random seed.
   If `params` is not provided simply return the default parameters."
  ([]
   (apply-defaults {}))
  ([{:keys [start from timezone seed] :as params}]
   (merge
    params
    (let [start (or start (.toString (Instant/now)))]
      {:start    start
       :from     (or from start)
       :timezone (or timezone "UTC")
       :seed     (or seed (random/rand-long (random/rng)))}))))
