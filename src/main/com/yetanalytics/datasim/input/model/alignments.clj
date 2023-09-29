(ns com.yetanalytics.datasim.input.model.alignments
  "Models for individual Concepts, Templates, Patterns, and Object
   Overrides. The term \"alignments\" is largely historical, as it was used
   to refer to models, before model personae were added."
  (:require [clojure.set            :as cset]
            [clojure.spec.alpha     :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.walk           :as w]
            [xapi-schema.spec       :as xs]
            [com.yetanalytics.datasim.math.random :as random]
            [com.yetanalytics.datasim.input.model.alignments.weight :as-alias weight]
            [com.yetanalytics.datasim.input.model.alignments.bounds :as-alias bounds]
            [com.yetanalytics.datasim.input.model.alignments.period :as-alias period]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ID
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Use pan.axioms/iri once that has its own generator

;; nilable so that optional patterns can specify weight for `null` option
(s/def ::id (s/nilable ::xs/iri))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Weight
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See also: ::object-overrides/weight

(s/def ::weight ::random/weight)

(s/def ::weights
  (s/every (s/keys :req-un [::id ::weight]) :kind vector?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time Bounds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- interval? [[start end]]
  (< start end))

(defmacro bound-spec [scalar-spec]
  `(s/every (s/or :scalar
                  ~scalar-spec
                  :interval
                  (s/and (s/tuple ~scalar-spec ~scalar-spec)
                         interval?)
                  :step-interval
                  (s/and (s/tuple ~scalar-spec ~scalar-spec pos-int?)
                         interval?))
            :kind vector?
            :min-count 1
            :gen-max 3))

(defmacro named-time-spec [index-spec name-index-map]
  `(s/and
    (s/or :integer ~index-spec
          :string  (s/and (set (keys ~name-index-map))
                          (s/conformer ~name-index-map
                                       (cset/map-invert ~name-index-map))
                          ~index-spec))
    ;; Remove s/or tags
    (s/conformer second)))

(def day-of-week-map
  {"Sunday"    0
   "Monday"    1
   "Tuesday"   2
   "Wednesday" 3
   "Thursday"  4
   "Friday"    5
   "Saturday"  6})

(def month-of-year-map
  {"January"   1
   "February"  2
   "March"     3
   "April"     4
   "May"       5
   "June"      6
   "July"      7
   "August"    8
   "September" 9
   "October"   10
   "November"  11
   "December"  12})

(s/def ::bounds/seconds
  (bound-spec (s/int-in 0 60)))

(s/def ::bounds/minutes
  (bound-spec (s/int-in 0 60)))

(s/def ::bounds/hours
  (bound-spec (s/int-in 0 24)))

(s/def ::bounds/daysOfWeek
  (bound-spec (named-time-spec (s/int-in 0 7) day-of-week-map)))

(s/def ::bounds/daysOfMonth
  (bound-spec (s/int-in 1 32)))

(s/def ::bounds/months
  (bound-spec (named-time-spec (s/int-in 1 13) month-of-year-map)))

(s/def ::bounds/years
  (bound-spec pos-int?))

(s/def ::bounds
  (s/every (s/keys :opt-un [::bounds/seconds
                            ::bounds/minutes
                            ::bounds/hours
                            ::bounds/daysOfWeek
                            ::bounds/daysOfMonth
                            ::bounds/months
                            ::bounds/years])
           :kind vector?
           :min-count 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time Bound Retries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::boundRestarts
  (s/every ::xs/iri :kind vector?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time Period
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- has-default-period?
  [periods]
  (some (fn [{:keys [bounds] :as period}] (when (not bounds) period))
        periods))

(s/def ::period/min
  (s/and number? pos?))

(s/def ::period/mean
  (s/and number? pos? (comp not zero?)))

(s/def ::period/fixed
  (s/and number? pos? (comp not zero?)))

(s/def ::period/unit
  #{"millis" "seconds" "minutes" "hours" "days" "weeks"})

(s/def ::period/bounds
  ::bounds)

(s/def ::periods
  (s/every (s/keys :opt-un [::period/min
                            ::period/mean
                            ::period/fixed
                            ::period/unit
                            ::period/bounds])
           :kind (every-pred vector? has-default-period?)
           :min-count 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Max Repeat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::repeatMax pos-int?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Due to limitations of keywords, we cannot have IRI keys, limiting extensions
(defn- no-iri-keys?
  "Returns false if there exists a key made from an IRI, e.g.
   (name :https://foo.org) => \"/foo.org\""
  [obj]
  (cond
    (map? obj)
    (if-not (->> obj vals (map no-iri-keys?) (some false?))
      (->> obj keys (some (partial re-matches #".*/.*")) not)
      false)
    (vector? obj)
    (every? no-iri-keys? obj)
    :else
    true))

(s/def ::object
  (s/with-gen (s/and (s/conformer w/stringify-keys w/keywordize-keys)
                     no-iri-keys?
                     :statement/object) ; from xapi-schema
    #(->> (s/gen :statement/object)
          (sgen/such-that no-iri-keys?)
          (sgen/fmap w/keywordize-keys))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alignment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def verb-spec
  (s/keys :req-un [::id
                   ::weight]))

(def activity-spec
  (s/keys :req-un [::id]
          :opt-un [::weight]))

(def activity-type-spec
  (s/keys :req-un [::id]
          :opt-un [::weight]))

(def pattern-spec
  (s/keys :req-un [::id]
          :opt-un [::weights   ; for alternate and optional patterns 
                   ::repeatMax ; for oneOrMore and zeroOrMore patterns
                   ::bounds
                   ::boundRestarts
                   ::periods]))

(def template-spec
  (s/keys :req-un [::id]
          :opt-un [::bounds
                   ::boundRestarts
                   ::periods]))

(def object-override-spec
  (s/keys :req-un [::object]
          :opt-un [::weight]))
