(ns com.yetanalytics.datasim.xapi.statement
  "Generate Statements"
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.set :as cset]
            [com.yetanalytics.datasim.xapi.profile :as profile]
            [com.yetanalytics.datasim.xapi.activity :as activity]
            [com.yetanalytics.datasim.input :as input]
            com.yetanalytics.datasim.input.alignments
            [com.yetanalytics.datasim.input.personae :as personae]
            [com.yetanalytics.pan.objects.template :as template]
            [com.yetanalytics.datasim.random :as random]
            [xapi-schema.spec :as xs]
            [clojure.walk :as w])
  (:import [java.time Instant]))

;; The duration, in milliseconds, of the returned statement
;; This is so we can resume processing AFTER the statement timestamp + duration
(s/def ::end-ms
  pos-int?)

(s/def ::timestamp-ms
  pos-int?)

(s/def ::meta
  (s/keys :req-un [::timestamp-ms
                   ::end-ms]))

(s/def ::alignment
  :alignment-map/actor-alignment)

(s/def ::sim-t pos-int?)

(s/def ::seed int?)

(s/def ::registration
  ::xs/uuid)

(s/def ::sub-registration
  ::xs/uuid)

;; TODO: this is a stub for the real ones
(s/def ::pattern-ancestors
  (s/every map?))

;; a stub for a map of activities by IRI
(s/def ::activities
  (s/map-of ::xs/iri ;; activity type
            (s/map-of ::xs/iri
                      ::xs/activity)))

(s/fdef generate-statement
  :args
  (s/cat
   :args-map
   (s/keys
    :req-un [;; input for the whole simulation
             :com.yetanalytics.datasim/input
             ;; flat map of profile iris to objects
             ::profile/iri-map
             ;; all the activities we can use, by activity type
             ::activities
             ;; the actor for the statement
             ;; (may have to stringify keys?)
             ::xs/actor
             ;; their alignment, a map of IRI to -1.0->1.0
             ::alignment
             ;; a statement template to generate from
             ::template/template
             ;; antecedent patterns to the current template, and whether or not
             ;; they are primary
             ::pattern-ancestors
             ;; Simulation time, in ms since epoch
             ::sim-t
             ;; A seed to generate with. Note that if you're calling more seeded
             ;; generators, you'll need to make a seed from this one for each.
             ::seed
             ;; A registration UUID string
             ::registration]
    :opt-un [::sub-registration]))
  :ret (s/and ::xs/statement
              (s/conformer meta)
              ::meta))

(defn generate-statement
  [{{:keys [profiles]} :input
    iri-map :iri-map
    activities :activities
    actor :actor
    alignment :alignment
    sim-t :sim-t
    seed :seed
    {template-iri :id
     ?verb-id :verb
     ?activity-type :objectActivityType} :template
    pattern-ancestors :pattern-ancestors
    registration :registration
    ?sub-registration :sub-registration}]

  (let [rng (random/seed-rng seed)]
    (with-meta
      ;; The generated statement (or the easy stuff anyhow)
      ;; doesn't use any rules or anything yet.
      ;; Maybe start with this and reduce over the rules?
      {"id" (random/rand-uuid rng)
       "actor" (w/stringify-keys actor)

       ;; TODO: this verb thing is a stub, make it better
       ;; and pay attention to the rules
       "verb" (or
               ;; explicit verb
               (and ?verb-id
                    (merge {"id" ?verb-id}
                           (when-let [lmap (get-in iri-map [?verb-id :prefLabel])]
                             {"display" (w/stringify-keys lmap)})))
               ;; choose a verb from the prof
               (when-let [verbs (not-empty (into {}
                                                 (filter (comp (partial = "Verb")
                                                               :type second))
                                                 iri-map))]
                 (let [some-v-id (random/choose rng alignment (keys verbs))
                       v-concept (get verbs some-v-id)]
                   (merge {"id" some-v-id}
                          (when-let [lmap (get v-concept :prefLabel)]
                            {"display" (w/stringify-keys lmap)}))))
               {"id" "http://adlnet.gov/expapi/verbs/experienced"
                "display" {"en" "experienced"}})

       "object" (or
                 ;; there's an activity type we choose one of those
                 (and ?activity-type
                      (let [some-activity-id
                            (random/choose rng
                                           alignment
                                           (keys (get activities ?activity-type)))]
                        (get-in activities [?activity-type some-activity-id])))
                 ;; no type, choose any activity
                 (let [flat-activities
                       (reduce merge
                               (vals activities))

                       some-activity-id
                       (random/choose rng
                                      alignment
                                      (keys flat-activities))]
                   (get flat-activities some-activity-id)))

       ;; Timestamp is up to you, here I just made it sim-t
       "timestamp" (.toString (Instant/ofEpochMilli sim-t))}
      ;; The duration in MS so we can continue the sim
      {
       ;; The time (in millis since the epoch) after which the actor can
       ;; continue activity
       :end-ms (+ sim-t
                  (long
                   (random/rand-gauss
                    rng 600000.0 0.5)))
       ;; the time of the timestamp (in millis since the epoch)
       ;; note that just has to be greater that sim-t and less than or eq to end-ms,
       ;; it's up to you. For the stub we just make it sim-t
       :timestamp-ms sim-t})))


(comment
  ;; for REPL hacking
  (let [top-seed 42
        top-rng (random/seed-rng top-seed)
        input (input/from-location :input :json "dev-resources/input/simple.json")
        iri-map (apply profile/profiles->map (:profiles input))
        activities (activity/derive-cosmos input (random/rand-long top-rng))]
    (generate-statement
     {:input input
      :iri-map iri-map
      :activities activities
      :actor (-> input :personae :member first)
      :alignment (get-in input [:alignments :alignment-map "mbox::mailto:bob@example.org"])
      :sim-t 0
      :seed (random/rand-long top-rng)
      :template (get iri-map "https://w3id.org/xapi/cmi5#satisfied")
      :pattern-ancestors
      [{:id "https://w3id.org/xapi/cmi5#toplevel", :primary true}
       {:id "https://w3id.org/xapi/cmi5#satisfieds", :primary false}]
      :registration (random/rand-uuid top-rng)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generation from a Rule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: given Kelvins work, `handle-json-path-str` is not exhaustive
;; - not going to include any of the edge cases within the demo profile

;; FIXME: handle @context when any/all/none contain objects (maps)
;; - JSONLD/Linked Data specific, not priority

;; FIXME: support for selectors

;; FIXME: usage of scopeNote?

;; TODO: handle generation inference/boundaries given stmt-path terminal value
;; - enumeration of possible/sensible `location` strings

;; TODO: handling of thing returned by `in-path-fn` (in `handle-matchables`) based on `location`

;; TODO: parsing of profile item returned by `iri-lookup-attempt` based on
;; `:type` and/or `location`

;; TODO: impl of value interp (profile item vs not profile item) within `matchable-values`
