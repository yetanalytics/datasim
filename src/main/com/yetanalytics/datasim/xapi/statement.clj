(ns com.yetanalytics.datasim.xapi.statement
  "Generate Statements"
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.xapi.profile :as profile]
            [com.yetanalytics.datasim.input :as input]
            com.yetanalytics.datasim.input.alignments
            [com.yetanalytics.datasim.input.personae :as personae]
            [com.yetanalytics.pan.objects.template :as template]
            [xapi-schema.spec :as xs]))

;; The duration, in milliseconds, of the returned statement
;; This is so we can resume processing AFTER the statement timestamp + duration
(s/def ::end-ms
  pos-int?)

(s/def ::meta
  (s/keys :req-un [::end-ms]))

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
  [{{} :input
    iri-map :iri-map
    actor :actor
    alignment :alignment
    sim-t :sim-t
    seed :seed
    {template-iri :id} :template
    registration :registration
    ?sub-registration :sub-registration}]

  (with-meta
    ;; The generated statement
    {}
    ;; The duration in MS so we can continue the sim
    {:end-ms (+ sim-t 1000)}))


(comment
  ;; for REPL hacking
  (let [input (input/from-location :input :json "dev-resources/input/simple.json")
        iri-map (apply profile/profiles->map (:profiles input))]
    (generate-statement
     {:input input
      :iri-map iri-map
      :actor (-> input :personae :member first)
      :alignment (get-in input [:alignments :alignment-map "mbox::mailto:bob@example.org"])
      :sim-t 0
      :seed 42
      :template (get iri-map "https://w3id.org/xapi/cmi5#satisfied")
      :registration (.toString (java.util.UUID/randomUUID))}))











  )
