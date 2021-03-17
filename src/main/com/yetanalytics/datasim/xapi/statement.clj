(ns com.yetanalytics.datasim.xapi.statement
  "Generate Statements"
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as cset]
            [clojure.string :as string]
            [com.yetanalytics.datasim.xapi.profile :as profile]
            [com.yetanalytics.datasim.xapi.activity :as activity]
            [com.yetanalytics.datasim.input :as input]
            [com.yetanalytics.datasim.input.alignments :as alignments]
            [com.yetanalytics.datasim.input.personae :as personae]
            [com.yetanalytics.pan.objects.template :as template]
            [com.yetanalytics.datasim.random :as random]
            [xapi-schema.spec :as xs]
            [clojure.walk :as w]
            [com.yetanalytics.datasim.xapi.profile.template.rule :as rule]
            [com.yetanalytics.datasim.json.schema :as j-schema]
            [com.yetanalytics.datasim.xapi.extensions :as ext])
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
  ::alignments/alignment)

(s/def ::sim-t pos-int?)

#_(s/def ::seed int?)

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
             ::random/seed
             ;; A registration UUID string
             ::registration]
    :opt-un [::sub-registration]))
  :ret (s/and ::xs/statement
              (s/conformer meta)
              ::meta))

(defn- search-rules
  [template-rules target-location]
  ;; filter `template-rules` for a rule whose location is set to `target-location`
  (->> template-rules
       (filterv (fn [r] (= (:location r) target-location)))
       first
       not-empty))

(defn- handle-rule-vals [rng alignment the-rule]
  ;; handle `:any` and/or `:all` within `the-rule`
  (let [{any-coll :any
         all-coll :all} the-rule
        n-any           (count any-coll)
        n-all           (count all-coll)]
    ;; returns single item from `:any` or `:all` or returns nil
    (cond (and (> n-any 0) (> n-all 0))
          ;; both any and all for some reason
          ;; -> use all to filter any
          (let [remaining (into [] (cset/intersection (set all-coll) (set any-coll)))]
            (random/choose rng alignment remaining))
          (= n-all 1)
          ;; not handling (> n-all 1) as it doesn't make sense in isolation
          (first all-coll)
          (> n-any 0)
          ;; allow for users to shoot themselves in the foot
          (random/choose rng alignment any-coll))))

;; TODO: subregistration from :pattern-ancestors logic
;; -> "https://w3id.org/xapi/profiles/extensions/subregistration"
;;    -> subregistration extension key
;;    -> only necessary when a primary pattern contains another primary pattern

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
     ?activity-type :objectActivityType
     template-rules :rules
     template-in-scheme :inScheme
     :as template} :template
    pattern-ancestors :pattern-ancestors
    registration :registration
    ?sub-registration :sub-registration}]
  (let [rng          (random/seed-rng seed)
        obj-override (when (not-empty alignment)
                       (->> alignment
                            keys
                            (random/choose rng alignment)
                            (get alignment)
                            :object-override))
        ;; components of `base-stmt`
        stmt-id    (random/rand-uuid rng)
        stmt-actor (w/stringify-keys actor)
        stmt-verb  (or
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
        stmt-obj   (or
                    ;; object override is valid for the template
                    ;; and is present in the alignment
                    (when obj-override obj-override)
                    ;; quick scan rules for "$.object.id"
                    ;; -> when found, use `?activity-type` or look for "$.object.definition.type" rule
                    ;;    -> use `obj-at` to lookup `rule-obj-id` in `activities`, nil if not found
                    ;;    -> remove activity type level from `activities` and search for `rule-obj-id`, nil if not found
                    (when-let [rule-obj-id (handle-rule-vals rng alignment (search-rules template-rules "$.object.id"))]
                      ;; there's a rule specifying object.id, prep for lookup in `activities`
                      (if-let [obj-at (or ?activity-type
                                          (when-let [obj-type-rule (search-rules template-rules "$.object.definition.type")]
                                            (handle-rule-vals rng alignment obj-type-rule)))]
                        (get-in activities [obj-at rule-obj-id])
                        (get (reduce merge (vals activities)) rule-obj-id)))
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
        stmt-ctx   {"contextActivities" {"category" [{"id" template-in-scheme}]}
                    "registration"      registration}
        stmt-ts    (.toString (Instant/ofEpochMilli sim-t))
        ;; Start with this and reduce over the rules
        base-stmt  {"id"        stmt-id
                    "actor"     stmt-actor
                    "verb"      stmt-verb
                    "object"    stmt-obj
                    "context"   stmt-ctx
                    "timestamp" stmt-ts}
        ;; addition of `:spec` key to 0 or more `template-rules`
        template-rules! (ext/derive-generation-hint profiles rng template-rules)]
    (with-meta
      (let [stmt (rule/apply-rules-gen base-stmt
                                       template-rules!
                                       :seed (random/rand-long rng))]
        ;; Need to override object again, in case object is changed
        ;; by the Template rules.
        (if obj-override (assoc stmt "object" obj-override) stmt))
      ;; The duration in MS so we can continue the sim
      {;; The time (in millis since the epoch) after which the actor can
       ;; continue activity
       :end-ms (+ sim-t
                  (long
                   (random/rand-gauss
                    rng 600000.0 0.5)))
       ;; the time of the timestamp (in millis since the epoch)
       ;; note that just has to be greater that sim-t and less than or eq to end-ms,
       ;; it's up to you. For the stub we just make it sim-t
       :timestamp-ms sim-t
       ;; Return the template, useful for a bunch of things
       :template template})))
