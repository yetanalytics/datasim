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
      :registration (.toString (java.util.UUID/randomUUID))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generation from a Rule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: given Kelvins work, `handle-json-path-str` is not exhaustive
;; - not going to include any of the edge cases within the demo profile

;; FIXME: handle @context when any/all/none contain objects (maps)
;; - JSONLD/Linked Data specific, not priority

;; FIXME: support for selectors

;; FIXME: usage of scopeNote?

;; FIXME: correct handling of seed, just a stub right now

;; TODO: handle generation inference/boundaries given stmt-path terminal value
;; - enumeration of possible/sensible `location` strings

;; TODO: handling of thing returned by `in-path-fn` (in `handle-matchables`) based on `location`

;; TODO: parsing of profile item returned by `iri-lookup-attempt` based on
;; `:type` and/or `location`

;; TODO: impl of value interp (profile item vs not profile item) within `matchable-values`

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; normalization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn normalization-helper
  "convert non-nil `scalar-or-coll` to vector or return empty vector"
  [case-k scalar-or-coll]
  (if-some [_ (seq scalar-or-coll)]
    (case case-k
      :literal [scalar-or-coll]
      :fn      (vec scalar-or-coll)
      [])
    []))

(defn normalize-to-vec
  "conversion from collection -> vector if not already a vector.
   - scalar values + maps are returned wrapped in vec, ie. this -> [this]"
  [this]
  (if-not (vector? this)
    ;; check for non-nil
    (if-some [scalar-or-coll this]
      ;; maps + strings are special case where we want to check for emptyness
      ;; - don't want to return [{}] or [""]
      ;; - dont want to return ["/f" "/o" "/o"] or [["foo" "baz"] ["another key" "another val"]]
      (cond (or (map? scalar-or-coll) (string? scalar-or-coll))
            (normalization-helper :literal scalar-or-coll)
            ;; ^ returns ["foo"] or [{"foo" "baz" "another key" "another val"}]
            (coll? scalar-or-coll)
            ;; trust in rich
            (normalization-helper :fn scalar-or-coll)
            ;; avoid call to seq which will throw, return wrapped in literal
            :else [scalar-or-coll])
      ;; nil -> []
      [])
    ;; already a vector
    this))

(comment
  (= [{:foo "bar"}] (normalize-to-vec {:foo "bar"}))
  (= [true] (normalize-to-vec true))
  (= [false] (normalize-to-vec false))
  (= []
     (normalize-to-vec nil)
     (normalize-to-vec [])
     (normalize-to-vec (list))
     (normalize-to-vec {})
     (normalize-to-vec #{})
     (normalize-to-vec ""))
  (= [1 2 3]
     (normalize-to-vec (list 1 2 3))
     (normalize-to-vec [1 2 3])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iteration through stmt-path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn butfirst
  "returns subvec of everything except for the first item in `coll`
   - `coll` will be normalized to a vector if not already one.
   -- see `normalize-to-vec` for logic"
  [coll]
  (let [normalized (normalize-to-vec coll)]
    (if (seq normalized) (subvec normalized 1) normalized)))

(comment
  (= []
     (butfirst [])
     (butfirst ["a"])
     (butfirst {:foo "baz"})
     (butfirst {:foo "baz" :buz "bar"})
     (butfirst (list "a"))
     (butfirst "a")
     (butfirst true)
     (butfirst 1.0))
  (= ["b" "c"]
     (butfirst ["a" "b" "c"])
     (butfirst (list "a" "b" "c"))
     (butfirst (set ["a" "b" "c"]))
     (butfirst (for [each ["a" "b" "c"]] each)))
  (= [{:foo "baz"}]
     (butfirst [{"I'm" "removed"} {:foo "baz"}])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Presence logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn continue-given-presence?
  "should processing of the current rule continue or not."
  [s]
  (case s
    "excluded" false
    true))

(comment
  (= false (continue-given-presence? "excluded"))
  (= true
     (continue-given-presence? "included")
     (continue-given-presence? "recommended")
     (continue-given-presence? nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON Path parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-json-path-str
  "extract items found within json-path string array, ie. ['some-iri']
   - `path` everything up to the array
   - `nested` everything inbetween [' and ']"
  [path-str]
  (if (string/includes? path-str "['")
    (let [[path v*] (string/split path-str #"\['")
          [v _] (string/split v* #"\']")]
      {:path path :nested v})
    {:path path-str}))

(comment
  (= (handle-json-path-str "$.context.contextActivities.category['https://w3id.org/xapi/catch/v1']")
     {:path "$.context.contextActivities.category"
      :nested "https://w3id.org/xapi/catch/v1"})
  (= (handle-json-path-str "$.context.contextActivities.category['*']")
     {:path "$.context.contextActivities.category"
      :nested "*"})
  (= (handle-json-path-str "$.result.score.raw") {:path "$.result.score.raw"}))

(defn deconstruct-json-path
  "ensure root was $ before returning the path into stmt"
  [path]
  (let [[root & components :as deconstructed] (string/split path #"\.")]
    (assert (= root "$") "JSONPath string did not start with root!")
    (butfirst deconstructed)))

(comment
  (= ["result" "score" "raw"]
     (deconstruct-json-path "$.result.score.raw"))
  (= ["context" "contextActivities" "category"]
     (deconstruct-json-path "$.context.contextActivities.category"))
  (= "bad root detected"
     (try (deconstruct-json-path "$$.result.score.raw")
          (catch AssertionError e "bad root detected"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; any/all/none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn any-all-helper
  "normalize coll then return if non-empty."
  [coll]
  (let [normalized (normalize-to-vec coll)]
    (when-some [_ (try (seq normalized)
                       (catch Exception e
                         (throw (ex-info "normalization is broken!"
                                         {:coll coll :normalized normalized}
                                         e))))]
      normalized)))

(defn handle-any
  "selection of a value from the `coll` of possible values
   - will return nil if there are no values to select from!"
  [rng coll]
  (when-some [data (any-all-helper coll)]
    (random/rand-nth* rng data)))

(defn handle-all
  "return the first item in `coll` if its the only item, otherwise return all of `coll`
   - will return nil if `coll` is empty"
  [coll]
  (when-some [data (any-all-helper coll)]
    (if (= 1 (count data))
      ;; only care about the single value inside the vector
      (first data)
      ;; return normalized `coll` containing 2 or more items
      data)))

(defn handle-none
  "remove items from `possibilities` that are found in
   `none-coll` and returns the set of remaining items."
  [possibilities none-coll]
  (let [p-set (set possibilities)
        n-set (set none-coll)]
    (if (seq p-set)
      (cset/difference p-set n-set)
      (throw (ex-info "no possibilities were provided!"
                      {:possibilities possibilities
                       :none          none-coll})))))
(comment
  (= #{7 4 5}
     (handle-none [3 4 5 6 7] [1 2 3 6]))
  (= #{4 3 5}
     (handle-none [3 4 5] [])
     (handle-none [3 4 5] nil))
  (= "no possibilities were provided!"
     (try (handle-none [] [1 2 3])
          (catch Exception e (ex-message e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; any + all + none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compound-logic
  "returns possible matchable value(s) based on `rule`"
  [rule rng]
  (let [{any-coll  :any
         all-coll  :all
         none-coll :none} rule]
    (if-some [remaining-set (try (handle-none any-coll none-coll)
                                 (catch Exception e nil))]
      (if (seq all-coll)
        ;; find the overlap between (`any` - `none`) and `all`
        (handle-all (cset/intersection remaining-set (set all-coll)))
        ;; pick one using `rng`
        (handle-any rng remaining-set))
      (if (seq all-coll)
        ;; ignore `none` and handle `all`
        (handle-all all-coll)
        ;; return a fn which expects a collection of possible values
        ;; - filtered down to valid values based on `none` and return one of them using `rng`
        (fn [possibilities]
          (->> none-coll
               (handle-none possibilities)
               (handle-any rng)))))))

(comment
  (= 2
     (compound-logic {:any [1 2 3]
                      :all [2]
                      :none [1 3]}
                     (random/seed-rng 123))
     (compound-logic {:any [1 2 3]
                      :all []
                      :none [1]}
                     (random/seed-rng 123))
     (compound-logic {:any [1 2 3]
                      :none [1]}
                     (random/seed-rng 123))
     (compound-logic {:any [2 3]}
                     (random/seed-rng 123))
     (compound-logic {:any [1 2 3 4]
                      :none [1 4]}
                     (random/seed-rng 123)))

  (= [1 2 3 4]
     (compound-logic {:all [1 2 3 4]} (random/seed-rng 123))
     ;; `none` ignored if no `any` but some `all`
     (compound-logic {:all [1 2 3 4] :none [1 2 3 4]} (random/seed-rng 123)))

  ;; filter coll of generated possibilities based on `none` then select using `rng`
  (= 3 ((compound-logic
         {:none [5 6 7 8]}
         (random/seed-rng 123)) [1 2 3 4])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Priority determination
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn iri-lookup-attempt
  "query `iri-map` for more data about `maybe-iri`, defaults to `maybe-iri` + `fallback` if not found."
  [{:keys [maybe-iri iri-map fallback]}]
  (get iri-map maybe-iri
       {:non-iri maybe-iri
        :fallback fallback}))

(comment
  (= "bar"
     (iri-lookup-attempt
      {:maybe-iri "foo"
       :iri-map   {"foo" "bar"}
       :fallback "not needed"}))
  (= {:non-iri "foo" :fallback "needed"}
     (iri-lookup-attempt
      {:maybe-iri "foo"
       :iri-map   {:foo "bar"}
       :fallback "needed"})))

(defn handle-matchables
  "helper fn which returns the stmt value to use for the current rule
   - `matchable` = any/all/none
   - `generated` = based on `location`
   - `within-path` = item within `location` array, ie. ...['within-path']
   - `iri-lookup` = iri-map from input"
  [{:keys [matchable generated within-path iri-lookup]}]
  (let [in-path-fn (fn [fallback]
                     (if-some [_ within-path]
                       ;; non-nil
                       (if (string? within-path)
                         ;; string
                         (case within-path
                           "*" fallback
                           ""  fallback
                           ;; assume IRI but return `within-path` + `fallback` if assumption is wrong
                           (iri-lookup-attempt
                            {:maybe-iri within-path
                             :iri-map   iri-lookup
                             :fallback  fallback}))
                         ;; some non-string, unexpected, return fallback
                         fallback)
                       ;; nil, return fallback
                       fallback))]
    ;; check `matchable` first to see if profile does the work for us
    (if-some [any-all-none (try (not-empty matchable)
                                (catch Exception e matchable))]
      ;; can be a fn or data
      (let [matches (if (fn? any-all-none)
                      ;; (->> none-coll (handle-none generated) (handle-any rng))
                      ;; - see `compound-logic`
                      (any-all-none generated)
                      any-all-none)]
        ;; attempt lookup, fallback to `matches`
        (in-path-fn matches))
      ;; attempt lookup, fallback to `generated`
      (in-path-fn generated))))

(comment
  ;; `within-path` takes highest priority bc it CAN be a direct reference to something in profile
  (= "test value"
     (handle-matchables
      {:matchable "matchable"
       :generated "generated"
       :within-path "mock-iri"
       :iri-lookup {"mock-iri" "test value"}}))
  ;; but if not, return `maybe-iri` + `fallback` with `matchable` taking higher priority then `generated`
  (= {:non-iri "lookup miss"
      :fallback "matchable"}
     (handle-matchables
      {:matchable "matchable"
       :generated "generated"
       :within-path "lookup miss"
       :iri-lookup {"mock-iri" "test value"}}))
  ;; but when `matchable` is not usable, fallback to `generated`
  (= {:non-iri "lookup miss"
      :fallback "generated"}
     (handle-matchables
      {:matchable nil
       :generated "generated"
       :within-path "lookup miss"
       :iri-lookup {"mock-iri" "test value"}})
     (handle-matchables
      {:matchable []
       :generated "generated"
       :within-path "lookup miss"
       :iri-lookup {"mock-iri" "test value"}}))
  ;; no special handling of `fallback`
  (= {:non-iri "lookup miss"
      :fallback []}
     (handle-matchables
      {:matchable []
       :generated []
       :within-path "lookup miss"
       :iri-lookup {"mock-iri" "test value"}}))
  ;; but when its *, "" or nil, next priority is `matchable`
  (= "Generated"
     (handle-matchables
      {:matchable (fn [d] (string/capitalize d))
       :generated "generated"
       :within-path nil
       :iri-lookup {}}))
  (= "matchable"
     (handle-matchables
      {:matchable "matchable"
       :generated "generated"
       :within-path :unexpected
       :iri-lookup {}})
     (handle-matchables
      {:matchable "matchable"
       :generated "generated"
       :within-path "*"
       :iri-lookup {}})
     (handle-matchables
      {:matchable "matchable"
       :generated "generated"
       :within-path ""
       :iri-lookup {}})
     (handle-matchables
      {:matchable "matchable"
       :generated "generated"
       :iri-lookup {}}))
  ;; return of `generation` only happens when necessary
  (= "generated"
     (handle-matchables
      {:matchable []
       :generated "generated"
       :within-path nil
       :iri-lookup {}})
     (handle-matchables
      {:matchable nil
       :generated "generated"
       :within-path nil
       :iri-lookup {}})
     (handle-matchables
      {:matchable ""
       :generated "generated"
       :within-path nil
       :iri-lookup {}})
     (handle-matchables
      {:matchable nil
       :generated "generated"
       :within-path :unexpected
       :iri-lookup {}})
     (handle-matchables
      {:generated "generated"
       :within-path "*"
       :iri-lookup {}})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn follow-stmt-path
  [stmt-path & {:keys [] :as passdown}]
  (let [[top-lvl-k] stmt-path
        ?more       (not-empty (everything-but-first stmt-path))]
    (case top-lvl-k
      "id"          "FIXME: handle presence + any/all/none"
      "actor"       "FIXME: ?more is relevant"
      "verb"        "FIXME: ?more is relevant"
      "object"      "FIXME: ?more is relevant"
      "result"      "FIXME: ?more is relevant"
      "context"     "FIXME: ?more is relevant"
      "timestamp"   "FIXME: handle presence + any/all/none"
      "stored"      "FIXME: handle presence + any/all/none"
      "authority"   (throw (ex-info "authority is not currently supported!"
                                    {:stmt-path stmt-path}))
      "version"     (throw (ex-info "version is not currently supported!"
                                    {:stmt-path stmt-path}))
      "attachments" (throw (ex-info "attachments are not currently supported!"
                                    {:stmt-path stmt-path}))
      (throw (ex-info "unexpected JSONPath key after root!"
                      {:path       stmt-path
                       :after-root top-lvl-k
                       :more       ?more})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Per rule fn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn from-rule
  [{:keys [location
           ;; selector FIXME: not currently supported
           ;; scopeNote FIXME: not currently relevant
           presence
           any
           all
           none] :as rule}
   seed]
  (let [{:keys [path nested]} (handle-json-path-str location)
        stmt-path             (deconstruct-json-path path)]
    ;; FIXME: update/impl top-down logic once bottom-up support is fleshed out
    stmt-path))
