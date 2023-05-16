(ns com.yetanalytics.datasim.xapi.statement
  "Generate Statements"
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as cset]
            [clojure.string :as string]
            [clojure.walk :as w]
            [com.yetanalytics.pathetic :as pathetic]
            [com.yetanalytics.datasim.xapi.profile :as profile]
            [com.yetanalytics.datasim.xapi.activity :as activity]
            [com.yetanalytics.datasim.input :as input]
            [com.yetanalytics.datasim.input.alignments :as alignments]
            [com.yetanalytics.datasim.input.personae :as personae]
            [com.yetanalytics.pan.objects.template :as template]
            [com.yetanalytics.datasim.random :as random]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.xapi.profile.template.rule :as rule]
            [com.yetanalytics.datasim.json.schema :as j-schema]
            [com.yetanalytics.datasim.xapi.extensions :as ext]
            [com.yetanalytics.datasim.json.path :as path])
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
  (let [rng           (random/seed-rng seed)
        ?obj-override (some->> alignment
                               not-empty
                               keys
                               (random/choose rng alignment)
                               (get alignment)
                               :object-override
                               w/stringify-keys)
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
                    ;; object override is valid for the template and is present
                    ;; in the alignment
                    ;; - note: ?obj-override will get overriden; we are using
                    ;;   it as a placeholder
                    (when ?obj-override ?obj-override)
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
        (if ?obj-override (assoc stmt "object" ?obj-override) stmt))
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

;; NEW ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def verb-path
  (pathetic/parse-paths "$.verb" {:strict? true}))
(def object-path
  (pathetic/parse-paths "$.object" {:strict? true}))
(def object-definition-type-path
  (pathetic/parse-paths "$.object.definition.type" {:strict? true}))
(def object-type-path
  (pathetic/parse-paths "$.object.type" {:strict? true}))
(def context-activity-category-path
  (pathetic/parse-paths "$.context.contextActivities.category" {:strict? true}))
(def context-activity-grouping-path
  (pathetic/parse-paths "$.context.contextActivities.grouping" {:strict? true}))
(def context-activity-parent-path
  (pathetic/parse-paths "$.context.contextActivities.parent" {:strict? true}))
(def context-activity-other-path
  (pathetic/parse-paths "$.context.contextActivities.other" {:strict? true}))
(def attachment-path
  (pathetic/parse-paths "$.attachments" {:strict? true}))

;; TODO: objectStatementRefTemplate and contextStatementRefTemplate
(defn parse-template
  [profile-iri-map
   {?verb-id                     :verb
    ?object-activity-type        :objectActivityType
    ?ctx-category-activity-types :contextCategoryActivityType
    ?ctx-grouping-activity-types :contextGroupingActivityType
    ?ctx-parent-activity-types   :contextParentActivityType
    ?ctx-other-activity-types    :contextOtherActivityType
    ?attachment-usage-types      :attachmentUsageType
    ?rules                       :rules}]
  (cond-> []
    ?verb-id
    (into [{:location verb-path
            :all      #{(get profile-iri-map ?verb-id)}}])
    ?object-activity-type
    (into [{:location object-path
            :all      #{(-> (get profile-iri-map ?object-activity-type)
                            (assoc "objectType" "Activity"))}}])
    #_(into [{:location object-type-path
            :all      #{"Activity"}}
           {:location object-definition-type-path
            :all      #{?object-activity-type}}])
    ?ctx-category-activity-types
    (into (map-indexed
           (fn [idx activity-type]
             {:location (conj context-activity-category-path #{idx} #{"id"})
              :all      [activity-type]})
           ?ctx-category-activity-types))
    ?ctx-grouping-activity-types
    (into (map-indexed
           (fn [idx activity-type]
             {:location (conj context-activity-grouping-path #{idx} #{"id"})
              :all      [activity-type]})
           ?ctx-grouping-activity-types))
    ?ctx-parent-activity-types
    (into (map-indexed
           (fn [idx activity-type]
             {:location (conj context-activity-parent-path #{idx} #{"id"})
              :all      [activity-type]})
           ?ctx-parent-activity-types))
    ?ctx-other-activity-types
    (into (map-indexed
           (fn [idx activity-type]
             {:location (conj context-activity-other-path #{idx} #{"id"})
              :all      [activity-type]})
           ?ctx-other-activity-types))
    ?attachment-usage-types
    (into (map-indexed
           (fn [idx usage-type]
             {:location (conj attachment-path #{idx} #{"usageType"})
              :all      [usage-type]})
           ?attachment-usage-types))
    ?rules
    (into (map rule/parse-rule-2 ?rules))))

(defn valid-object-override?
  "Is the `object-override` Object valid against the `template`?"
  [{?activity-type          :objectActivityType
    ?statement-ref-template :objectStatementRefTemplate
    ?rules                  :rules
    :as                     _template}
   {override-type "objectType"
    :as object-override}]
  (let [object-rules  (->> ?rules
                           (map rule/parse-rule)
                           (filterv #(-> % :location first (= #{"object"}))))
        override-stmt {"object" object-override}]
    (and (or (= "Activity" override-type)
             (nil? ?activity-type))
         (or (= "StatementRef" override-type)
             (nil? ?statement-ref-template))
         (every? (partial rule/follows-rule? override-stmt)
                 object-rules))))

(comment
  (def object-rule
    {:location "$.object.definition.type"
     :presence "included"
     :any ["https://w3id.org/xapi/cmi5/activitytype/block"
           "https://w3id.org/xapi/cmi5/activitytype/course"]})

  (valid-object-override?
   {:rules [object-rule]}
   {"objectType" "Activity"
    "id"         "https://www.whatever.com/activities#course2"
    "definition"
    {"name"        {"en-US" "Course 2"}
     "description" {"en-US" "Course Description 2"}
     "type"        "https://w3id.org/xapi/cmi5/activitytype/course"}})
  ;; => true

  (valid-object-override?
   {:rules [object-rule]}
   {"objectType" "Agent"
    "name"       "Owen Overrider"
    "mbox"       "mailto:owoverrider@example.com"})
  ;; => false
  )

(defn- group-by-prefix
  "Given `rules-coll`, return a map grouped by the `:location` prefix.
   Concatenates `:selector` onto `:location`. `skip-n` is provided to
   skip `n` entries for the purposes of grouping, e.g. `0` to group by
   the full prefix, or `1` to ignore the very first element in `:location`."
  [skip-n rules-coll]
  (->> rules-coll
       (map
        (fn [{:keys [location selector] :as rule}]
          (-> rule
              (assoc :location (vec (concat location selector)))
              (dissoc :selector))))
       (group-by
        (fn [{:keys [location]}]
          (reduce (fn [prefix k]
                    (if (string? k)
                      (conj prefix k)
                      (reduced prefix)))
                  (subvec location 0 skip-n)
                  (subvec location skip-n))))
       (reduce-kv
        (fn [m prefix rules]
          (->> rules
               (mapv #(update % :location subvec (count prefix)))
               (assoc m prefix)))
        {})))

(defn- get-first-loc [rule]
  (-> rule :location first))

(defn- fill-rule-gaps**
  "Fill in the missing indexes (the first element of `:location`) with
   ```
   {:location [index]
    :presence \"included\"}
   ```
   rules."
  [rules]
  (loop [rules (sort-by get-first-loc rules)
         prev  {:location [-1]}
         new   []]
    (if-let [rule (first rules)]
      (let [fillers (->> (range (-> prev get-first-loc inc)
                                (-> rule get-first-loc))
                         (mapv (fn [idx] {:location [idx] :presence "included"})))]
        
        (recur (rest rules)
               rule
               (into new (conj fillers rule))))
      (vec new))))

(defn- fill-rule-gaps*
  "Group `rules-coll` by `:location` prefixes and return a map from prefixes
   to rules. Recursive; some map values may themselves be prefix-rule maps,
   if the prefix ends with an int (indicating an array). Each missing array
   index is filled in by `fill-rule-gaps**`."
  [skip-n rules-coll]
  (reduce-kv
   (fn [m prefix rules]
     (let [rule-indexes (map get-first-loc rules)]
       (condp #(every? %1 %2) rule-indexes
         (comp not int?)
         (assoc m prefix rules)
         (some-fn nil? int?)
         (assoc m
                prefix
                (merge (->> rules ; in case some rules terminate early
                            (filter (comp nil? get-first-loc))
                            (fill-rule-gaps* 0))
                       (->> rules
                            (filter (comp int? get-first-loc))
                            fill-rule-gaps**
                            (fill-rule-gaps* 1))))
         ;; else
         (throw (ex-info "Cannot mix array indexes with properties"
                         {:type  ::invalid-rules
                          :rules rules-coll})))))
   {}
   (group-by-prefix skip-n rules-coll)))

(defn- restore-locs
  "Given the return value of `fill-rule-gaps*`, returns a vector of rules
   sorted by `:location` and with mising array indexes filled in."
  [prefix-rule-m]
  (reduce-kv (fn [acc prefix rules]
               (let [rules* (cond-> rules (map? rules) restore-locs)]
                 (->> rules*
                      (map #(update % :location (partial into prefix)))
                      (into acc))))
             []
             prefix-rule-m))

(defn- fill-rule-gaps
  "Given `rules-coll`, sort them by `:location` (with `:selector folded into
   that vector) and fill in any missing array indexes (to avoid
   IndexOutOfBoundsException errors when assoc'ing arrays)."
  [rules-coll]
  (->> rules-coll (fill-rule-gaps* 0) restore-locs))

(comment 
  (group-by-prefix
   0
   [{:location ["context" "contextActivity" "grouping" 1 "id"]
     :any ["http://example.com/1"]}
    {:location ["context" "contextActivity" "grouping" 3 "id" 0]
     :any ["http://example.com/3-a"]}
    {:location ["context" "contextActivity" "grouping" 3 "id" 2]
     :any ["http://example.com/3-b"]}])
  
  (group-by-prefix
   1
   [{:location [0], :presence "included"}
    {:location [1 "id"], :any ["http://example.com/1"]}
    {:location [2], :presence "included"}
    {:location [3 "id" 0], :any ["http://example.com/3-a"]}
    {:location [3 "id" 2], :any ["http://example.com/3-b"]}]) 
  
  (fill-rule-gaps
   [{:location ["id"]
     :any ["http://example.com/the-id"]}
    {:location ["context" "contextActivity" "grouping" 1 "id"]
     :any ["http://example.com/1"]}
    {:location ["context" "contextActivity" "grouping" 3 "id" 0]
     :any ["http://example.com/3-a"]}
    {:location ["context" "contextActivity" "grouping" 3 "id" 2]
     :any ["http://example.com/3-b"]}])
  
  (fill-rule-gaps
   [{:location ["id"]
     :any ["http://example.com/1"]}
    {:location ["id"]
     :any ["http://example.com/2"]}])
  
  (fill-rule-gaps
   [{:location ["context" "contextActivity" "grouping"]
     :presence "included"}
    {:location ["context" "contextActivity" "grouping" 0 "id"]
     :any ["http://example.com/2"]}
    {:location ["context" "contextActivity" "grouping" 2 "id"]
     :any ["http://example.com/2"]}])
  )

(def default-verb
  {:id        "http://adlnet.gov/expapi/verbs/experienced" 
   :type      "Verb"
   :prefLabel {:en "experienced"}})

(defn- profile-verb
  [iri-map rng alignment]
  (let [verbs*    (reduce-kv (fn [m k v]
                               (cond-> m (#{"Verb"} (:type v)) (assoc k v)))
                             {}
                             iri-map)
        verbs     (or (not-empty verbs*)
                      {(:id default-verb) default-verb})
        verb-id   (random/choose rng alignment (keys verbs*))
        verb      (get verbs verb-id)
        ?lang-map (get verb :prefLabel)]
    (cond-> {"id" verb-id}
      ?lang-map
      (assoc "display" (w/stringify-keys ?lang-map)))))

(defn- profile-activity
  [activities rng alignment]
  (let [activities* (reduce merge (vals activities))
        activity-id (random/choose rng alignment (keys activities*))
        activity    (get activities* activity-id)]
    activity))

(defn- registration-rules
  ([registration]
   (registration-rules registration nil nil))
  ([registration sub-registration profile-id]
   (cond-> [{:location ["context" "registration"]
             :presence "included"
             :all      [registration]}]
     (and (some? sub-registration)
          (some? profile-id))
     (conj {:location ["context" "extensions" "https://w3id.org/xapi/profiles/extensions/subregistration"]
            :presence "included"
            :all      [{"profile"         profile-id
                        "subregistration" sub-registration}]}))))

(defn- profile-context-rule
  [profile-version-id]
  {:location "$.context.contextActivities.category[*].id"
   :any      [profile-version-id]})

(defn- object-override-rule
  [{:keys [rng alignment template]}]
  (let [valid-override? (partial valid-object-override? template)
        object-override (some->> alignment
                                 (filter valid-override?)
                                 not-empty
                                 keys
                                 (random/choose rng alignment)
                                 (get alignment)
                                 :object-override
                                 w/stringify-keys)]
    (when object-override
      {:location "$.object"
       :all      [object-override]})))
