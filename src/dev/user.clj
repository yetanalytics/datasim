(ns user
  (:require [clojure.repl :refer [source doc apropos]]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as cset]
            [clojure.string :as s]
            [com.yetanalytics.datasim.runtime :as runtime]
            [com.yetanalytics.datasim.json.path :as json-path]
            [com.yetanalytics.datasim.sim :as sim]
            [com.yetanalytics.datasim.xapi.activity :as activity]
            [com.yetanalytics.datasim.xapi.profile.template.rule :as rule]
            [com.yetanalytics.datasim.input :as input]
            [clojure.java.io :as io]
            [clojure.core.match :refer [match]]
            [com.yetanalytics.datasim.util.sequence :as su]
            [cheshire.core :as json]))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dev tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pp-json-to-file
  "helper for pprinting JSON `data` to `path`"
  [path data]
  (json/generate-stream data (io/writer (io/as-file path)) {:pretty {:indent-arrays? true}}))

(defn clean-external-profiles
  [profile target-id-set]
  (-> profile
      (dissoc :templates :patterns)
      (update :concepts (fn [concepts] (filterv (fn [{:keys [id]}] (cset/subset? #{id} target-id-set)) concepts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATASIM input - profiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def test-profile
  (input/from-location
   :profile
   :json
   "./dev-resources/profiles/tccc/cuf_hc_video_and_asm_student_survey_profile.jsonld"))

(def video-profile
  (clean-external-profiles
   (input/from-location
    :profile
    :json
    "./dev-resources/profiles/video/profile.jsonld")
   #{"https://w3id.org/xapi/video/verbs/paused"
     "https://w3id.org/xapi/video/verbs/played"
     "https://w3id.org/xapi/video/verbs/seeked"
     "https://w3id.org/xapi/video/activity-type/video"
     "https://w3id.org/xapi/video/extensions/length"
     "https://w3id.org/xapi/video/extensions/volume"}))

(def activity-stream-profile
  (clean-external-profiles
   (input/from-location
    :profile
    :json
    "./dev-resources/profiles/activity_streams/profile.jsonld")
   #{"http://activitystrea.ms/start"
     "http://activitystrea.ms/submit"}))

(def acrossx-profile
  (clean-external-profiles
   (input/from-location
    :profile
    :json
    "./dev-resources/profiles/acrossx/profile.jsonld")
   #{"https://w3id.org/xapi/acrossx/activities/page"}))

(def tincan-profile
  (clean-external-profiles
   (input/from-location
    :profile
    :json
    "./dev-resources/profiles/tincan/profile.jsonld")
   #{"http://id.tincanapi.com/verb/skipped"}))

(def testing-profiles
  [test-profile video-profile activity-stream-profile acrossx-profile tincan-profile])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATASIM input - personae
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-perona
  [name]
  (let [[f-name l-name] (s/split name #" ")
        mbox (format "mailto:%s@example.org" (s/lower-case f-name))]
    {:name name
     :mbox mbox}))

(defn create-personae
  [group-name member-names]
  {:name group-name
   :objectType "Group"
   :member (mapv create-perona member-names)})

(def example-personae
  ;; used https://www.name-generator.org.uk/last/ for last name generation
  (pp-json-to-file "./dev-resources/personae/tccc_dev.json"
                   (create-personae "trainees"
                                    ["Bob Nelson"
                                     "Phil Walker"
                                     "Sally Davis"
                                     "Steve Stewart"
                                     "Fred Evans"
                                     "Alice Edwards"])))

(def test-personae
  (input/from-location
   :personae
   :json
   "./dev-resources/personae/tccc_dev.json"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATASIM input - Alignments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-concept
  [concept]
  (let [{id                   :id
         label-lmap           :prefLabel
         c-type               :type
         {a-name-lmap :name}  :activityDefinition} (select-keys concept [:id :prefLabel :type :activityDefinition])
        label  (first (vals label-lmap))
        a-name (first (vals a-name-lmap))]
    (cond-> {:id id :type c-type}
      (string? label)  (assoc :label label)
      (string? a-name) (assoc :activity-name a-name))))

(defn parse-templates
  [template]
  (let [{id         :id
         t-type     :type
         label-lmap :prefLabel} (select-keys template [:id :type :prefLabel])
        label (first (vals label-lmap))]
    (if (string? label)
      {:id id :type t-type :label label}
      {:id id :type t-type})))

(defn parse-pattern
  [pattern]
  (let [{id         :id
         p-type     :type
         primary    :primary
         label-lmap :prefLabel} (select-keys pattern [:id :type :primary :prefLabel])
        label (first (vals label-lmap))]
    (cond-> {:id id :type p-type}
      (some? primary) (assoc :primary primary)
      (string? label) (assoc :label label))))

(defn in-profile
  [profile]
  (let [{concepts  :concepts
         templates :templates
         patterns  :patterns
         id        :id} profile
        c-ms            (mapv parse-concept concepts)
        t-ms            (mapv parse-templates templates)
        p-ms            (mapv parse-pattern patterns)]
    (cond-> {:alignment-options/profile-id id}
      (not-empty c-ms) (assoc :alignment-options/concepts c-ms)
      (not-empty t-ms) (assoc :alignment-options/templates t-ms)
      (not-empty p-ms) (assoc :alignment-options/patterns p-ms))))

(defn across-profiles
  [profiles]
  (let [parsed-profiles (mapv in-profile profiles)
        all-things      (reduce-kv
                         (fn [ack _ v]
                           (let [{c :alignment-options/concepts
                                  t :alignment-options/templates
                                  p :alignment-options/patterns} v]
                             (cond-> ack
                               (not-empty c) (update :alignment-options/concepts (fnil (fn [old] (into old c)) []))
                               (not-empty t) (update :alignment-options/templates (fnil (fn [old] (into old t)) []))
                               (not-empty p) (update :alignment-options/patterns (fnil (fn [old] (into old p)) [])))))
                         {} parsed-profiles)]
    {:across-all-profiles all-things
     :per-profile         parsed-profiles}))

(defn create-alignments-cosmos
  [profiles]
  (across-profiles profiles))

(defn- query-by-name
  [search-space the-name]
  (filterv (fn [{:keys [activity-name]}] (= the-name activity-name)) search-space))

(defn- query-by-label
  [search-space the-label]
  (filterv (fn [{:keys [label]}] (= the-label label)) search-space))

(defn- query-by-id
  [search-space the-id]
  (filterv (fn [{:keys [id]}] (= the-id id)) search-space))

(defn- query-for-non-primary-patterns
  [search-space]
  (filterv (fn [{:keys [primary]}] (not primary)) search-space))

(defn query-alignment-cosmos
  "given a `cosmos` (see `create-alignments-cosmos`) returns either
    a) result of querying `cosmos`, query keys are combinatory and either result in scoping `cosmos`, querying `cosmos` or both.
       Query result will either be a single map or a non-empty collection of maps when successful
       -> `a-profile-id` (scope) search space to particular profile instead of across all profiles in `cosmos`
       -> `a-name` (scope, query) search space to activity concepts, returns activity concept when its activityDefinition.name language map text is `a-name`
       -> `a-primary` (scope) search space to patterns whose `primary` is `a-primary`
       -> `a-type` (scope) search space to the profile components whose `type` is `a-type`
       -> `a-label` (scope, query) search space to the profile components whose `prefLabel` language map text is `a-label`
       -> `a-id` (query) search space for the profile components whose `id` is `a-id`
    b) a map of {:refined-search-space `refined-search-space`} when a query result returns `nil`
    c) error when unable to create `search-space` from `cosmos`"
  [cosmos {:keys [a-id a-label a-type a-primary a-name a-profile-id] :as query}]
  (if-let [search-space (if (string? a-profile-id)
                          (->> cosmos
                               :per-profile
                               (filterv (fn [coll] (= a-profile-id (:alignment-options/profile-id coll))))
                               first
                               not-empty)
                          (->> cosmos
                               :across-all-profiles
                               not-empty))]
    (let [refined-search-space* (cond (string? a-name) ;; must be an activity
                                      (->> search-space
                                           :alignment-options/concepts
                                           (filterv (fn [concept] (= "Activity" (:type concept)))))
                                      (some? a-primary) ;; must be a pattern
                                      (->> search-space
                                           :alignment-options/patterns
                                           ;; patterns with `primary` = `nil` should match `a-primary` = `false`
                                           (filterv (fn [pattern] (if a-primary (:primary pattern) (not (:primary pattern))))))
                                      (some? a-type) ;; must be a concept, template or pattern
                                      (if (contains? #{"Verb" "ActivityType" "AttachmentUsageType" "ContextExtension" "ResultExtension" "ActivityExtension"
                                                       "StateResource" "AgentProfileResource" "ActivityProfileResource" "Activity"} a-type)
                                        (->> search-space
                                             :alignment-options/concepts
                                             (filterv (fn [concept] (= a-type (:type concept)))))
                                        (case a-type
                                          "StatementTemplate" (:alignment-options/templates search-space)
                                          "Pattern"           (:alignment-options/patterns search-space)))
                                      :else search-space)
          refined-search-space (if (vector? refined-search-space*)
                                 refined-search-space*
                                 ;; single vector of all valid query results
                                 (reduce-kv (fn [ack k v]
                                              (if (contains? #{:alignment-options/concepts
                                                               :alignment-options/templates
                                                               :alignment-options/patterns} k)
                                                (into ack v)
                                                ack))
                                            [] refined-search-space*))]
      (letfn [(qresult-or-default [this]
                (cond (map? this)
                      this
                      (vector? this)
                      (case (count this)
                        0 {:refined-search-space refined-search-space}
                        1 (first this)
                        this)
                      :else {:refined-search-space refined-search-space}))]
        (qresult-or-default
         (match [a-id a-label a-type a-name a-primary]
                ;; thing with id = `a-id`
                [(_ :guard string?) _ _ _ _]
                (query-by-id refined-search-space a-id)

                ;; activity with `a-name`
                [_ _ _ (_ :guard string?) _]
                (query-by-name refined-search-space a-name)

                ;; non-primary patterns with `a-label`
                [_ (_ :guard string?) "Pattern" _ nil]
                (-> refined-search-space query-for-non-primary-patterns (query-by-label a-label))

                ;; non-primary patterns with `a-label`
                [_ (_ :guard string?) _ _ false]
                (query-by-label refined-search-space a-label)

                ;; primary patterns with `a-label`
                [_ (_ :guard string?) _ _ true]
                (query-by-label refined-search-space a-label)

                ;; pattern where `primary` is `false` or `nil`
                [_ _ "Pattern" _ nil]
                (query-for-non-primary-patterns refined-search-space)

                ;; things of the specified `a-type` and `a-label`
                [_ (_ :guard string?) (_ :guard string?) _ _]
                (query-by-label refined-search-space a-label)

                ;; things with a label text = `a-label`
                [_ (_ :guard string?) _ _ _]
                (query-by-label refined-search-space a-label)

                ;; things of the specified `a-type`
                [_ _ (_ :guard string?) _ _]
                refined-search-space

                ;; pattern where `primary` is `true`
                [_ _ _ _ true]
                refined-search-space

                ;; pattern where `primary` is `false` or `nil`
                [_ _ _ _ false]
                refined-search-space))))
    (throw (ex-info "unable to derive search space!"
                    {:cosmos-useable? (some? (try (not-empty cosmos) (catch Exception e nil)))
                     :cosmos-query {:alignment-target-id      a-id
                                    :alignment-target-label   a-label
                                    :alignment-target-type    a-type
                                    :alignment-target-name    a-name
                                    :alignment-target-primary a-primary
                                    :alignment-target-profile a-profile-id}}))))

(defn alignment-for-agent
  [cosmos agent-alignment-k coll-query-to-alignment & {:keys [accumulator]}]
  (reduce-kv
   (fn [ack _ [cosmos-query-m alignment-n]]
     (let [q-result (query-alignment-cosmos cosmos cosmos-query-m)]
       (if (:refined-search-space q-result)
         (throw (ex-info "alignment cosmos query failure!"
                         {:cosmos-query-m cosmos-query-m
                          :query-result   q-result}))
         (cond (map? q-result)
               (assoc-in ack [agent-alignment-k (:id q-result)] alignment-n )
               (vector? q-result)
               (reduce-kv (fn [accum _ thing-id]
                            (assoc-in accum [agent-alignment-k thing-id] alignment-n))
                          ack (mapv :id q-result))
               :else ack))))
   (or accumulator {})
   coll-query-to-alignment))

(defn create-alignments
  "`agent-alignments-m` looks like: {`agent-alignment-id` [[`query`, `alignment-val-for-query-result`] [...] ...]}"
  [cosmos agent-alignments-m & {:keys [print-to-file? file-path]
                                :or {print-to-file? false
                                     file-path      "./dev-resources/alignments/user.json"}}]
  (let [alignment-m (reduce-kv (fn [ack agent-alignment-id coll-query-to-alignment]
                                 (alignment-for-agent
                                  cosmos agent-alignment-id coll-query-to-alignment :accumulator ack))
                               {} agent-alignments-m)]
    (when print-to-file? (pp-json-to-file file-path alignment-m))
    alignment-m))

(comment
  (def example-alignments-cosmos
    (let [ex-cosmos (create-alignments-cosmos testing-profiles)]
      #_(pp-json-to-file "./dev-resources/alignments/example_iri_cosmos.json"
                         ex-cosmos)
      ex-cosmos))

  (= {:id "https://books.allogy.com/v1/tenant/8/media/8e30b016-b97f-48e6-aece-98fd3664f380",
      :type "Activity",
      :activity-name "Care Under Fire Hemorrhage Control"}
     (query-alignment-cosmos
      example-alignments-cosmos
      {:a-id "https://books.allogy.com/v1/tenant/8/media/8e30b016-b97f-48e6-aece-98fd3664f380"})

     (query-alignment-cosmos
      example-alignments-cosmos
      {:a-name "Care Under Fire Hemorrhage Control"}))

  (def ex-actor-alignment-map
    {"mbox::mailto:bob@example.org"
     [;; verbs
      [{:a-id "https://w3id.org/xapi/video/verbs/paused"} 1.0]
      [{:a-id "https://w3id.org/xapi/video/verbs/seeked"} -1.0]
      [{:a-id "http://id.tincanapi.com/verb/skipped"} 1.0]
      ;; video activities
      [{:a-name "Care Under Fire Hemorrhage Control"} -1.0]
      ;; Self > CAT > SOFT-T > looped, routed
      [{:a-name "CAT Self (looped)"} 1.0]
      [{:a-name "CAT Self (routed)"} 1.0]
      [{:a-name "CAT Buddy (routed)"} -1.0]
      [{:a-name "SOFT-T Self (looped)"} 0.50]
      [{:a-name "SOFT-T Self (routed)"} 0.50]
      [{:a-name "SOFT-T Buddy (looped)"} -1.0]
      [{:a-name "SOFT-T Buddy (routed)"} -1.0]
      ;; video templates - alignment generic to template action not object!
      [{:a-label "Played"
        :a-type "StatementTemplate"} 1.0]
      [{:a-label "Paused"
        :a-type "StatementTemplate"} 1.0]
      [{:a-label "Seeked"
        :a-type "StatementTemplate"} -1.0]
      [{:a-label "Completed"
        :a-type "StatementTemplate"} 0.50]
      [{:a-label "Volume Change Interaction"
        :a-type "StatementTemplate"} 0.0]
      ;; patterns
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#survey-lifecycle"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#intro-video-lifecycle"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#tourniquet-cat-soft"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-pairs-single-double"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-pairs-single-double"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-self-loop-routed"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-buddy-loop-routed"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-self-loop-routed"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#tourniquet-soft-cat"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-pairs-double-single"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-pairs-double-single"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-self-routed-loop"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-buddy-routed-loop"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-self-routed-loop"} -1.0]]
     "mbox::mailto:phil@example.org"
     [;; verbs
      [{:a-id "https://w3id.org/xapi/video/verbs/paused"} 0.50]
      [{:a-id "https://w3id.org/xapi/video/verbs/seeked"} -0.50]
      [{:a-id "http://id.tincanapi.com/verb/skipped"} 0.50]
      ;; video activities
      [{:a-name "Care Under Fire Hemorrhage Control"} 1.0]
      ;; buddy > CAT > SOFT-T > looped > routed
      [{:a-name "CAT Self (looped)"} 0.50]
      [{:a-name "CAT Self (routed)"} 0.25]
      [{:a-name "CAT Buddy (routed)"} 0.75]
      [{:a-name "SOFT-T Self (looped)"} -0.25]
      [{:a-name "SOFT-T Self (routed)"} -0.50]
      [{:a-name "SOFT-T Buddy (looped)"} 0.75]
      [{:a-name "SOFT-T Buddy (routed)"} 0.50]
      ;; video templates - alignment generic to template action not object!
      [{:a-label "Played"
        :a-type "StatementTemplate"} 0.50]
      [{:a-label "Paused"
        :a-type "StatementTemplate"} 0.50]
      [{:a-label "Seeked"
        :a-type "StatementTemplate"} -0.50]
      [{:a-label "Completed"
        :a-type "StatementTemplate"} 0.75]
      [{:a-label "Volume Change Interaction"
        :a-type "StatementTemplate"} -0.50]
      ;; patterns
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#intro-video-lifecycle"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#survey-lifecycle"} 0.75]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#tourniquet-soft-cat"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#tourniquet-cat-soft"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-pairs-double-single"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-pairs-single-double"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-pairs-double-single"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-pairs-single-double"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-self-routed-loop"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-self-loop-routed"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-buddy-routed-loop"} 0.50]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-buddy-loop-routed"} 0.75]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-self-routed-loop"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-self-loop-routed"} -1.0]]
     "mbox::mailto:sally@example.org"
     [;; verbs
      [{:a-id "https://w3id.org/xapi/video/verbs/paused"} 0.25]
      [{:a-id "https://w3id.org/xapi/video/verbs/seeked"} -0.25]
      [{:a-id "http://id.tincanapi.com/verb/skipped"} 0.25]
      ;; video activities
      [{:a-name "Care Under Fire Hemorrhage Control"} 1.0]
      ;; SOFT-T > routed > looped > buddy > self > CAT
      [{:a-name "CAT Self (looped)"} -0.75]
      [{:a-name "CAT Self (routed)"} -0.50]
      [{:a-name "CAT Buddy (routed)"} -0.25]
      [{:a-name "SOFT-T Self (looped)"} 0.25]
      [{:a-name "SOFT-T Self (routed)"} 0.50]
      [{:a-name "SOFT-T Buddy (looped)"} 0.75]
      [{:a-name "SOFT-T Buddy (routed)"} 1.0]
      ;; video templates - alignment generic to template action not object!
      [{:a-label "Played"
        :a-type "StatementTemplate"} 0.25]
      [{:a-label "Paused"
        :a-type "StatementTemplate"} 0.25]
      [{:a-label "Seeked"
        :a-type "StatementTemplate"} -0.25]
      [{:a-label "Completed"
        :a-type "StatementTemplate"} 0.25]
      [{:a-label "Volume Change Interaction"
        :a-type "StatementTemplate"} -0.75]
      ;; patterns
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#intro-video-lifecycle"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#survey-lifecycle"} 0.50]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#tourniquet-soft-cat"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-pairs-double-single"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-pairs-double-single"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-self-routed-loop"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-buddy-routed-loop"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-self-routed-loop"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#tourniquet-cat-soft"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-pairs-single-double"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-pairs-single-double"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-self-loop-routed"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-buddy-loop-routed"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-self-loop-routed"} -1.0]]
     "mbox::mailto:steve@example.org"
     [;; verbs
      [{:a-id "https://w3id.org/xapi/video/verbs/paused"} -0.25]
      [{:a-id "https://w3id.org/xapi/video/verbs/seeked"} 0.25]
      [{:a-id "http://id.tincanapi.com/verb/skipped"} -0.25]
      ;; video activities
      [{:a-name "Care Under Fire Hemorrhage Control"} 1.0]
      ;; routed > looped > self > buddy > SOFT-T > CAT
      [{:a-name "CAT Self (looped)"} -0.25]
      [{:a-name "CAT Self (routed)"} 0.25]
      [{:a-name "CAT Buddy (routed)"} 0.0]
      [{:a-name "SOFT-T Self (looped)"} 0.75]
      [{:a-name "SOFT-T Self (routed)"} 1.0]
      [{:a-name "SOFT-T Buddy (looped)"} 0.50]
      [{:a-name "SOFT-T Buddy (routed)"} 0.75]
      ;; video templates - alignment generic to template action not object!
      [{:a-label "Played"
        :a-type "StatementTemplate"} 0.0]
      [{:a-label "Paused"
        :a-type "StatementTemplate"} -0.25]
      [{:a-label "Seeked"
        :a-type "StatementTemplate"} 0.25]
      [{:a-label "Completed"
        :a-type "StatementTemplate"} 1.0]
      [{:a-label "Volume Change Interaction"
        :a-type "StatementTemplate"} -1.0]
      ;; patterns
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#intro-video-lifecycle"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#survey-lifecycle"} 0.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#tourniquet-soft-cat"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#tourniquet-cat-soft"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-pairs-single-double"} -0.75]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-pairs-double-single"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-pairs-single-double"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-self-routed-loop"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-self-loop-routed"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-pairs-double-single"} 0.75]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-self-routed-loop"} 0.75]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-self-loop-routed"} 0.50]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-buddy-routed-loop"} 0.75]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-buddy-loop-routed"} 0.50]]
     "mbox::mailto:fred@example.org"
     [;; verbs
      [{:a-id "https://w3id.org/xapi/video/verbs/paused"} -0.50]
      [{:a-id "https://w3id.org/xapi/video/verbs/seeked"} 0.50]
      [{:a-id "http://id.tincanapi.com/verb/skipped"} -0.5]
      ;; video activities
      [{:a-name "Care Under Fire Hemorrhage Control"} 1.0]
      ;;  looped > routed > CAT > SOFT > buddy > self
      [{:a-name "CAT Self (looped)"} 1.0]
      [{:a-name "CAT Self (routed)"} 0.50]
      [{:a-name "CAT Buddy (routed)"} 0.75]
      [{:a-name "SOFT-T Self (looped)"} 0.50]
      [{:a-name "SOFT-T Self (routed)"} 0]
      [{:a-name "SOFT-T Buddy (looped)"} 0.75]
      [{:a-name "SOFT-T Buddy (routed)"} 0.50]
      ;; video templates - alignment generic to template action not object!
      [{:a-label "Played"
        :a-type "StatementTemplate"} -0.25]
      [{:a-label "Paused"
        :a-type "StatementTemplate"} -0.50]
      [{:a-label "Seeked"
        :a-type "StatementTemplate"} 0.50]
      [{:a-label "Completed"
        :a-type "StatementTemplate"} -1.0]
      [{:a-label "Volume Change Interaction"
        :a-type "StatementTemplate"} 1.0]
      ;; patterns
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#intro-video-lifecycle"} 1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#survey-lifecycle"} -0.50]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#tourniquet-soft-cat"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#tourniquet-cat-soft"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-pairs-double-single"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-pairs-single-double"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-pairs-double-single"} -0.75]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-pairs-single-double"} 0.75]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-self-routed-loop"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-self-loop-routed"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-buddy-routed-loop"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-buddy-loop-routed"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-self-routed-loop"} 0.25]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-self-loop-routed"} 0.75]]
     "mbox::mailto:alice@example.org"
     [;; verbs
      [{:a-id "https://w3id.org/xapi/video/verbs/paused"} -1.0]
      [{:a-id "https://w3id.org/xapi/video/verbs/seeked"} -1.0]
      [{:a-id "http://id.tincanapi.com/verb/skipped"} -1.0]
      ;; video activities
      [{:a-name "Care Under Fire Hemorrhage Control"} -1.0]
      ;; doesn't like anything equally
      [{:a-name "CAT Self (looped)"} -1.0]
      [{:a-name "CAT Self (routed)"} -1.0]
      [{:a-name "CAT Buddy (routed)"} -1.0]
      [{:a-name "SOFT-T Self (looped)"} -1.0]
      [{:a-name "SOFT-T Self (routed)"} -1.0]
      [{:a-name "SOFT-T Buddy (looped)"} -1.0]
      [{:a-name "SOFT-T Buddy (routed)"} -1.0]
      ;; video templates - alignment generic to template action not object!
      [{:a-label "Played"
        :a-type "StatementTemplate"} -0.50]
      [{:a-label "Paused"
        :a-type "StatementTemplate"} -1.0]
      [{:a-label "Seeked"
        :a-type "StatementTemplate"} -1.0]
      [{:a-label "Completed"
        :a-type "StatementTemplate"} 0.0]
      [{:a-label "Volume Change Interaction"
        :a-type "StatementTemplate"} 0.0]
      ;; patterns
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#intro-video-lifecycle"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#survey-lifecycle"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#tourniquet-soft-cat"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#tourniquet-cat-soft"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-pairs-double-single"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-pairs-single-double"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-pairs-double-single"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-pairs-single-double"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-buddy-routed-loop"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-buddy-loop-routed"} -1.0]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-self-routed-loop"} 0.50]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#soft-self-loop-routed"} 0.50]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-self-routed-loop"} 0.50]
      [{:a-id "https://xapinet.org/xapi/tc3/cuf/hemorrhage_control/v1/patterns#cat-self-loop-routed"} 0.50]]})

  (def example-alignment
    (create-alignments
     (create-alignments-cosmos testing-profiles)
     ex-actor-alignment-map
     :print-to-file? true
     :file-path      "./dev-resources/alignments/tccc_dev.json"))
  )

(def test-alignments
  (input/from-location
   :alignments
   :json
   "./dev-resources/alignments/tccc_dev.json"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATASIM input - Parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-parameters
  [& {:keys [start-ts end-ts timezone seed]
      :or {start-ts "2020-02-20T11:38:39.219768Z"
           end-ts "2020-02-20T17:38:39.219768Z"
           timezone "America/New_York"
           seed 88420}}]
  {:start    start-ts
   :end      end-ts
   :timezone timezone
   :seed     seed})

(def example-parameters
  (pp-json-to-file "./dev-resources/parameters/tccc_dev.json"
                   (create-parameters
                    :start-ts "2020-02-10T11:38:39.219768Z"
                    :end-ts "2020-02-25T17:38:39.219768Z"
                    :timezone "America/New_York"
                    :seed 40)))

(def test-parameters
  (input/from-location
   :parameters
   :json
   "./dev-resources/parameters/tccc_dev.json"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATASIM input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def test-input
  {:profiles [test-profile video-profile activity-stream-profile acrossx-profile tincan-profile]
   :personae test-personae
   :alignments test-alignments
   :parameters test-parameters})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATASIM run
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-sim!
  []
  (sim/build-skeleton test-input))

(comment

  (def test-cosmos
    (activity/derive-cosmos
     test-input
     (-> test-input :parameters :seed)))

  (clojure.pprint/pprint test-cosmos)

  (def sim-run (run-sim!))
  
  (->> sim-run
       first
       second
       (sort-by (fn [{:strs [timestamp]}] timestamp))
       clojure.pprint/pprint)
  )
