(ns com.yetanalytics.datasim-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.math       :as math]
            [clojure.core.async :as a]
            [clojure.spec.alpha :as s]
            [java-time.api      :as t]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.datasim.test-constants :as const]
            [com.yetanalytics.datasim.sim :as sim]
            [com.yetanalytics.datasim :refer [generate-map
                                              generate-seq
                                              generate-map-async
                                              generate-seq-async]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- get-timestamp [sim-statement]
  (get sim-statement "timestamp"))

(defn- get-object [sim-statement]
  (get sim-statement "object"))

(defn- get-actor-mbox [sim-statement]
  (get-in sim-statement ["actor" "mbox"]))

(defn- get-context-category-activities [sim-statement]
  (get-in sim-statement ["context" "contextActivities" "category"]))

(def alice-mailto
  "mailto:alicefaux@example.org")
(def bob-mailto
  "mailto:bobfake@example.org")
(def fred-mailto
  "mailto:frederstaz@example.org")

(def alice-mbox
  "mbox::mailto:alicefaux@example.org")
(def bob-mbox
  "mbox::mailto:bobfake@example.org")
(def fred-mbox
  "mbox::mailto:frederstaz@example.org")

(def cmi5-id
  "https://w3id.org/xapi/cmi5")
(def cmi5-version-id
  "https://w3id.org/xapi/cmi5/v1.0")
(def cmi5-moveon-id
  "https://w3id.org/xapi/cmi5/context/categories/moveon")

(def tla-version-id
  "https://w3id.org/xapi/tla/v0.13")
(def tla-completed-session-id
  "https://w3id.org/xapi/tla#completed_session")

(def referential-completed-session-id
  "https://xapinet.org/xapi/yet/referential#completed_session")

(def override-1
  {"objectType" "Activity"
   "id"         "https://www.whatever.com/activities#course2"
   "definition"
   {"name"        {"en-US" "Course 2"}
    "description" {"en-US" "Course Description 2"}
    "type"        "http://adlnet.gov/expapi/activities/course"}})

(def override-2
  {"objectType" "Agent"
   "name"       "Owen Overrider"
   "mbox"       "mailto:owoverrider@example.com"})

(def double-profile-input
  (update const/simple-input :profiles conj const/mom-profile))

(def no-concepts-profile-input
  (assoc const/simple-input :profiles [const/no-concept-profile]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest generate-map-test
  (testing "Given valid input, returns a valid skeleton"
    (is (s/valid? ::sim/skeleton (generate-map const/simple-input))))
  (testing "Profiles w/o concepts count as valid input"
    (is (s/valid? ::sim/skeleton (generate-map no-concepts-profile-input))))
  (testing "Make sure RNGs aren't shared across threads."
    (let [skeleton (generate-map (assoc-in const/simple-input
                                           [:parameters :end]
                                           nil))]
      (are [actor-id] (let [statement-seq (get skeleton
                                               actor-id)
                            f1 (future (nth statement-seq 1000))
                            f2 (future (nth statement-seq 1000))]
                        (= @f1 @f2))
        alice-mbox
        bob-mbox
        fred-mbox)))
  (testing "Simulation returns valid xapi statements"
    (let [skeleton (generate-map (assoc-in const/simple-input
                                           [:parameters :end]
                                           nil))]
      (are [actor-id] (s/valid? (s/every ::xs/statement)
                                (get skeleton actor-id))
        alice-mbox
        bob-mbox
        fred-mbox)))
  (testing "We can iterate for a long time w/o a stack overflow"
    (is (s/valid? ::xs/statement
                  (-> const/simple-input
                      (assoc-in [:parameters :end] nil)
                      generate-map
                      (get bob-mbox)
                      (nth 10000)))))
  (testing "We respect temporal properties for different actors"
    (let [satisfied "http://adlnet.gov/expapi/verbs/satisfied"
          ms-in-hr  3600000
          start-t   (-> const/simple-input
                        (get-in [:parameters :start]))
          input     (-> const/simple-input
                        (assoc :models const/temporal-models)
                        (assoc-in [:parameters :end] nil))
          result    (generate-map input)
          ->diffs   (fn [result-seq]
                      (map (fn [{t1 "timestamp"}
                                {t2 "timestamp" {verb "id"} "verb"}]
                             {:verb verb
                              :diff (t/time-between
                                     (t/instant t1)
                                     (t/instant t2)
                                     :millis)})
                           (cons {"timestamp" start-t} result-seq)
                           result-seq))]
      (testing "- Bob: satisfieds happen on the order of seconds, other verbs on the order of hours"
        (is (->> (get result bob-mbox)
                 (take 100)
                 ->diffs
                 (every? (fn [{:keys [verb diff]}]
                           (or (and (= verb satisfied)
                                    (< diff ms-in-hr))
                               (< ms-in-hr diff)))))))
      (testing "- Alice: all verbs happen on the order of minutes"
        (is (->> (get result alice-mbox)
                 (take 100)
                 ->diffs
                 (every? (fn [{:keys [diff]}]
                           (< diff ms-in-hr)))))))))

(deftest generate-seq-test
  (testing "Returns statements"
    (is (s/valid? (s/every ::xs/statement) (generate-seq const/simple-input))))
  (testing "Returns statements even without concepts"
    (is (s/valid? (s/every ::xs/statement) (generate-seq no-concepts-profile-input))))
  (testing "Respects `max` param"
    (let [input  (assoc-in const/simple-input [:parameters :max] 3)
          result (generate-seq input)]
      (is (s/valid? (s/every ::xs/statement) result))
      (is (= 3 (count result)))))
  (testing "Respects `from` param"
    (let [[s0 s1 & _] (generate-seq const/simple-input)
          from-input  (assoc-in const/simple-input
                                [:parameters :from]
                                (get-timestamp s0))
          [s1' & _]   (generate-seq from-input)]
      (is (not= s0 s1'))
      (is (= s1 s1'))))
  (testing "Respects `gen-profiles` param (w/ multiple profiles)"
    (let [input    (update double-profile-input
                           :parameters
                           assoc
                           :gen-profiles [cmi5-id])
          result   (generate-seq input)
          cat-acts (map get-context-category-activities result)]
      (is (= [[{"id" cmi5-version-id}]
              [{"id" cmi5-version-id}  ; has both since cmi5-moveon-id is an
               {"id" cmi5-moveon-id}]] ; 'any' or 'none' value in the profile
             (distinct cat-acts)))))
  (testing "Respects `gen-patterns` param (w/ multiple profiles)"
    (let [input    (update double-profile-input
                           :parameters
                           assoc
                           :gen-patterns [tla-completed-session-id])
          result   (generate-seq input)
          cat-acts (map get-context-category-activities result)]
      (is (= [nil [{"id" tla-version-id}]] ; why are some category activites nil?
             (distinct cat-acts)))))
  (testing "Allows referential use of non-gen profiles"
    (let [input    (-> double-profile-input
                       (update :profiles conj const/referential-profile)
                       (update :parameters
                               assoc
                               :gen-patterns [referential-completed-session-id]))
          result   (generate-seq input)
          cat-acts (map get-context-category-activities result)]
      (is (= [nil [{"id" tla-version-id}]] ; why are some category activites nil?
             (distinct cat-acts)))))
  (testing "Respects agent selection"
    (let [input  (assoc-in const/simple-input [:parameters :max] 3)
          result (generate-seq input
                               ;; specify we only want the given agent(s)
                               :select-agents [bob-mbox])]
      (is (every? #(= bob-mailto (get-actor-mbox %))
                  result))))
  (testing "Only actors in personae are generated"
    (let [result (generate-seq const/simple-input)]
      (is (= #{alice-mailto bob-mailto fred-mailto}
             (set (map get-actor-mbox result))))))
  (testing "Respects pattern weights"
    (let [alignments [{:id     "https://w3id.org/xapi/cmi5#waivedsession"
                       :weight 1.0}
                      {:id     "https://w3id.org/xapi/cmi5#noresultsession"
                       :weight 0.0}
                      {:id     "https://w3id.org/xapi/cmi5#failedsession"
                       :weight 0.0}
                      {:id     "https://w3id.org/xapi/cmi5#completionnosuccesssession"
                       :weight 0.0}
                      {:id     "https://w3id.org/xapi/cmi5#completionmaybefailedsession"
                       :weight 0.0}
                      {:id     "https://w3id.org/xapi/cmi5#passedsession"
                       :weight 0.0}
                      {:id     "https://w3id.org/xapi/cmi5#completionpassedsession"
                       :weight 0.0}]
          input      (update-in const/simple-input
                                [:models 0 :alignments]
                                into
                                alignments)
          result     (generate-seq input :select-agents [bob-mbox])
          verbs      (map #(get-in % ["verb" "id"]) result)]
      (is (every? #{"http://adlnet.gov/expapi/verbs/satisfied"
                    "http://adlnet.gov/expapi/verbs/waived"}
                  verbs))))
  (testing "Respects activity weights"
    (let [alignments [{:id "https://w3id.org/xapi/cmi5/activities/block"
                       :weight 1.0}
                      {:id "https://w3id.org/xapi/cmi5/activities/course"
                       :weight 0.0}
                      {:id "https://w3id.org/xapi/cmi5/activitytype/block"
                       :weight 0.0}
                      {:id "https://w3id.org/xapi/cmi5/activitytype/course"
                       :weight 0.0}]
          input      (update-in const/simple-input
                                [:models 0 :alignments]
                                into
                                alignments)
          result     (generate-seq input :select-agents [bob-mbox])
          act-types  (->> result
                          ;; "satisfied" statements define object activity
                          ;; via rules, hence we need to exclude them
                          (filter (fn [{{verb-id "id"} "verb"}]
                                    (not= verb-id "http://adlnet.gov/expapi/verbs/satisfied")))
                          (map (fn [stmt]
                                 (get-in stmt ["object" "definition" "type"]))))]
      (is (every? #{"https://w3id.org/xapi/cmi5/activities/block"}
                  act-types))))
  (testing "Can apply object override and respect weights"
    (let [input     (assoc const/simple-input :models const/overrides-models)
          result    (generate-seq input
                                  :select-agents [bob-mbox])
          objects   (map get-object result)
          obj-count (count objects)
          obj-freq  (frequencies objects)
          ;; See `datasim.math.random` for math details
          mean-1*   (- 1 (/ 0.3 (* 2 0.7)))
          mean-2*   (- 1 mean-1*)
          mean-1    (* obj-count mean-1*)
          mean-2    (* obj-count mean-2*)
          sd        (math/sqrt (* obj-count mean-1* mean-2*))]
      (is (every? #(or (= override-1 %)
                       (= override-2 %))
                  objects))
      ;; 3 standard devs from mean => 1/370 chance of failure
      ;; (not like it matters here since gen is deterministic)
      (is (< (- mean-1 (* 3 sd))
             (get obj-freq override-1)
             (+ mean-1 (* 3 sd))))
      (is (< (- mean-2 (* 3 sd))
             (get obj-freq override-2)
             (+ mean-2 (* 3 sd))))))
  (testing "Can apply object override and respect weights - only activity"
    (let [input     (-> const/simple-input
                        (assoc :models const/overrides-models)
                        (update-in [:models 0 :objectOverrides 0]
                                   assoc
                                   :weight 1.0)
                        (update-in [:models 0 :objectOverrides 1]
                                   assoc
                                   :weight 0.0))
          result    (generate-seq input
                                  :select-agents [bob-mbox])
          objects   (map get-object result)]
      (is (every? #(= override-1 %) objects))))
  (testing "Can apply multiple personae"
    (let [input  (update const/simple-input
                         :personae-array conj const/tc3-personae)
          result (generate-seq input)
          ids    (map get-actor-mbox result)]
      (is (= #{;; simple personae
               alice-mailto
               bob-mailto
               fred-mailto
               ;; tc3 personae
               "mailto:alice@example.org"
               "mailto:bob@example.org"
               "mailto:fred@example.org"
               "mailto:phil@example.org"
               "mailto:sally@example.org"
               "mailto:steve@example.org"}
             (set ids)))))
  (testing "Respects temporal parameters"
    (let [satisfied "http://adlnet.gov/expapi/verbs/satisfied"
          ms-in-hr  3600000
          start-t   (-> const/simple-input
                        (get-in [:parameters :start])
                        t/instant
                        .toEpochMilli)
          input     (-> const/simple-input
                        (assoc :models const/temporal-models)
                        (assoc-in [:parameters :end] nil))
          result    (generate-seq
                     input
                     :select-agents ["mbox::mailto:bobfake@example.org"])
          result*   (take 100 result)
          ts-maps   (map (fn [{{verb "id"} "verb" ts "timestamp"}]
                           {:verb verb
                            :time (.toEpochMilli (t/instant ts))})
                         result*)
          ts-diffs  (map (fn [{t1 :time} {verb :verb t2 :time}]
                           {:verb verb
                            :diff (- t2 t1)})
                         (cons {:time start-t} ts-maps)
                         ts-maps)]
      (is (every? (fn [{:keys [verb diff]}]
                    (or (and (= verb satisfied)
                             (< diff ms-in-hr))
                        (< ms-in-hr diff)))
                  ts-diffs))))
  (testing "Respects temporal parameters when not applied"))

(deftest generate-async-test
  (testing "Async statement gen produces valid statements"
    (let [agent-chan (-> const/simple-input generate-map-async (get alice-mbox))
          async-res  (a/<!! (a/into [] agent-chan))]
      (is (s/valid? (s/every ::xs/statement) async-res))))
  (testing "Sync vs async statement gen has same result"
    (let [input     const/simple-input
          sync-res  (->> input generate-seq)
          async-res (->> input generate-seq-async (a/into []) a/<!!)]
      (is (= sync-res async-res)))))
