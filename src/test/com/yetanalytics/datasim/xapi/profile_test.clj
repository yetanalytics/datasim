(ns com.yetanalytics.datasim.xapi.profile-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [java-time.api :as t]
            [com.yetanalytics.datasim.xapi.profile :as profile]
            [com.yetanalytics.datasim.test-constants :as const]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ID Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def cmi5-id "https://w3id.org/xapi/cmi5")
(def cmi5-pattern-id "https://w3id.org/xapi/cmi5#toplevel")
(def tla-id "https://w3id.org/xapi/tla")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profile Map Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest profile->type-iri-map-test
  (testing "profiles->type-iri-map function"
    (is (->> const/simple-input
             :profiles
             profile/profiles->type-iri-map
             (s/valid? ::profile/type-iri-map)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Select Primary Pattern Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def combined-iri-map
  (profile/profiles->type-iri-map [const/cmi5-profile const/mom-profile]))

(defn- primary-pattern-ids
  [patterns]
  (keep (fn [{:keys [id primary]}] (when primary id)) patterns))

(deftest select-primary-patterns-test
  (testing "with no params, returns iri map"
    (is (= combined-iri-map
           (profile/select-primary-patterns
            combined-iri-map
            {}))))
  (testing "profile selection, all patterns"
    (is (= combined-iri-map
           (profile/select-primary-patterns
            combined-iri-map
            {:gen-profiles [cmi5-id tla-id]}))))
  (testing "profile selection, selected patterns"
    (is (not= combined-iri-map
              (profile/select-primary-patterns
               combined-iri-map
               {:gen-profiles [cmi5-id tla-id]
                :gen-patterns [cmi5-pattern-id]}))))
  (testing "filters by profile"
    (is (= [cmi5-pattern-id]
           (-> (profile/select-primary-patterns
                combined-iri-map
                {:gen-profiles [cmi5-id]})
               (get "Pattern")
               vals
               primary-pattern-ids))))
  (testing "filters by pattern"
    (is (= [cmi5-pattern-id]
           (-> (profile/select-primary-patterns
                combined-iri-map
                {:gen-patterns [cmi5-pattern-id]})
               (get "Pattern")
               vals
               primary-pattern-ids)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activity Map test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def activity-profile
  {:id "http://example.org/activity-profile"
   :concepts [{:id         "http://example.org/activity-with-type"
               :type       "Activity"
               :definition {:type "http://example.org/activity-type-1"}}
              {:id   "http://example.org/activity-without-type"
               :type "Activity"}
              {:id   "http://example.org/activity-type-1"
               :type "ActivityType"}
              {:id   "http://example.org/activity-type-2"
               :type "ActivityType"}
              {:id   "http://example.org/activity-type-3"
               :type "ActivityType"}]
   :templates [{:id "http://example.org/template"
                :objectActivityType "http://example.org/activity-type-1"}
               {:id "http://example.org/template"
                :rules [{:location "$.object.definition.type"
                         :all ["http://example.org/activity-type-1"]}
                        {:location "$.object.definition"
                         :all [{:type "http://example.org/activity-type-1"}]}]}]})

(def expected-activity-map
  {nil
   {"http://example.org/activity-without-type"
    {"id" "http://example.org/activity-without-type"}}
   "http://example.org/activity-type-1"
   {"http://example.org/activity-with-type"
    {"id" "http://example.org/activity-with-type"
     "definition" {"type" "http://example.org/activity-type-1"}}}
   "http://example.org/activity-type-2"
   {"https://example.org/activity/418707894"
    {"id"         "https://example.org/activity/418707894"
     "definition" {"type" "http://example.org/activity-type-2"}}}
   "http://example.org/activity-type-3"
   {"https://example.org/activity/1432714272"
    {"id" "https://example.org/activity/1432714272"
     "definition" {"type" "http://example.org/activity-type-3"}}}})

(deftest activity-map-test
  (testing "Activity Map creation"
    (is (= expected-activity-map
           (-> [activity-profile]
               (profile/profiles->profile-map {} 100)
               :activity-map)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Template Maps test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def template-profile
  {:id
   "http://example.org/template-profile"
   :templates
   [{:id "http://example.org/template-object-activity"
     :objectActivityType "http://example.org/object-activity-type"
     :rules [{:location "$.object"
              :presence "included"}]}
    {:id "http://example.org/template-object-statement-ref"
     :objectStatementRefTemplate "http://example.org/object-statement-ref-template"
     :rules [{:location "$.object"
              :presence "included"}]}
    {:id "http://example.org/template-object-rules"
     :rules [{:location "$.object.objectType"
              :all ["SubStatement"]}]}]})

(deftest template-maps-test
  (is (= {"http://example.org/template-object-activity"
          {"object" {"definition" {"type" "http://example.org/object-activity-type"}}}
          ;; Not yet implemented
          "http://example.org/template-object-statement-ref" {}
          ;; No statement base from rule
          "http://example.org/template-object-rules" {}}
         (-> [template-profile]
             (profile/profiles->profile-map {} 100)
             :statement-base-map)))
  (is (= {"http://example.org/template-object-activity"
          [{:location [[["object"]]]
            :presence :included
            :path     ["object"]
            :spec     :xapi-schema.spec/activity
            :valueset #{{"id"         "https://example.org/activity/418707894"
                         "definition" {"type" "http://example.org/object-activity-type"}}}}]
          "http://example.org/template-object-statement-ref"
          [{:location [[["object"]]]
            :presence :included
            :path     ["object"]
            :spec     :xapi-schema.spec/statement-ref}]
          "http://example.org/template-object-rules"
          [{:location [[["object"] ["objectType"]]]
            :valueset #{"SubStatement"}
            :all      #{"SubStatement"}
            :path     ["object" "objectType"]
            :spec     :sub-statement/objectType}]}
         (-> [template-profile]
             (profile/profiles->profile-map {} 100)
             :parsed-rules-map
             (update-vals (partial map #(dissoc % :generator)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern Walk Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Add test for max-repeats (for oneOrMore and zeroOrMore)

;; Pattern predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cmi5-iri [verb] (str "https://w3id.org/xapi/cmi5#" verb))

(defn is-cmi5-id? [verb stmt] (= (cmi5-iri verb) (:id stmt)))

(def cmi5-launched? (partial is-cmi5-id? "launched"))
(def cmi5-initialized? (partial is-cmi5-id? "initialized"))
(def cmi5-completed? (partial is-cmi5-id? "completed"))
(def cmi5-passed? (partial is-cmi5-id? "passed"))
(def cmi5-failed? (partial is-cmi5-id? "failed"))
(def cmi5-abandoned? (partial is-cmi5-id? "abandoned"))
(def cmi5-waived? (partial is-cmi5-id? "waived"))
(def cmi5-terminated? (partial is-cmi5-id? "terminated"))
(def cmi5-satisfied? (partial is-cmi5-id? "satisfied"))

(def cmi5-satisfieds? (s/* cmi5-satisfied?))
(def cmi5-maybe-completed? (s/? cmi5-completed?))
(def cmi5-terminated-or-abandoned?
  (s/alt :terminated cmi5-terminated?
         :abandoned  cmi5-abandoned?))
(def cmi5-completed-then-passed?
  (s/cat :completed  cmi5-completed?
         :satisfieds cmi5-satisfieds?
         :passed     cmi5-passed?))
(def cmi5-passed-then-completed?
  (s/cat :passed     cmi5-passed?
         :satisfieds cmi5-satisfieds?
         :completed  cmi5-completed?))
(def cmi5-completed-and-passed?
  (s/alt :completed-then-passed cmi5-completed-then-passed?
         :passed-then-completed cmi5-passed-then-completed?))
(def cmi5-maybe-completed-then-failed?
  (s/cat :maybe-completed cmi5-maybe-completed?
         :satisfieds      cmi5-satisfieds?
         :failed          cmi5-failed?))
(def cmi5-failed-then-maybe-completed?
  (s/cat :failed          cmi5-failed?
         :maybe-completed cmi5-maybe-completed?))
(def cmi5-completed-and-maybe-failed?
  (s/alt :maybe-completed-then-failed cmi5-maybe-completed-then-failed?
         :failed-then-maybe-completed cmi5-failed-then-maybe-completed?))

(def cmi5-waived-session?
  (s/cat :satisfieds cmi5-satisfieds?
         :waived     cmi5-waived?
         :satisfieds cmi5-satisfieds?))
(def cmi5-no-result-session?
  (s/cat :launched    cmi5-launched?
         :initialized cmi5-initialized?
         :terminated-or-abandoned
         cmi5-terminated-or-abandoned?))
(def cmi5-completion-no-success-session?
  (s/cat :launched    cmi5-launched?
         :initialized cmi5-initialized?
         :completed   cmi5-completed?
         :satisfieds  cmi5-satisfieds?
         :terminated-or-abandoned
         cmi5-terminated-or-abandoned?))
(def cmi5-passed-session?
  (s/cat :launched    cmi5-launched?
         :initialized cmi5-initialized?
         :passed      cmi5-passed?
         :satisfieds  cmi5-satisfieds?
         :terminated-or-abandoned
         cmi5-terminated-or-abandoned?))
(def cmi5-completion-passed-session?
  (s/cat :launched    cmi5-launched?
         :initialized cmi5-initialized?
         :completed-and-passed
         cmi5-completed-and-passed?
         :satisfieds  cmi5-satisfieds?
         :terminated-or-abandoned
         cmi5-terminated-or-abandoned?))
(def cmi5-failed-session?
  (s/cat :launched    cmi5-launched?
         :initialized cmi5-initialized?
         :failed      cmi5-failed?
         :terminated-or-abandoned
         cmi5-terminated-or-abandoned?))
(def cmi5-completion-maybe-failed-session?
  (s/cat :launched    cmi5-launched?
         :initialized cmi5-initialized?
         :completed-and-maybe-failed
         cmi5-completed-and-maybe-failed?
         :satisfieds  cmi5-satisfieds?
         :terminated-or-abandoned
         cmi5-terminated-or-abandoned?))

(def cmi5-typical-session?
  (s/alt :completion-maybe-failed-session
         cmi5-completion-maybe-failed-session?
         :completion-passed-session
         cmi5-completion-passed-session?
         :failed-session
         cmi5-failed-session?
         :no-result-session
         cmi5-no-result-session?
         :passed-session
         cmi5-passed-session?
         :completion-no-success-session
         cmi5-completion-no-success-session?
         :waived-session
         cmi5-waived-session?))

;; Original typical-sessions Pattern was zeroOrMore, but changed to exercise
;; oneOrMore.
(def cmi5-typical-sessions?
  (s/+ cmi5-typical-session?))

(def cmi5-general-pattern?
  (s/cat :satisfieds cmi5-satisfieds?
         :typical-sessions cmi5-typical-sessions?))

;; Walk pattern ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def profile-map
  (-> const/simple-input
      :profiles
      (profile/profiles->profile-map {} 100)))

(def start-time
  (t/local-date-time (t/instant) "UTC"))

(s/fdef walk-pattern
  :args (s/cat :seed int?)
  :ret cmi5-general-pattern?)

(defn walk-pattern
  ([seed]
   (walk-pattern {} seed))
  ([alignments seed]
   (->> (profile/walk-profile-patterns profile-map alignments seed start-time)
        (map :template))))

(deftest walk-pattern-test
  (testing "Walk and generate seq for a single pattern"
    (let [{total :total check-passed :check-passed}
          (stest/summarize-results (stest/check `walk-pattern))]
      (is (= total check-passed)))))

(deftest walk-weighted-pattern-test
  (testing "Remove abandoned template from consideration"
    (let [pat-weights {(cmi5-iri "terminated") 1.0
                       (cmi5-iri "abandoned")  0.0}
          pat-align   {(cmi5-iri "terminatedorabandoned")
                       {:weights pat-weights}}
          alignments  {:patterns pat-align}
          results     (walk-pattern alignments 100)]
      (is (s/valid? cmi5-general-pattern? results))
      (is (s/valid? cmi5-terminated? (last results)))
      (is (not (s/valid? cmi5-abandoned? (last results))))))
  (testing "Remove terminated template from consideration"
    (let [pat-weights {(cmi5-iri "terminated") 0.0
                       (cmi5-iri "abandoned")  1.0}
          pat-align   {(cmi5-iri "terminatedorabandoned")
                       {:weights pat-weights}}
          alignments  {:patterns pat-align}
          results     (walk-pattern alignments 100)]
      (is (s/valid? cmi5-general-pattern? results))
      (is (s/valid? cmi5-abandoned? (last results)))
      (is (not (s/valid? cmi5-terminated? (last results))))))
  (testing "Force completed pattern to appear"
    ;; FIXME: This does not guarentee that "completed" appears, since
    ;; the weight for `nil` is still 0.5, not 0.0
    (let [;; Force topmost path
          pat-weights-1 {(cmi5-iri "completionmaybefailedsession") 1.0
                         (cmi5-iri "completionpassedsession")      0.0
                         (cmi5-iri "failedsession")                0.0
                         (cmi5-iri "noresultsession")              0.0
                         (cmi5-iri "passedsession")                0.0
                         (cmi5-iri "completionnosuccesssession")   0.0
                         (cmi5-iri "waivedsession")                0.0}
          pat-align-1   {(cmi5-iri "typicalsession")
                         {:weights pat-weights-1}}
          ;; Force secondary path
          pat-weights-2 {(cmi5-iri "maybecompletedthenfailed") 1.0
                         (cmi5-iri "failedthenmaybecompleted") 0.0}
          pat-align-2   {(cmi5-iri "completedandmaybefailed")
                         {:weights pat-weights-2}}
          ;; Encourage optional
          pat-weights-3 {(cmi5-iri "completed") 1.0}
          pat-align-3   {(cmi5-iri "maybecompleted")
                         {:weights pat-weights-3}}
          alignments    {:patterns (merge pat-align-1 pat-align-2 pat-align-3)}
          results       (walk-pattern alignments 100)]
      (is (s/valid? cmi5-general-pattern? results))
      (is (s/valid? (s/cat :satisfieds cmi5-satisfieds?
                           :typical-sessions (s/+ cmi5-completion-maybe-failed-session?))
                    results))
      (is (some #(s/valid? cmi5-completed? %) results))))
  (testing "Force completed template to not appear"
    (let [;; Force topmost path
          pat-weights-1 {(cmi5-iri "completionmaybefailedsession") 1.0
                         (cmi5-iri "completionpassedsession")      0.0
                         (cmi5-iri "failedsession")                0.0
                         (cmi5-iri "noresultsession")              0.0
                         (cmi5-iri "passedsession")                0.0
                         (cmi5-iri "completionnosuccesssession")   0.0
                         (cmi5-iri "waivedsession")                0.0}
          pat-align-1   {(cmi5-iri "typicalsession")
                         {:weights pat-weights-1}}
          ;; Force secondary path
          pat-weights-2 {(cmi5-iri "maybecompletedthenfailed") 1.0
                         (cmi5-iri "failedthenmaybecompleted") 0.0}
          pat-align-2   {(cmi5-iri "completedandmaybefailed")
                         {:weights pat-weights-2}}
          ;; Force no optional
          pat-weights-3 {(cmi5-iri "completed") 0.0}
          pat-align-3   {(cmi5-iri "maybecompleted")
                         {:weights pat-weights-3}}
          alignments    {:patterns (merge pat-align-1 pat-align-2 pat-align-3)}
          results       (walk-pattern alignments 100)]
      (is (s/valid? cmi5-general-pattern? results))
      (is (s/valid? (s/cat :satisfieds cmi5-satisfieds?
                           :typical-sessions (s/+ cmi5-completion-maybe-failed-session?))
                    results))
      (is (not (some #(s/valid? cmi5-completed? %) results))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profile Map Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def combined-profile-map
  (profile/profiles->profile-map [const/cmi5-profile const/mom-profile] {} 100))

(deftest profile-map-test
  (testing "profile"
    (testing "map"
      (is (s/valid? ::profile/profile-map combined-profile-map)))
    (testing "type iri map"
      (let [{:keys [type-iri-map]} combined-profile-map]
        (is (= 41 (count (get type-iri-map "Verb"))))
        (is (= 11 (count (get type-iri-map "ActivityType"))))
        (is (= 00 (count (get type-iri-map "AttachmentUsageType"))))
        (is (= 01 (count (get type-iri-map "ActivityExtension"))))
        (is (= 16 (count (get type-iri-map "ContextExtension"))))
        (is (= 03 (count (get type-iri-map "ResultExtension"))))
        (is (= 00 (count (get type-iri-map "Activity"))))
        (is (= 60 (count (get type-iri-map "StatementTemplate"))))
        (is (= 34 (count (get type-iri-map "Pattern"))))))
    (testing "activity map"
      (let [{:keys [activity-map]} combined-profile-map]
        (is (= #{;; cmi5 profile - from concepts
                 "https://w3id.org/xapi/cmi5/activities/block"
                 "https://w3id.org/xapi/cmi5/activities/course"
                 ;; cmi5 profile - from template rules
                 "https://w3id.org/xapi/cmi5/activitytype/block"
                 "https://w3id.org/xapi/cmi5/activitytype/course"
                 ;; mom profile - from concepts
                 "https://w3id.org/xapi/tla/activity-types/competency"
                 "https://w3id.org/xapi/tla/activity-types/activity"
                 "https://w3id.org/xapi/tla/activity-types/assessment"
                 "https://w3id.org/xapi/tla/activity-types/content_set"
                 "https://w3id.org/xapi/tla/activity-types/badge"
                 "https://w3id.org/xapi/tla/activity-types/credential"
                 "https://w3id.org/xapi/tla/activity-types/career"
                 "https://w3id.org/xapi/tla/activity-types/career_state"
                 "https://w3id.org/xapi/tla/activity-types/job_duty_gig"
                 ;; mom profile - from template rules
                 "https://w3id.org/xapi/tla/activity-types/rank"}
               (set (keys activity-map))))))
    (testing "verb maps"
      (let [{:keys [verb-map]} combined-profile-map]
        (is (= 41 (count verb-map)))))
    (testing "extension spec map"
      (let [{:keys [extension-spec-map]} combined-profile-map]
        (is (= 01 (count (:activity extension-spec-map))))
        (is (= 16 (count (:context extension-spec-map))))
        (is (= 03 (count (:result extension-spec-map))))
        (is (nil? (get-in extension-spec-map ; no inlineSchema
                          [:activity "https://w3id.org/xapi/tla/extensions/instance"])))
        (is (some? (get-in extension-spec-map ; has inlineSchema
                           [:result "https://w3id.org/xapi/cmi5/result/extensions/progress"])))))
    (testing "template statement base map"
      (let [{:keys [statement-base-map]} combined-profile-map]
        (is (= 60 (count statement-base-map)))))
    (testing "template parsed rules map"
      (let [{:keys [parsed-rules-map]} combined-profile-map]
        (is (= 60 (count parsed-rules-map)))))))
