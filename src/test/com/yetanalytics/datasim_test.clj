(ns com.yetanalytics.datasim-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.core.async :as a]
            [clojure.spec.alpha :as s]
            [xapi-schema.spec   :as xs]
            [com.yetanalytics.datasim.test-constants :as const]
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

(def override-activity-1
  {"objectType" "Activity"
   "id"         "https://www.whatever.com/activities#course2"
   "definition"
   {"name"        {"en-US" "Course 2"}
    "description" {"en-US" "Course Description 2"}
    "type"        "http://adlnet.gov/expapi/activities/course"}})

(def override-activity-2
  {"objectType" "Agent"
   "name"       "Owen Overrider"
   "mbox"       "mailto:owoverrider@example.com"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deftest generate-map-test
  (testing "given valid input, returns a valid skeleton"
    (is (s/valid?
         :com.yetanalytics.datasim.sim/skeleton
         (generate-map const/simple-input)))))

(deftest disjoint-rng-test
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
        fred-mbox))))

(deftest xapi-test
  (testing "sim returns valid xapi statements"
    (let [skeleton (generate-map (assoc-in const/simple-input
                                             [:parameters :end]
                                             nil))]
      (are [actor-id] (s/valid? (s/every ::xs/statement)
                                (get skeleton actor-id))
        alice-mbox
        bob-mbox
        fred-mbox))))

(deftest stack-test
  (testing "that we can iterate for a long time w/o a stack overflow"
    (is (s/valid? ::xs/statement
                  (-> const/simple-input
                      (assoc-in [:parameters :end] nil)
                      generate-map
                      (get bob-mbox)
                      (nth 10000))))))

(deftest generate-seq-test
  (testing "returns statements"
    (is (s/valid? (s/every ::xs/statement) (generate-seq const/simple-input))))
  (testing "respects max param"
    (let [ret (generate-seq (assoc-in const/simple-input [:parameters :max] 3))]
      (is (s/valid? (s/every ::xs/statement) ret))
      (is (= 3 (count ret)))))
  (testing "respects from param"
    (let [[s0 s1 & _] (generate-seq const/simple-input)
          [s1' & _]   (generate-seq (assoc-in const/simple-input
                                         [:parameters :from]
                                         (get-timestamp s0)))]
      (is (not= s0 s1'))
      (is (= s1 s1'))))
  (testing "multiple profiles"
    (let [double-input (update const/simple-input :profiles conj const/mom-profile)]
      (testing "respects gen-profiles param"
        (is (= [[{"id" cmi5-version-id}]
                [{"id" cmi5-version-id}  ; has both since cmi5-moveon-id is an
                 {"id" cmi5-moveon-id}]] ; 'any' or 'none' value in the profile
               (-> double-input
                   (update :parameters
                           assoc
                           :gen-profiles [cmi5-id])
                   generate-seq
                   (->> (map get-context-category-activities))
                   distinct))))
      (testing "respects gen-patterns param"
        (is (= [nil [{"id" tla-version-id}]] ; why are some category activites nil?
               (-> double-input
                   (update :parameters
                           assoc
                           :gen-patterns [tla-completed-session-id])
                   generate-seq
                   (->> (map get-context-category-activities))
                   distinct))))
      (testing "allows referential use of non-gen profiles"
        (is (= [nil [{"id" tla-version-id}]] ; why are some category activites nil?
               (-> double-input
                   (update :profiles conj const/referential-profile)
                   (update :parameters
                           assoc
                           :gen-patterns [referential-completed-session-id])
                   generate-seq
                   (->> (map get-context-category-activities))
                   distinct))))))
  (testing "respects agent selection"
    (let [ret (generate-seq (assoc-in const/simple-input [:parameters :max] 3)
                       ;; specify we only want the given agent(s)
                       :select-agents [bob-mbox])]
      (is (every?
           #(= bob-mailto (get-actor-mbox %))
           ret))))
  (testing "only actors in personae are generated"
    (is (= #{alice-mailto bob-mailto fred-mailto}
           (->> const/simple-input generate-seq (map get-actor-mbox) set))))
  (testing "can apply object override"
    (let [ret (generate-seq (assoc const/simple-input
                              :alignments const/override-alignments)
                       :select-agents [bob-mbox])]
      (is (every? #(or (= override-activity-1 %)
                       (= override-activity-2 %))
                  (map get-object ret)))))
  (testing "can apply multiple personae"
    (let [ret (generate-seq (update const/simple-input
                               :personae-array conj const/tc3-personae))
          ids (map get-actor-mbox ret)]
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
             (set ids))))))

(deftest sim-chan-test
  (testing "async statement gen produces valid statements"
    (let [agent-chan (-> const/simple-input generate-map-async (get alice-mbox))
          async-res  (a/<!! (a/into [] agent-chan))]
      (is (s/valid? (s/every ::xs/statement) async-res))))
  (testing "sync vs async statement gen has same result"
    (let [input     const/simple-input
          sync-res  (->> input generate-seq)
          async-res (->> input generate-seq-async (a/into []) a/<!!)]
      (is (= sync-res async-res)))))
