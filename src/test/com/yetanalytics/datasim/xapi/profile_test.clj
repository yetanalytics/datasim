(ns com.yetanalytics.datasim.xapi.profile-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [com.yetanalytics.datasim.xapi.profile :as profile]
            [com.yetanalytics.datasim.random :as random]
            [com.yetanalytics.datasim.test-constants :as const]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ID Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def cmi5-id "https://w3id.org/xapi/cmi5")
(def cmi5-pattern-id "https://w3id.org/xapi/cmi5#toplevel")
(def tla-id "https://w3id.org/xapi/tla")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest profiles->map-test
  (testing "profiles->map function"
    (is (s/valid? ::profile/iri-map
                  (profile/profiles->map (:profiles const/simple-input))))))

(deftest pattern-zip-test
  (testing "pattern-zip function"
    (is (= '(:zip/branch? :zip/children :zip/make-node ::profile/iri-map)
           (-> const/simple-input
               :profiles
               profile/profiles->map
               profile/pattern-zip
               meta
               keys)))
    (is (s/valid? ::profile/iri-map
                  (-> const/simple-input
                      :profiles
                      profile/profiles->map
                      profile/pattern-zip
                      profile/loc-iri-map
                      (dissoc ::profile/root))))
    (is (= {:id         ::profile/root
            :type       "Pattern"
            :alternates [cmi5-pattern-id]}
           (-> const/simple-input
               :profiles
               profile/profiles->map
               profile/pattern-zip
               profile/loc-iri-map
               ::profile/root)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmi5 profile primary pattern
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Add test for max-repeats (for oneOrMore and zeroOrMore)

(defn is-cmi5-id? [verb stmt] (= (str "https://w3id.org/xapi/cmi5#" verb)
                                 (:id stmt)))

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

(s/fdef gen-single-walk
  :args (s/cat :seed int?)
  :ret cmi5-general-pattern?)

(defn gen-single-walk [seed]
  (let [{:keys [profiles alignments]} const/simple-input
        profile-map (profile/profiles->map profiles)
        seeded-rng  (random/seed-rng seed)]
    (->> (profile/rand-pattern-zip profile-map
                                   alignments
                                   seeded-rng)
         profile/walk-once
         (keep (fn [loc]
                 (let [{obj-type :type :as loc-obj} (profile/loc-object loc)]
                   (when (= "StatementTemplate" obj-type) loc-obj)))))))

(deftest zip-walk-test
  (testing "rand-pattern-zip followed by walk-once"
    (let [{total :total check-passed :check-passed}
          (stest/summarize-results (stest/check `gen-single-walk))]
      (is (= total check-passed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmi5 + tla profiles tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def combined-iri-map
  (profile/profiles->map [const/cmi5-profile const/mom-profile]))

(defn- primary-pattern-ids
  [patterns]
  (keep (fn [{:keys [id primary]}] (when primary id)) patterns))

(deftest select-primary-patterns-test
  (testing "with no params, returns iri map"
    (is (= combined-iri-map
           (profile/select-primary-patterns
            combined-iri-map
            {}))))
  (testing "profile selection implies patterns"
    (is (= combined-iri-map
           (profile/select-primary-patterns
            combined-iri-map
            {:gen-profiles [cmi5-id tla-id]})))
    (testing "unless also specified"
      (is (not= combined-iri-map
                (profile/select-primary-patterns
                 combined-iri-map
                 {:gen-profiles [cmi5-id tla-id]
                  :gen-patterns [cmi5-pattern-id]})))))
  (testing "filters by profile"
    (is (= [cmi5-pattern-id]
           (-> (profile/select-primary-patterns
                combined-iri-map
                {:gen-profiles [cmi5-id]})
               vals
               primary-pattern-ids))))
  (testing "filters by pattern"
    (is (= [cmi5-pattern-id]
           (-> (profile/select-primary-patterns
                combined-iri-map
                {:gen-patterns [cmi5-pattern-id]})
               vals
               primary-pattern-ids)))))
