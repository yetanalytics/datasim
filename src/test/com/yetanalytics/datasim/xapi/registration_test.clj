(ns com.yetanalytics.datasim.xapi.registration-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [com.yetanalytics.datasim.xapi.profile :as profile]
            [com.yetanalytics.datasim.xapi.registration :as reg]
            [com.yetanalytics.datasim.test-constants :as const]))

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

(s/fdef gen-reg-seq-instance
  :args (s/cat :seed int?)
  :ret (s/and cmi5-general-pattern?))

(defn gen-reg-seq-instance [seed]
  (let [{:keys [profiles alignments]} const/simple-input
        alignment   (->> (get-in alignments [:alignment-vector 0 :alignments])
                         (reduce (fn [m {:keys [component] :as align}]
                                   (assoc m component align))
                                 {}))
        profile-map (profile/profiles->type-iri-map profiles)]
    (->> (reg/registration-seq-instance profile-map alignment seed)
         (map :template))))

(s/fdef gen-registration-seq
  :args (s/cat :seed int? :limit int?)
  :ret (s/every ::reg/registration-map
                :kind #(instance? clojure.lang.LazySeq %)))

(defn- gen-registration-seq [seed limit]
  (let [{:keys [profiles alignments]} const/simple-input
        alignment   (->> (get-in alignments [:alignment-vector 0 :alignments])
                         (reduce (fn [m {:keys [component] :as align}]
                                   (assoc m component align))
                                 {}))
        profile-map (profile/profiles->type-iri-map profiles)]
    (->> (reg/registration-seq profile-map alignment seed)
         (take limit))))

(deftest registration-seq-test
  (testing "Walk and generate seq for a single pattern"
    (let [{total :total check-passed :check-passed}
          (stest/summarize-results (stest/check `gen-reg-seq-instance))]
      (is (= total check-passed))))
  (testing "Walk and generate seq continuously"
    (let [{total :total check-passed :check-passed}
          (stest/summarize-results (stest/check `gen-registration-seq))]
      (is (= total check-passed)))))