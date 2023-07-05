(ns com.yetanalytics.datasim.xapi.profile-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
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
