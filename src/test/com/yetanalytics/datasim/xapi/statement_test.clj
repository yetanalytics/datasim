(ns com.yetanalytics.datasim.xapi.statement-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.spec.alpha :as s]
            [clojure.walk :as w]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.xapi.statement :refer [generate-statement]]
            [com.yetanalytics.datasim.random :as random]
            [com.yetanalytics.datasim.xapi.profile :as profile]
            [com.yetanalytics.datasim.xapi.activity :as activity]
            [com.yetanalytics.datasim.test-constants :as const]))

;; FIXME: generate-statement will still generate statements with blatantly contradictory rules,
;; e.g.
;; {:rules [{:location "$.id" :presence "included" :none ["3829c803-1f4c-44ed-8d8f-36e502cadd0f"]}
;;          {:location "$.id" :presence "included" :all ["3829c803-1f4c-44ed-8d8f-36e502cadd0f"]}}}]}
;;
;; FIXME: rule/follows-rule? will raise an exception when trying to apply :presence "excluded"
;; to an already-existing location, e.g.
;; {:rules [... {:location "$.id" :presence "excluded"}]}

;; TODO: a lot more variation in this test, preferably generative

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def top-seed 42)

(def top-rng
  (random/seed-rng top-seed))

(def profile-iri-map
  (-> const/simple-input :profiles profile/profiles->map))

(def activities
  (-> const/simple-input (activity/derive-cosmos (random/rand-long top-rng))))

(def actor
  (-> const/simple-input :personae-array first :member first (dissoc :role)))

(def alignments*
  (-> const/simple-input (get-in [:alignments :alignment-vector 0 :alignments])))

(def object-override
  {:objectType "Activity"
   :id         "https://www.whatever.com/activities#course1"
   :definition {:name        {:en-US "Course 1"}
                :description {:en-US "Course Description 1"}
                :type        "http://adlnet.gov/expapi/activities/course"}})

(def alignments
  (reduce
   (fn [acc {:keys [component weight objectOverride]}]
     (assoc acc component {:weight          weight
                           :object-override objectOverride}))
   {}
   alignments*))

(def template
  (get profile-iri-map "https://w3id.org/xapi/cmi5#satisfied"))

(def pattern-ancestors
  [{:id      "https://w3id.org/xapi/cmi5#toplevel"
    :primary true}
   {:id      "https://w3id.org/xapi/cmi5#satisfieds"
    :primary false}])

(def valid-args
  {:input             const/simple-input
   :iri-map           profile-iri-map
   :activities        activities
   :actor             actor
   :alignment         alignments
   :sim-t             0
   :seed              (random/rand-long top-rng)
   :template          template
   :pattern-ancestors pattern-ancestors
   :registration      (random/rand-uuid top-rng)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest generate-statement-test
  (testing "given valid args,"
    (testing "produces a valid xapi statement"
      (is (s/valid? ::xs/statement (generate-statement valid-args)))
      (testing "no matter what seed is used"
        (are [seed] (->> (assoc valid-args :seed seed)
                         generate-statement
                         (s/valid? ::xs/statement))
          -94832 0 39 9600)))
    (testing "is deterministic"
      (is (->> #(generate-statement valid-args)
               (repeatedly 100)
               (apply distinct?)
               not)))
    (testing "object override works"
      (let [valid-args'
            (-> valid-args
                (assoc-in
                 [:alignment "https://example.org/activity/a" :object-override]
                 object-override)
                (update-in [:alignment] dissoc "https://example.org/activity/c"))]
        (is (s/valid? ::xs/statement (generate-statement valid-args')))
        (is (= object-override
               (-> (generate-statement valid-args')
                   (get "object")
                   w/keywordize-keys)))))))
