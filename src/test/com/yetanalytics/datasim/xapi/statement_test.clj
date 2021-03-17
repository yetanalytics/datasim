(ns com.yetanalytics.datasim.xapi.statement-test
  (:require
   [clojure.test :refer :all]
   [com.yetanalytics.datasim.xapi.statement :refer [generate-statement]]
   [com.yetanalytics.datasim.input :as input]
   [com.yetanalytics.datasim.random :as random]
   [com.yetanalytics.datasim.xapi.profile :as profile]
   [com.yetanalytics.datasim.xapi.activity :as activity]
   [clojure.spec.alpha :as s]
   [xapi-schema.spec :as xs]))

;; FIXME: generate-statement will still generate statements with blatantly contradictory rules,
;; e.g.
;; {:rules [{:location "$.id" :presence "included" :none ["3829c803-1f4c-44ed-8d8f-36e502cadd0f"]}
;;          {:location "$.id" :presence "included" :all ["3829c803-1f4c-44ed-8d8f-36e502cadd0f"]}}}]}
;;
;; FIXME: rule/follows-rule? will raise an exception when trying to apply :presence "excluded"
;; to an already-existing location, e.g.
;; {:rules [... {:location "$.id" :presence "excluded"}]}

;; TODO: a lot more variation in this test, preferably generative
(deftest generate-statement-test
  (testing "given valid args,"
    (let [top-seed 42
          top-rng (random/seed-rng top-seed)
          input (input/from-location :input :json "dev-resources/input/simple.json")
          iri-map (apply profile/profiles->map (:profiles input))
          activities (activity/derive-cosmos input (random/rand-long top-rng))
          valid-args {:input input
                      :iri-map iri-map
                      :activities activities
                      :actor (-> input :personae :member first)
                      :alignment
                      (reduce
                       (fn [acc {:keys [component weight objectOverride]}]
                         (assoc acc component {:weight weight :object-override objectOverride}))
                       {}
                       (get-in input [:alignments :alignment-vector 0 :alignments]))
                      :sim-t 0
                      :seed (random/rand-long top-rng)
                      :template (get iri-map "https://w3id.org/xapi/cmi5#satisfied")
                      :pattern-ancestors
                      [{:id "https://w3id.org/xapi/cmi5#toplevel", :primary true}
                       {:id "https://w3id.org/xapi/cmi5#satisfieds", :primary false}]
                      :registration (random/rand-uuid top-rng)}]
      (testing "produces a valid xapi statement"
        (is (s/valid? ::xs/statement (generate-statement valid-args)))
        (testing "no matter what seed is used"
          (are [seed] (s/valid? ::xs/statement
                                (generate-statement
                                 (assoc valid-args
                                        :seed
                                        seed)))
            -94832 0 39 9600)))
      (testing "is deterministic"
        (is (not
             (apply distinct?
                    (repeatedly 100 #(generate-statement valid-args))))))
      (testing "object override works"
        (let [new-object
              {"objectType" "Activity"
               "id" "https://www.whatever.com/activities#course1"
               "definition" {"name" {"en-US" "Course 1"}
                             "description" {"en-US" "Course Description 1"}
                             "type" "http://adlnet.gov/expapi/activities/course"}}
              valid-args'
              (-> valid-args
                  (assoc-in
                   [:alignment "https://example.org/activity/a" :object-override]
                   new-object)
                  (update-in [:alignment] dissoc "https://example.org/activity/c"))]
          (is (s/valid? ::xs/statement (generate-statement valid-args')))
          (is (= new-object
                 (get (generate-statement valid-args') "object"))))))))
