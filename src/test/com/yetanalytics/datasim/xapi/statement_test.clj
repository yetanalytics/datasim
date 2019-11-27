(ns com.yetanalytics.datasim.xapi.statement-test
  (:require
   [clojure.test :refer :all]
   [com.yetanalytics.datasim.xapi.statement :refer :all]
   [com.yetanalytics.datasim.input :as input]
   [com.yetanalytics.datasim.random :as random]
   [com.yetanalytics.datasim.xapi.profile :as profile]
   [com.yetanalytics.datasim.xapi.activity :as activity]
   [clojure.spec.alpha :as s]
   [xapi-schema.spec :as xs]))

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
                      :alignment (get-in input [:alignments :alignment-map "mbox::mailto:bob@example.org"])
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
                    (repeatedly 100 #(generate-statement valid-args)))))))))
