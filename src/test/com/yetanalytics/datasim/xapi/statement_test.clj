(ns com.yetanalytics.datasim.xapi.statement-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.spec.alpha :as s]
            [clojure.walk :as w]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.xapi.statement :as stmt :refer [generate-statement]]
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

(defn- heal-statement
  [statement]
  (cond-> statement
    (nil? (get-in statement ["id"]))
    (assoc "id" "00000000-4000-8000-0000-000000000000")
    (nil? (get-in statement ["actor"]))
    (assoc "actor" {})
    (nil? (get-in statement ["verb"]))
    (assoc "verb" {})
    (nil? (get-in statement ["object"]))
    (assoc "object" {})
    (nil? (get-in statement ["timestamp"]))
    (assoc "timestamp" "2023-03-03T10:10:10.000Z")
    ))

(comment
  (require '[com.yetanalytics.pathetic :as pathetic])

  (reduce (fn [stmt {:keys [location presence any all none]}]
            (cond
             (= "excluded" presence)
             (pathetic/excise* stmt location)
             :else
             (pathetic/apply-value* stmt location (or (first all)
                                                      (first any)))))
          {}
          (stmt/parse-template profile-iri-map template))
  
  (s/explain-data ::xs/statement
                  {"verb" {"id" "http://adlnet.gov/expapi/verbs/satisfied"},
                   "context" {"contextActivities" {"category" [{"id" nil}]}},
                   "object" {"definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}})

  )

(comment
  (require '[com.yetanalytics.datasim.xapi.spec :as datasim-spec])
  
  (datasim-spec/fill-object
   (assoc valid-args :rng (random/seed-rng 100))
   :statement
   {"verb" {"id" "https://w3id.org/xapi/adl/verbs/satisfied"},
    "context" {"contextActivities" {"category" [{"id" nil}]}},
    "object" {"definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}})
  
  (merge {"name" "Bob Fakename", "mbox" "mailto:bobfake@example.org"}
         {"name" "Bob Fakename"})
  
  (datasim-spec/fill-object
   (-> valid-args
       (assoc :rng (random/seed-rng 100))
       (update :iri-map assoc "http://attachment-usage-type.com" {:id "http://attachment-usage-type.com"
                                                                  :type "AttachmentUsageType"
                                                                  :prefLabel {"en-us" "foo"}
                                                                  :definition {"en-us" "bar"}})
       (update :alignments assoc "http://attachment-usage-type.com" {:weight 0.0}))
   :statement
   {"actor" {"name" "Bob Fakename"}
    "verb" {"id" "https://w3id.org/xapi/adl/verbs/satisfied"},
    "context" {"contextActivities" {"category" [{"id" nil}]}},
    "object" {"definition" {"type" "https://w3id.org/xapi/cmi5/activitytype/course"}}
    "attachments" {}}))
