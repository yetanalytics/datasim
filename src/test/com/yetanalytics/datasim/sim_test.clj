(ns com.yetanalytics.datasim.sim-test
  (:require [clojure.test :refer [deftest testing is are]]
            [com.yetanalytics.datasim.sim :refer [build-skeleton sim-seq]]
            [com.yetanalytics.datasim.input :as input]
            [clojure.spec.alpha :as s]
            [xapi-schema.spec :as xs]))

(def valid-input
  (input/from-location :input :json "dev-resources/input/simple.json"))

(deftest build-skeleton-test
  (testing "given valid input, returns a valid skeleton"
    (is (s/valid?
         :com.yetanalytics.datasim.sim/skeleton
         (build-skeleton valid-input)))))

(deftest disjoint-rng-test
  (testing "Make sure RNGs aren't shared across threads."
    (let [skeleton (build-skeleton (assoc-in valid-input
                                             [:parameters :end]
                                             nil))]
      (are [actor-id] (let [statement-seq (get skeleton
                                               actor-id)
                            f1 (future (nth statement-seq 1000))
                            f2 (future (nth statement-seq 1000))]
                        (= @f1 @f2))
        "mbox::mailto:alicefaux@example.org"
        "mbox::mailto:bobfake@example.org"
        "mbox::mailto:frederstaz@example.org"))))

(deftest xapi-test
  (testing "sim returns valid xapi statements"
    (let [skeleton (build-skeleton (assoc-in valid-input
                                             [:parameters :end]
                                             nil))]
      (are [actor-id] (s/valid? (s/every ::xs/statement)
                                (get skeleton actor-id))
        "mbox::mailto:alicefaux@example.org"
        "mbox::mailto:bobfake@example.org"
        "mbox::mailto:frederstaz@example.org"))))

(deftest stack-test
  (testing "that we can iterate for a long time w/o a stack overflow"
    (is (s/valid? ::xs/statement
                  (-> valid-input
                      (assoc-in [:parameters :end] nil)
                      build-skeleton
                      (get "mbox::mailto:bobfake@example.org")
                      (nth 10000))))))

(deftest sim-seq-test
  (testing "returns statements"
    (is (s/valid? (s/every ::xs/statement) (sim-seq valid-input))))
  (testing "respects max param"
    (let [ret (sim-seq (assoc-in valid-input [:parameters :max] 3))]
      (is (s/valid? (s/every ::xs/statement) ret))
      (is (= 3 (count ret)))))
  (testing "respects from param"
    (let [[s0 s1 & _] (sim-seq valid-input)
          [s1' & _]   (sim-seq (assoc-in valid-input
                                         [:parameters :from]
                                         (get s0 "timestamp")))]
      (is (not= s0 s1'))
      (is (= s1 s1'))))
  (testing "multiple profiles"
    (let [double-input (update valid-input
                               :profiles
                               conj
                               (input/from-location
                                :profile
                                :json
                                "dev-resources/profiles/tla/mom.jsonld"))]
      (testing "respects gen-profiles param"
        (is (= [[{"id" "https://w3id.org/xapi/cmi5/v1.0"}]
                [{"id" "https://w3id.org/xapi/cmi5/context/categories/moveon"}]]
               (-> double-input
                   (update :parameters
                           assoc
                           :gen-profiles
                           ["https://w3id.org/xapi/cmi5"])
                   sim-seq
                   (->> (map #(get-in % ["context" "contextActivities" "category"])))
                   distinct))))
      (testing "respects gen-patterns param"
        (is (= [nil [{"id" "https://w3id.org/xapi/tla/v0.13"}]]
               (-> double-input
                   (update :parameters
                           assoc
                           :gen-patterns
                           ["https://w3id.org/xapi/tla#completed_session"])
                   sim-seq
                   (->> (map #(get-in % ["context" "contextActivities" "category"])))
                   distinct))))
      (testing "allows referential use of non-gen profiles"
        (is (= [nil [{"id" "https://w3id.org/xapi/tla/v0.13"}]]
               (-> double-input
                   (update :profiles
                           conj
                           (input/from-location
                            :profile
                            :json
                            "dev-resources/profiles/referential.jsonld"))
                   (update :parameters
                           assoc
                           :gen-patterns
                           ["https://xapinet.org/xapi/yet/referential#completed_session"])
                   sim-seq
                   (->> (map #(get-in % ["context" "contextActivities" "category"])))
                   distinct))))))
  (testing "respects agent selection"
    (let [ret (sim-seq (assoc-in valid-input [:parameters :max] 3)
                       ;; specify we only want the given agent(s)
                       :select-agents ["mbox::mailto:bobfake@example.org"])]
      (is (every?
           #(= "mailto:bobfake@example.org"
               (get-in % ["actor" "mbox"]))
           ret))))
  (testing "only actors in personae are generated"
    (is (= #{"mailto:alicefaux@example.org"
             "mailto:bobfake@example.org"
             "mailto:frederstaz@example.org"}
           (->> valid-input sim-seq (map #(get-in % ["actor" "mbox"])) set))))
  (testing "can apply object override"
    (let [align (input/from-location :alignments :json "dev-resources/alignments/simple_with_overrides.json")
          ret   (sim-seq (assoc valid-input :alignments align)
                         :select-agents ["mbox::mailto:bobfake@example.org"])]
      (is (every?
           #(or (= % {"objectType" "Activity"
                      "id"         "https://www.whatever.com/activities#course2"
                      "definition"
                      {"name"        {"en-US" "Course 2"}
                       "description" {"en-US" "Course Description 2"}
                       "type"        "http://adlnet.gov/expapi/activities/course"}})
                (= % {"objectType" "Agent"
                      "name"       "Owen Overrider"
                      "mbox"       "mailto:owoverrider@example.com"}))
           (map #(get % "object") ret)))))
  (testing "can apply multiple personae"
    (let [per (input/from-location :personae :json "dev-resources/personae/tccc_dev.json")
          ret (sim-seq (update valid-input :personae-array conj per))
          ids (map #(get-in % ["actor" "mbox"]) ret)]
      (is (= #{;; simple personae
               "mailto:alicefaux@example.org"
               "mailto:bobfake@example.org"
               "mailto:frederstaz@example.org"
               ;; tc3 personae
               "mailto:alice@example.org"
               "mailto:bob@example.org"
               "mailto:fred@example.org"
               "mailto:phil@example.org"
               "mailto:sally@example.org"
               "mailto:steve@example.org"}
             (set ids))))))
