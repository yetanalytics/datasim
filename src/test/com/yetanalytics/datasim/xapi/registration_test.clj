(ns com.yetanalytics.datasim.xapi.registration-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [com.yetanalytics.datasim.xapi.profile :as profile]
            [com.yetanalytics.datasim.xapi.registration :as reg]
            [com.yetanalytics.datasim.test-constants :as const]))

(s/fdef gen-registration-seq
  :args (s/cat :seed int? :limit int?)
  :ret (s/every ::reg/registration-map
                :kind #(instance? clojure.lang.LazySeq %)))

(defn- gen-registration-seq [seed limit]
  (let [{:keys [profiles models]} const/simple-input
        alignments (->> (get-in models [0 :alignments])
                        (reduce (fn [m {:keys [id weight]}]
                                  (assoc m id weight))
                                {})
                        (assoc {} :weights))
        profile-map (profile/profiles->profile-map profiles {} seed)]
    (->> (reg/registration-seq profile-map alignments seed)
         (take limit))))

(deftest registration-seq-test
  (testing "Walk and generate seq continuously"
    (let [{total :total check-passed :check-passed}
          (stest/summarize-results (stest/check `gen-registration-seq))]
      (is (= total check-passed)))))
