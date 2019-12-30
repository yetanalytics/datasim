(ns com.yetanalytics.datasim.json.zip-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.json :as json]
            [com.yetanalytics.datasim.json.zip :refer :all]
            [clojure.zip :as z]
            [clojure.test.check :as tc]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer :all]))

(deftest zip-functions-test
  (let [results (stest/check
                 `#{json-zip
                    internal?
                    el-key
                    k-path
                    get-child
                    get-child-in
                    loc-in
                    json-locs
                    json->path-map
                    path-map->json
                    ;; prune ;; TODO: Fix gen so it doesn't get roots
                    })
        {:keys [total
                check-passed]} (stest/summarize-results results)]
    (is (= total check-passed))))

(defspec k-path-is-valid-for-get-in
  (prop/for-all
   [json (s/gen ::json/any)]
   (let [locs (->> json
                   json-zip
                   (iterate z/next)
                   (take-while (complement z/end?))
                   ;; don't look at map entries
                   (remove internal?))]
     (every?
      (fn [loc]
        (let [key-path (k-path loc)]
          (= (z/node loc)
             (get-in json key-path)
             (z/node (loc-in json key-path)))))
      locs))))
