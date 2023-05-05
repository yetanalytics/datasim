(ns com.yetanalytics.datasim.xapi.path-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [xapi-schema.spec :as xs]
            [cheshire.core :as json]
            [com.yetanalytics.datasim.json.zip :as pzip]
            [com.yetanalytics.datasim.xapi.path :as path]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixtures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def long-statement
  (with-open
   [r (io/reader (io/resource "xapi/statements/long.json"))]
    (json/parse-stream r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest spec-map-test
  (is (s/valid? ::path/spec-map path/spec-map)))

(deftest path->spec-test
  (testing "works for lots of paths"
    ;; Explode a statement using a helper from our zipperoo to get a bunch of
    ;; paths and leaf values
    (is (every?
         (fn [[path v]]
           (let [spec (path/path->spec ::xs/statement path long-statement)]
             (and spec
                  (s/valid? spec v))))
         (pzip/json->path-map long-statement))))
  (testing "works for arbitrary and relative paths"
    (is (= ::xs/language-map-text
           (path/path->spec ::xs/activity ["definition" "name" "en-US"]))))
  (testing "can return functions for use as specs"
    (is (= string?
           (path/path->spec ::xs/statement
                            ["object" "definition" "correctResponsesPattern" 0])))))
