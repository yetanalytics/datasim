(ns com.yetanalytics.datasim.xapi.statement.json-path
  (:require [clojure.string :as string]
            [com.yetanalytics.datasim.xapi.statement.helpers :as h]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse array within JSON Path string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-json-path-str
  "extract items found within json-path string array, ie. ['some-iri']
   - `path` everything up to the array
   - `nested` everything inbetween [' and ']"
  [path-str]
  (if (string/includes? path-str "['")
    (let [[path v*] (string/split path-str #"\['")
          [v _] (string/split v* #"\']")]
      {:path path :nested v})
    {:path path-str}))

(comment
  (= (handle-json-path-str "$.context.contextActivities.category['https://w3id.org/xapi/catch/v1']")
     {:path "$.context.contextActivities.category"
      :nested "https://w3id.org/xapi/catch/v1"})
  (= (handle-json-path-str "$.context.contextActivities.category['*']")
     {:path "$.context.contextActivities.category"
      :nested "*"})
  (= (handle-json-path-str "$.result.score.raw") {:path "$.result.score.raw"}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Split on branch/node to create key seq for nav into stmt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn deconstruct-json-path
  "ensure root was $ before returning the path into stmt"
  [path]
  (let [[root & components :as deconstructed] (string/split path #"\.")]
    (assert (= root "$") "JSONPath string did not start with root!")
    (h/butfirst deconstructed)))

(comment
  (= ["result" "score" "raw"]
     (deconstruct-json-path "$.result.score.raw"))
  (= ["context" "contextActivities" "category"]
     (deconstruct-json-path "$.context.contextActivities.category"))
  (= "bad root detected"
     (try (deconstruct-json-path "$$.result.score.raw")
          (catch AssertionError e "bad root detected"))))
