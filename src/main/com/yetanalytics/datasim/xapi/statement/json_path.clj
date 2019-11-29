(ns com.yetanalytics.datasim.xapi.statement.json-path
  (:require [clojure.string :as string]
            [com.yetanalytics.datasim.xapi.statement.helpers :as h]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse array within JSON Path string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn strip-single-quotes
  "when `s` contains a single quote, remove all instances of single quotes"
  [s]
  (if (string/includes? s "'")
    (string/replace s "'" "")
    s))

(defn handle-json-path-str
  "extract items found within json-path string array, ie. ['some-iri']
   - `path` everything up to the array
   - `nested` everything inbetween [' and ']
   - `after-nested` everything after the closing bracket of `nested`"
  [path-str]
  (if (string/includes? path-str "[")
    (let [[path v*]          (string/split path-str #"\[")
          ;; FIXME: not exhaustive parsing of JSON Path
          no-quotes          (strip-single-quotes v*)
          [v :as around-vec] (string/split no-quotes #"\]")
          after-vec          (h/butfirst around-vec)]
      (if (seq after-vec)
        {:path path :nested v :after-nested after-vec}
        {:path path :nested v}))
    {:path path-str}))

(comment
  (= (handle-json-path-str "$.context.contextActivities.category['https://w3id.org/xapi/catch/v1']")
     {:path "$.context.contextActivities.category"
      :nested "https://w3id.org/xapi/catch/v1"})
  (= (handle-json-path-str "$.attachments[*].usageType")
     {:path   "$.attachments"
      :nested "*"
      :after-nested [".usageType"]})
  (= (handle-json-path-str "$.attachments['*'].usageType.dummyChild")
     {:path   "$.attachments"
      :nested "*"
      :after-nested [".usageType.dummyChild"]})
  (= (handle-json-path-str "$.context.contextActivities.category['*']")
     {:path "$.context.contextActivities.category"
      :nested "*"})
  (= (handle-json-path-str "$.result.score.raw") {:path "$.result.score.raw"}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Split on branch/node to create key seq for nav into stmt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn deconstruct-json-path
  "ensure root was $ before returning the path into stmt"
  [path & {:keys [remainder]}]
  (let [[root & components :as deconstructed] (string/split path #"\.")
        within-array-path (when (seq remainder) (string/split remainder #"\."))]
    (assert (= root "$") "JSONPath string did not start with root!")
    ;; FIXME: figure out simple/clean way to talk about within array paths!
    (h/butfirst deconstructed)))

(comment
  (= ["result" "score" "raw"]
     (deconstruct-json-path "$.result.score.raw"))
  (= ["context" "contextActivities" "category"]
     (deconstruct-json-path "$.context.contextActivities.category"))
  (= "bad root detected"
     (try (deconstruct-json-path "$$.result.score.raw")
          (catch AssertionError e "bad root detected"))))
