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
  ;; FIXME: look for start of array, then check and strip quote if there, etc.
  ;; - currently incorrectly assumes that * is wrapped in single quotes
  ;; -- I have seen it both ways, not sure which one is correct
  (if (string/includes? path-str "['")
    (let [[path v*] (string/split path-str #"\['")
          [v _ :as around-vec] (string/split v* #"\']")
          after-vec            (h/butfirst around-vec)]
      (if (seq after-vec)
        {:path path :nested v :rest after-vec}
        {:path path :nested v}))
    {:path path-str}))

;; TODO: mini dsl based on :path, :nested, :rest
;; - :nested = * ~ mapv
;;           = 'some-iri' ~ filterv (fn [{:keys [id]}] (= id some-iri))
;; - :rest = navigation into object when :nested = some-iri
;;         = navigate into every object

(comment
  (= (handle-json-path-str "$.context.contextActivities.category['https://w3id.org/xapi/catch/v1']")
     {:path "$.context.contextActivities.category"
      :nested "https://w3id.org/xapi/catch/v1"})
  (= (handle-json-path-str "$.attachments['*'].usageType")
     {:path   "$.attachments"
      :nested "*"
      :rest [".usageType"]})
  (= (handle-json-path-str "$.attachments['*'].usageType.dummyChild")
     {:path   "$.attachments"
      :nested "*"
      :rest [".usageType.dummyChild"]})
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
