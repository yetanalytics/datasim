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

(defn split-on-dot
  "split `s` at `.` when present in `s`"
  [s]
  (if (string/includes? s ".")
    (string/split s #"\.")
    s))

(defn deconstruct-json-path
  "ensure root was $ before returning the path into stmt"
  [{:keys [path nested after-nested]}]
  (let [[root & components :as deconstructed] (split-on-dot path)
        maybe-nested (when (string? nested) {:nested nested})
        maybe-more   (when (seq after-nested)
                       (reduce into []
                               (map (fn [s]
                                      (let [[maybe-empty :as split] (split-on-dot s)]
                                        (if (and (vector? split) (empty? maybe-empty))
                                          (h/butfirst split)
                                          split)))
                                    after-nested)))]
    (assert (= root "$") "JSONPath string did not start with root!")
    (as-> {:path-ks (h/butfirst deconstructed)} k-seqs
      (if (map? maybe-nested) (merge k-seqs maybe-nested) k-seqs)
      (if (seq maybe-more) (assoc k-seqs :within-array-path maybe-more) k-seqs))))

(comment
  (= {:path-ks ["result" "score" "raw"]}
     (deconstruct-json-path {:path "$.result.score.raw"}))
  (= {:path-ks ["context" "contextActivities" "category"]}
     (deconstruct-json-path {:path "$.context.contextActivities.category"}))
  (= "bad root detected"
     (try (deconstruct-json-path "$$.result.score.raw")
          (catch AssertionError e "bad root detected"))))
