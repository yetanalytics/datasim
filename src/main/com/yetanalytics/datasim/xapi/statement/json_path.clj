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
  "ensure root was $ before returning the path into stmt.
   results in a map of `:path-ks`, `:nested` and `within-array-path`.
    - keys will only be included in return map if their value is non-nil"
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
     (try (deconstruct-json-path {:path "$$.result.score.raw"})
          (catch AssertionError e "bad root detected")))
  (= {:path-ks ["context" "contextActivities" "category"]
      :nested "https://w3id.org/xapi/catch/v1"}
     (deconstruct-json-path {:path "$.context.contextActivities.category"
                             :nested "https://w3id.org/xapi/catch/v1"}))
  (= {:path-ks ["attachments"]
      :nested "*"
      :within-array-path ["usageType" "dummyChild"]}
     (deconstruct-json-path {:path   "$.attachments"
                             :nested "*"
                             :after-nested [".usageType.dummyChild"]})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update fn based on deconstructed JSON Path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn apply-deconstruced-json-path
  "use the results of `deconstruct-json-path` to return a fn which
   expects a `stmt` as its only argument. the fn will follow the json path
   into the `stmt` and return a map containing `:path`, `:at-path` and `:stmt`"
  [{:keys [path-ks nested within-array-path]}]
  (when (seq path-ks)
    (fn [stmt]
      (let [base (get-in stmt path-ks)]
        (if (seq nested)
          (let [target-obj       (case nested
                                   "*" base
                                   ;; there should only be one obj with the target id (if any)
                                   (first (filter (fn [m] (= (get m "id") nested)) base)))
                with-placeholder (conj path-ks :placeholder)]
            (if (seq within-array-path)
              (let [at-path (cond (map? target-obj)
                                  ;; get to the value in target object
                                  (get-in target-obj within-array-path)
                                  ;; get to the value in every target obj
                                  (vector? target-obj)
                                  (mapv (fn [each-obj] (get-in each-obj within-array-path)) target-obj)
                                  ;; no target obj, return whatever is there (most likely nil)
                                  :else target-obj)]
                {:path    (into with-placeholder within-array-path)
                 :at-path at-path
                 :stmt    stmt
                 :nested  nested})
              {:path    with-placeholder
               :at-path target-obj
               :stmt    stmt
               :nested  nested}))
          {:path    path-ks
           :at-path base
           :stmt    stmt})))))

(comment
  ;; simple get-in call
  (= {:at-path "target-val"
      :path    ["foo" "bar"]
      :stmt    {"foo" {"bar" "target-val"}}}
     ((apply-deconstruced-json-path {:path-ks ["foo" "bar"]})
      {"foo" {"bar" "target-val"}}))
  ;; get-in call + filter for `target-obj-id`
  (= {:at-path {"id" "target-obj-id"}
      :path    ["foo" "bar" :placeholder]
      :stmt    {"foo" {"bar" [{"id" "target-obj-id"}
                              {"id" "NOT-target-obj-id"}]}}
      :nested  "target-obj-id"}
     ((apply-deconstruced-json-path {:path-ks ["foo" "bar"]
                                     :nested "target-obj-id"})
      {"foo" {"bar" [{"id" "target-obj-id"}
                     {"id" "NOT-target-obj-id"}]}}))
  ;; get-in call + filter for `target-obj-id` + get call
  (= {:at-path "nested in target obj"
      :path    ["foo" "bar" :placeholder "nested-key"]
      :stmt    {"foo" {"bar" [{"id" "target-obj-id"
                               "nested-key" "nested in target obj"}
                              {"id" "NOT-target-obj-id"}]}}
      :nested "target-obj-id"}
     ((apply-deconstruced-json-path {:path-ks ["foo" "bar"]
                                     :nested "target-obj-id"
                                     :within-array-path ["nested-key"]})
      {"foo" {"bar" [{"id" "target-obj-id"
                      "nested-key" "nested in target obj"}
                     {"id" "NOT-target-obj-id"}]}}))
  ;; get-in call + filter for `target-obj-id` + get-in call
  (= {:at-path "even deeper"
      :path    ["foo" "bar" :placeholder "nested-key" "nested"]
      :stmt    {"foo" {"bar" [{"id" "target-obj-id"
                               "nested-key" {"nested" "even deeper"}}
                              {"id" "NOT-target-obj-id"}]}}
      :nested  "target-obj-id"}
     ((apply-deconstruced-json-path {:path-ks ["foo" "bar"]
                                     :nested "target-obj-id"
                                     :within-array-path ["nested-key" "nested"]})
      {"foo" {"bar" [{"id" "target-obj-id"
                      "nested-key" {"nested" "even deeper"}}
                     {"id" "NOT-target-obj-id"}]}}))
  ;; get-in call which returns array, we care about everything in the array
  (= {:at-path [{"id" "target-obj-id"}
                {"id" "NOT-target-obj-id"}]
      :path    ["foo" "bar" :placeholder]
      :stmt    {"foo" {"bar" [{"id" "target-obj-id"}
                              {"id" "NOT-target-obj-id"}]}}
      :nested  "*"}
     ((apply-deconstruced-json-path {:path-ks ["foo" "bar"]
                                     :nested "*"})
      {"foo" {"bar" [{"id" "target-obj-id"}
                     {"id" "NOT-target-obj-id"}]}}))
  ;; get-in call which returns array, we care about something within each array item
  (= {:at-path ["from-first-obj" "from-second-obj"]
      :path    ["foo" "bar" :placeholder "top-lvl-prop"]
      :stmt    {"foo" {"bar" [{"id" "target-obj-id"
                               "top-lvl-prop" "from-first-obj"}
                              {"id" "NOT-target-obj-id"
                               "top-lvl-prop" "from-second-obj"}]}}
      :nested  "*"}
     ((apply-deconstruced-json-path {:path-ks ["foo" "bar"]
                                     :nested "*"
                                     :within-array-path ["top-lvl-prop"]})
      {"foo" {"bar" [{"id" "target-obj-id"
                      "top-lvl-prop" "from-first-obj"}
                     {"id" "NOT-target-obj-id"
                      "top-lvl-prop" "from-second-obj"}]}}))
  ;; get-in call which returns array, we care about something nested within each array item
  (= {:at-path ["from-first-obj" "from-second-obj"]
      :path    ["foo" "bar" :placeholder "top-lvl-prop" "nested-prop"]
      :stmt    {"foo" {"bar" [{"id" "target-obj-id"
                               "top-lvl-prop" {"nested-prop" "from-first-obj"}}
                              {"id" "NOT-target-obj-id"
                               "top-lvl-prop" {"nested-prop" "from-second-obj"}}]}}
      :nested  "*"}
     ((apply-deconstruced-json-path {:path-ks ["foo" "bar"]
                                     :nested "*"
                                     :within-array-path ["top-lvl-prop" "nested-prop"]})
      {"foo" {"bar" [{"id" "target-obj-id"
                      "top-lvl-prop" {"nested-prop" "from-first-obj"}}
                     {"id" "NOT-target-obj-id"
                      "top-lvl-prop" {"nested-prop" "from-second-obj"}}]}})))
