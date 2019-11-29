(ns com.yetanalytics.datasim.xapi.statement.rules
  (:require [clojure.set :as cset]
            [com.yetanalytics.datasim.xapi.statement.helpers :as h]
            [com.yetanalytics.datasim.xapi.statement.location :as loc]
            [com.yetanalytics.datasim.xapi.statement.json-path :as jpath]
            [com.yetanalytics.datasim.xapi.statement.matchable :as matchable]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Presence logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn continue-given-presence?
  "should processing of the current rule continue or not."
  [s]
  (case s
    "excluded" false
    true))

(comment
  (= false (continue-given-presence? "excluded"))
  (= true
     (continue-given-presence? "included")
     (continue-given-presence? "recommended")
     (continue-given-presence? nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Per rule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  ;; possible key within `any-all-none`
  :must-nots
  ;; -- TODO: impl of fn which takes the things it MUST NOT BE and infers what could be
  ;; --- should be very similar to `follow-stmt-path` but based on
  ;; ---- `:stmt-path`, `:within-stmt`, `:within-path`, `:any-all-none` and `:iri-lookup`
  :all-of
  ;; no need to generate, we have our value
  :could-be
  ;; rng selection from possibilities complete, just need to decide if we want to use the value at `could-be`
  ;; - TODO: choice between `within-stmt` and the returned scalar value `any-all-none`
  :generated
  ;; the rule didn't provide anything for us to use, make decision based off of
  ;; - `:stmt-path`, `:within-stmt`, `within-path`, `iri-lookup`
  )

(comment
  ;; `matchable-values` needs to account for a few things
  ;; - `ref-resolve` has 4 possible returns
  ;; -- `:splat` = coll of things found `at-path`, ie. {`:splat` `at-path`}
  ;; -- `:non-iri` = key queried for but nil was returned, ie. {`:non-iri` `nested`}
  ;; -- map not wrapped in `:splat` or `:non-iri`, indication of successful lookup from `iri-map`, profile thing for `nested` returned
  ;; -- thrown error, `nested` was non-nil, non-string, unexpected!
  ;; - `any-all-none` has 4 possible returns
  ;; -- `:could-be` = scalar or vector value which is approp for stmt based on any/all/none
  ;; -- `:must-nots` = vector of values which CAN NOT be at `path`
  ;; -- `:all-of` = vector of items which must be found at `path`
  ;; -- `:generated` = any/all/none didn't provide any useful info, the generated values are returned
  )

(defn derive-stmt-val
  "handles interpretation of matchable values defined by the rule"
  [{:keys [stmt-path within-stmt within-path any-all-none resolved-ref iri-lookup]}]
  ;; FIXME: impl based on comments above
  )

(defn from-rule
  "FIXME: update doc string to reflect updated functionality"
  [{:keys [location
           ;; selector FIXME: not currently supported
           presence
           any
           all
           none] :as rule}
   & {:keys [rng iri-lookup statement] :as passdown}]
  (let [parsed                             (jpath/handle-json-path-str location)
        deconstructed                      (jpath/deconstruct-json-path parsed)
        in-stmt-fn                         (jpath/apply-deconstruced-json-path deconstructed)
        {:keys [path at-path stmt nested]} (in-stmt-fn statement)
        continue?                          (continue-given-presence? presence)]
    ;; `old` = `at-path`
    ;; `new` = result of following let binding
    (if continue?
      (let [matchable (matchable/compound-logic rule rng)
            generated (loc/follow-stmt-path stmt-path :rng rng)
            stmt-val  (matchable-values {:matchable   matchable
                                         :generated   generated
                                         :within-path nested
                                         :iri-lookup  iri-lookup
                                         :stmt-path   stmt-path})]
        {:stmt/path stmt-path
         :stmt/val  stmt-val})
      {:stmt/path stmt-path
       :stmt/val :excluded})))
