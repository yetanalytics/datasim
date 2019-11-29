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
  ;; `derive-stmt-val` needs to account for a few things
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
      (let [;; what do we know based on `nested`?
            ref-resolve  (when-some [_ nested]
                           ;; there is some non-nil reference to either
                           ;; - all objs
                           ;; OR
                           ;; - something within all objs
                           ;; OR
                           ;; - target obj
                           (if (string? nested)
                             ;; should be a string, if not, throw as unexpected state
                             (case nested
                               ;; return the vector of things found `at-path`
                               "*" {:splat at-path}
                               ;; assume IRI but return `nested` if assumption is wrong
                               ;; - `nested` will be found at the key `:non-iri` if lookup returned nil
                               (h/iri-lookup-attempt
                                {:maybe-iri nested
                                 :iri-map   iri-lookup}))
                             (throw (ex-info "the lookup was not a string! unexpected state!"
                                             {:nested  nested
                                              :path    path
                                              :at-path at-path}))))
            ;; what do we know based on rule's any/all/none
            matchable    (when (or (some? any)
                                   (some? all)
                                   (some? none))
                           ;; we have something from the rule, use it
                           ;; OR
                           ;; we don't have something from the rule, return nil
                           (matchable/compound-logic rule rng))
            any-all-none (cond (fn? matchable)
                               ;; we need to filter generated values based on `none`
                               (let [generated (loc/follow-stmt-path path :rng rng)]
                                 (if (some? generated)
                                   ;; we were able to infer enough to come up with at least one possible value
                                   ;; - filter `generated` based on `none`
                                   {:could-be (matchable generated)}
                                   ;; if generation doesn't provide anything, the must-not values are returned
                                   ;; - the must not values will then used to further improve inference
                                   {:must-nots (matchable generated)}))
                               (and (vector? matchable)
                                    (cset/subset? (set matchable) (set all)))
                               ;; the `all` collection or a setubet of `all` was returned by `compound-logic`
                               ;; - no need to generate, we have our value
                               {:all-of matchable}
                               (some? matchable)
                               ;; scalar or vector value for potential inclusion within `stmt` at `path`
                               ;; - no need to generate, we have our value, just determine if it will be used to update `stmt`
                               {:could-be matchable}
                               :else
                               ;; we don't have something form the rule, return generated
                               {:generated (loc/follow-stmt-path path :rng rng)})
            stmt-val     (derive-stmt-val {:stmt-path     path
                                           :within-stmt   at-path
                                           :within-path   nested
                                           :any-all-none  any-all-none
                                           :resolved-ref  ref-resolve
                                           :iri-lookup    iri-lookup})]
        {:stmt/path path
         :stmt/val  stmt-val
         :stmt/this stmt})
      {:stmt/path path
       :stmt/val  :excluded
       :stmt/this stmt})))
