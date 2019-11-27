(ns com.yetanalytics.datasim.xapi.statement.rules
  (:require [com.yetanalytics.datasim.xapi.statement.helpers :as h]
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
;; Priority determination
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-matchables
  "helper fn which returns the stmt value to use for the current rule
   - `matchable` = any/all/none
   - `generated` = based on `location`
   - `within-path` = item within `location` array, ie. ...['within-path']
   - `iri-lookup` = iri-map from input"
  [{:keys [matchable generated within-path iri-lookup]}]
  (let [in-path-fn (fn [fallback]
                     (if-some [_ within-path]
                       ;; non-nil
                       (if (string? within-path)
                         ;; string
                         (case within-path
                           "*" fallback
                           ""  fallback
                           ;; assume IRI but return `within-path` + `fallback` if assumption is wrong
                           (h/iri-lookup-attempt
                            {:maybe-iri within-path
                             :iri-map   iri-lookup
                             :fallback  fallback}))
                         ;; some non-string, unexpected, return fallback
                         fallback)
                       ;; nil, return fallback
                       fallback))]
    ;; check `matchable` first to see if profile does the work for us
    (if-some [any-all-none (try (not-empty matchable)
                                (catch Exception e matchable))]
      ;; can be a fn or data
      (let [matches (if (fn? any-all-none)
                      ;; (->> none-coll (handle-none generated) (handle-any rng))
                      ;; - see `compound-logic`
                      (any-all-none generated)
                      any-all-none)]
        ;; attempt lookup, fallback to `matches`
        (in-path-fn matches))
      ;; attempt lookup, fallback to `generated`
      (in-path-fn generated))))

(comment
  ;; `within-path` takes highest priority bc it CAN be a direct reference to something in profile
  (= "test value"
     (handle-matchables
      {:matchable "matchable"
       :generated "generated"
       :within-path "mock-iri"
       :iri-lookup {"mock-iri" "test value"}}))
  ;; but if not, return `maybe-iri` + `fallback` with `matchable` taking higher priority then `generated`
  (= {:non-iri "lookup miss"
      :fallback "matchable"}
     (handle-matchables
      {:matchable "matchable"
       :generated "generated"
       :within-path "lookup miss"
       :iri-lookup {"mock-iri" "test value"}}))
  ;; but when `matchable` is not usable, fallback to `generated`
  (= {:non-iri "lookup miss"
      :fallback "generated"}
     (handle-matchables
      {:matchable nil
       :generated "generated"
       :within-path "lookup miss"
       :iri-lookup {"mock-iri" "test value"}})
     (handle-matchables
      {:matchable []
       :generated "generated"
       :within-path "lookup miss"
       :iri-lookup {"mock-iri" "test value"}}))
  ;; no special handling of `fallback`
  (= {:non-iri "lookup miss"
      :fallback []}
     (handle-matchables
      {:matchable []
       :generated []
       :within-path "lookup miss"
       :iri-lookup {"mock-iri" "test value"}}))
  ;; but when its *, "" or nil, next priority is `matchable`
  (= "Generated"
     (handle-matchables
      {:matchable (fn [d] (string/capitalize d))
       :generated "generated"
       :within-path nil
       :iri-lookup {}}))
  (= "matchable"
     (handle-matchables
      {:matchable "matchable"
       :generated "generated"
       :within-path :unexpected
       :iri-lookup {}})
     (handle-matchables
      {:matchable "matchable"
       :generated "generated"
       :within-path "*"
       :iri-lookup {}})
     (handle-matchables
      {:matchable "matchable"
       :generated "generated"
       :within-path ""
       :iri-lookup {}})
     (handle-matchables
      {:matchable "matchable"
       :generated "generated"
       :iri-lookup {}}))
  ;; return of `generation` only happens when necessary
  (= "generated"
     (handle-matchables
      {:matchable []
       :generated "generated"
       :within-path nil
       :iri-lookup {}})
     (handle-matchables
      {:matchable nil
       :generated "generated"
       :within-path nil
       :iri-lookup {}})
     (handle-matchables
      {:matchable ""
       :generated "generated"
       :within-path nil
       :iri-lookup {}})
     (handle-matchables
      {:matchable nil
       :generated "generated"
       :within-path :unexpected
       :iri-lookup {}})
     (handle-matchables
      {:generated "generated"
       :within-path "*"
       :iri-lookup {}})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Per rule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn matchable-values
  "handles interpretation of matchable values defined by the rule"
  [{:keys [matchable generated within-path iri-lookup]}]
  (let [matched-values (handle-matchables
                        {:matchable   matchable
                         :generated   generated
                         :within-path within-path
                         :iri-lookup  iri-lookup})
        non-iri?       (or (try (contains? matched-values :fallback)
                                (catch Exception e false))
                           (try (contains? matched-values :non-iri)
                                (catch Exception e false)))]
    (if non-iri?
      (let [{:keys [non-iri fallback]} matched-values]
        "TODO: figure out which is used as stmt value based on stmt-path")
      (let [{component-type :type :as profile-item} matched-values]
        "TODO: figure out what needs to be parsed from profile item based on stmt-path"))))

(defn from-rule
  "top level fn that returns `stmt-path` and `stmt-val` or nil"
  [{:keys [location
           ;; selector FIXME: not currently supported
           ;; scopeNote FIXME: not currently relevant
           presence
           any
           all
           none] :as rule}
   & {:keys [rng iri-lookup] :as passdown}]
  (when (continue-given-presence? presence)
    (let [{:keys [path nested]} (jpath/handle-json-path-str location)
          stmt-path             (jpath/deconstruct-json-path path)
          matchable             (matchable/compound-logic rule rng)
          generated             (loc/follow-stmt-path stmt-path :rng rng)
          stmt-val              (matchable-values {:matchable   matchable
                                                   :generated   generated
                                                   :within-path nested
                                                   :iri-lookup  iri-lookup})]
      {:stmt/path stmt-path
       :stmt/val  stmt-val})))
