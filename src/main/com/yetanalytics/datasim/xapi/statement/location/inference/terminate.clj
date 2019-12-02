(ns com.yetanalytics.datasim.xapi.statement.location.inference.terminate
  "Answers the question, 'Where am I?' given an arbitrary JSON Path String.

  These fns should be used alongside those found in the `within-xapi-type` ns
  in order to determine what xAPI Properties might need to be generated prior
  to comparison to any/all/none from the rule itself."
  (:require [com.yetanalytics.datasim.xapi.statement.helpers :as h]
            [com.yetanalytics.datasim.xapi.statement.location.nav-by-expectation :as nav]
            [com.yetanalytics.datasim.xapi.statement.location.inference.within-xapi-type :as within-xapi-type]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xAPI Properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; `within` vs `at` is necessary for correct generation
;; - ensure generation of valid xAPI JSON objects when JSON path doesn't
;;   provide any insight into what properties are WITHIN the JSON object
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Account IFI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-account-ifi?
  "terminate at the account IFI, not within it.
   - needed so that the generation can include both `name` and `homePage`
     without explicit reference to them within the rule location string."
  [stmt-path]
  (= "account" (peek stmt-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IFIs including account
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-ifi?
  "termiante at any IFI? Will return false if WITHIN the account IFI"
  [stmt-path]
  (let [terminate-at (peek stmt-path)]
    (or (at-account-ifi? stmt-path)
        (case terminate-at
          "mbox"         true
          "mbox_sha1sum" true
          "openid"       true
          false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private helper used as convience
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- location-check-helper
  [stmt-path prop]
  (:test-result (nav/path-value-check prop stmt-path) false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group Member
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-group-member?
  "`stmt-path` terminates at `member`"
  [stmt-path]
  (let [terminate-at (peek stmt-path)
        placeholder? (location-check-helper stmt-path "member")]
    (case terminate-at
      "member"     true
      :placeholder placeholder?
      false)))

(comment
  (= true
     (at-group-member? ["actor" "member" :placeholder])
     (at-group-member? ["actor" "member"])
     ;; within member, not at it
     (false? (at-group-member? ["actor" "member" :placeholder "mbox"]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-actor?
  "`stmt-path` terminates at `actor`"
  [stmt-path]
  (= "actor" (peek stmt-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-language-map?
  "does `stmt-path` terminate at one of the expected keys AND
   is the expected key found in the correct place?
   - see `within-xapi-type/language-map?` for more info"
  [stmt-path]
  (let [terminate-at   (peek stmt-path)
        within-a-def?  (within-xapi-type/activity-definition? stmt-path)
        within-v?      (within-xapi-type/verb? stmt-path)
        within-attach? (within-xapi-type/attachment? stmt-path)
        within-i-comp? (within-xapi-type/interaction-component? stmt-path)]
    (case terminate-at
      "display"     (or within-v? within-attach?)
      "description" (or within-i-comp? within-a-def? within-attach?)
      "name"        within-a-def?
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-language-tag?
  "if one step back is taken from the language tag, should end up at
   a language map given the places a language map can be.
    - equivalent to `within-xapi-type/language-map?` as must be
      AT a language tag to be WITHIN a language map"
  [stmt-path]
  (at-language-map? (pop stmt-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-verb?
  [stmt-path]
  (= "verb" (peek stmt-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interaction Component
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-interaction-component?
  [stmt-path]
  (let [terminate-at (peek stmt-path)]
    (case terminate-at
      "choices" true
      "scale"   true
      "source"  true
      "target"  true
      "steps"   true
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ISO Duration
;;
;; When dealing with a duration, we need to
;; let the simulation know how long the
;; event happened for to ensure no other
;; activity is generated for the actor within
;; that period of time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-duration?
  [stmt-path]
  (let [terminate-at (peek stmt-path)]
    (case terminate-at
      "duration" true
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Abstract Expectations for help within generation
;; - used in combination with other preds for proper generation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Array/Vector
;;
;; Collection of some kind of xAPI thing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-array?
  [stmt-path]
  (let [terminate-at (peek stmt-path)]
    (case terminate-at
      "correctResponsePattern" true
      "attachments"            true
      "category"               true
      "grouping"               true
      "member"                 true
      "parent"                 true
      "other"                  true
      "choices"                true
      "scale"                  true
      "source"                 true
      "target"                 true
      "steps"                  true
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON object
;;
;; will generation have to account for more
;; than whats indicated by JSON Path
;; - if account must be included for `instructor`
;;    what does that actually entail?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-json-object?
  [stmt-path]
  (let [terminate-at (peek stmt-path)]
    (case terminate-at
      "actor"      true
      "verb"       true
      "object"     true
      "result"     true
      "context"    true
      "authority" (throw (ex-info "authority is not currently supported!"
                                  {:stmt-path stmt-path}))
      "account"    true
      "definition" true
      "extensions" true
      ;; TODO: other places where valid to have JSON Object
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UUID
;;
;; `id` can be a UUID in certain places
;; - does location point to one of those?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-uuid?
  [stmt-path]
  (let [terminate-at (peek stmt-path)]
    (case terminate-at
      "id" (or (within-xapi-type/statement-reference? stmt-path)
               ;; TODO: other places where there can be a UUID
               )
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRI - non-lookup
;;
;; there are some xAPI properties that are IRI/URI/etc.
;; but the Profiles are not expected to contain any
;; additional information about these IRI/URI/etc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-non-lookup-iri?
  [stmt-path]
  (let [terminate-at (peek stmt-path)]
    (case terminate-at
      "openid"   true 
      "homePage" true
      "moreInfo" true
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRI - lookup in profile
;;
;; xAPI Properties whose IRI may have metadata
;; defined for it within the input Profiles
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-iri?
  [stmt-path]
  (let [terminate-at (peek stmt-path)]
    (case terminate-at
      "id"       (or (within-xapi-type/verb? stmt-path)
                     (within-xapi-type/object? stmt-path)
                     (within-xapi-type/activity? stmt-path))
      "type"     (or (within-xapi-type/activity-definition? stmt-path))
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enum - static value from xAPI spec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-enum?
  [stmt-path]
  (let [terminate-at (peek stmt-path)]
    (case terminate-at
      "objectType"      true
      "interactionType" true
      "contentType"     true
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String
;;
;; some String which may have xAPI Property
;; specific meaning but is not special otherwise
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-string?
  [stmt-path]
  (let [terminate-at (peek stmt-path)]
    (case terminate-at
      "name"         true
      "mbox"         true
      "mbox_sha1sum" true
      "id"           (within-xapi-type/interaction-component? stmt-path)
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timestamp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-timestamp?
  [stmt-path]
  (let [terminate-at (peek stmt-path)]
    (case terminate-at
      "timestamp" true
      "stored"    true      
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numeric
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn at-number?
  [stmt-path]
  (let [terminate-at (peek stmt-path)]
    (case terminate-at
      "length" true
      ;; TODO: other numbers
      false)))

(defn at-int?
  [stmt-path]
  (if (at-number? stmt-path)
    (let [terminate-at (peek stmt-path)]
      (case terminate-at
        "length" true
        ;; TODO: other ints
        false))
    false))

(defn at-double?
  [stmt-path]
  (if (at-number? stmt-path)
    (let [terminate-at (peek stmt-path)]
      (case terminate-at
        ;; TODO: the doubles
        false))
    false))




