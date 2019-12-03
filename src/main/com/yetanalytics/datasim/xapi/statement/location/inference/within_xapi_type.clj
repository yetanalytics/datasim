(ns com.yetanalytics.datasim.xapi.statement.location.inference.within-xapi-type
  "Answers the question, 'Where am I?' given an arbitrary JSON Path String.

  Usage of fns in this ns for inference should happen starting at top level xAPI Properties
  and then working down to the specific element of the target xAPI Property.
   - opposite of fn definition order within the ns
  
  Should be used in combination with the fns found in
  `com.yetanalytics.datasim.xapi.statement.location.inference.terminate`
  to determine if the JSON Path string terminates at either JSON Object or Array OR
  within a JSON Object or Array at some xAPI Property.

  When one of the fns in this ns is unable to make a determination based on `stmt-path` alone,
  will return a map of:
  {`:test-result`        `:maybe`,
   `:determining-factor` `stmt-property`,
   `:path-to-factor`     `key-seq-vec`,
   `:determination-fn`   `fn` which expects a map or value which will be used to compare against expectation.}"
  (:require [com.yetanalytics.datasim.xapi.statement.helpers :as h]
            [com.yetanalytics.datasim.xapi.statement.location.nav-by-expectation :as nav]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collections or JSON Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; if within a particular Collection or JSON Object, able to make assumptions
;; about what should be in the container at the terminal val and possible
;; with respect to other values within the container
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Account JSON Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn account-ifi?
  "determine if `stmt-path` points to Property of the Account IFI"
  [stmt-path]
  (let [terminate-at           (peek stmt-path)
        {within? :test-result} (nav/path-value-check "account" stmt-path)]
    (case terminate-at
      "name"     within?
      "homePage" within?
      false)))

(comment
  (= true
     ;; actor = agent or identified group
     (account-ifi? ["actor" "account" "homePage"])
     (account-ifi? ["actor" "account" "name"])
     ;; actor = group with members of agents
     ;; - :placeholder is used whenever JSON path describes an array
     ;;   - covers both splat (`*`) and reference to something within the array
     (account-ifi? ["actor" "member" :placeholder "account" "homePage"])
     (account-ifi? ["actor" "member" :placeholder "account" "name"])
     ;; object is an agent or identified group
     (account-ifi? ["object" "account" "homePage"])
     (account-ifi? ["object" "account" "name"])
     ;; object is a group with members of agents
     (account-ifi? ["object" "member" :placeholder "account" "homePage"])
     (account-ifi? ["object" "member" :placeholder "account" "name"])
     ;; instructor - can be Agent or Group
     ;; - Agent or identified Group
     (account-ifi? ["context" "instructor" "account" "homePage"])
     (account-ifi? ["context" "instructor" "account" "name"])
     ;; - Anon group with member of agents
     (account-ifi? ["context" "instructor" "member" :placeholder "account" "homePage"])
     (account-ifi? ["context" "instructor" "member" :placeholder "account" "name"])
     ;; team - identified or anon group
     ;; - identified group
     (account-ifi? ["context" "team" "account" "homePage"])
     (account-ifi? ["context" "team" "account" "name"])
     ;; - anon group with members of agents
     (account-ifi? ["context" "team" "member" :placeholder "account" "homePage"])
     (account-ifi? ["context" "team" "member" :placeholder "account" "name"])
     ;; all of above covers substatements as well.
     ;; - only difference in path is at the start, not what's checked here
     ;; if we are at the account, return false as not within
     ;; - see ...terminate/at-account-ifi? for rational for within vs at when it comes to generation
     (false? (account-ifi? ["actor" "account"]))
     (false? (account-ifi? ["actor" "member" :placeholder "account"]))
     (false? (account-ifi? ["object" "account"]))
     (false? (account-ifi? ["object" "member" :placeholder "account"]))
     (false? (account-ifi? ["context" "instructor" "account"]))
     (false? (account-ifi? ["context" "instructor" "member" :placeholder "account"]))
     (false? (account-ifi? ["context" "team" "account"]))
     (false? (account-ifi? ["context" "team" "member" :placeholder "account"]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent JSON Object within Group member array
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn group-member?
  "determine if `stmt-path` points to Property of a group member"
  [stmt-path]
  (let [terminate-at (peek stmt-path)
        enclosing?   (or (nav/step-back-expected? :placeholder stmt-path :next-key "member")
                         ;; if placeholder not included, unexpected but possible
                         (:test-result (nav/path-value-check "member" stmt-path)))
        account?     (and (account-ifi? stmt-path)
                          (or enclosing?
                              (nav/step-back-expected? :placeholder (pop stmt-path)
                                                       :next-key "member")))]
    ;; must always be a group of Agents, can't contain subgrouping
    (case terminate-at
      ;; Agent name or Agent account name
      "name"         (or account? enclosing?)
      "objectType"   enclosing?
      "mbox"         enclosing?
      "mbox_sha1sum" enclosing?
      "openid"       enclosing?
      "account"      enclosing?
      "homePage"     account?
      false)))

(comment
  (= true
     (group-member? ["context" "instructor" "member" :placeholder "name"])
     (group-member? ["context" "instructor" "member" "name"])
     (group-member? ["context" "instructor" "member" :placeholder "objectType"])
     (group-member? ["context" "instructor" "member" "objectType"])
     (group-member? ["context" "instructor" "member" :placeholder "mbox"])
     (group-member? ["context" "instructor" "member" "mbox"])
     (group-member? ["context" "instructor" "member" :placeholder "mbox_sha1sum"])
     (group-member? ["context" "instructor" "member" "mbox_sha1sum"])
     (group-member? ["context" "instructor" "member" :placeholder "account"])
     (group-member? ["context" "instructor" "member" "account"])
     (group-member? ["context" "instructor" "member" :placeholder "account" "name"])
     (group-member? ["context" "instructor" "member" "account" "name"])
     (group-member? ["context" "instructor" "member" :placeholder "account" "homePage"])
     (group-member? ["context" "instructor" "member" "account" "homePage"])
     (group-member? ["context" "team" "member" "account" "homePage"])
     (group-member? ["context" "team" "member" :placeholder "account" "homePage"])
     (group-member? ["object" "member" "account" "homePage"])
     (group-member? ["object" "member" :placeholder "account" "homePage"])
     (group-member? ["actor" "member" "account" "homePage"])
     (group-member? ["actor" "member" :placeholder "account" "homePage"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; One of the xAPI properties which CAN be a group
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn property-which-can-be-a-group
  "groups can only be found in certain parts of an xAPI stmt, determines if `stmt-path`
   is navigating into one of those properties"
  [stmt-path]
  (let [;; does `stmt-path` point to something within the Account IFI?
        account?     (account-ifi? stmt-path)
        ;; top level Actor key or account IFI within actor
        actor?       (or (:test-result (nav/path-value-check "actor" stmt-path) false)
                         (and account? (nav/step-back-expected? "account" stmt-path :next-key "actor")))
        ;; top level Object key or account IFI within Object
        obj?         (or (:test-result (nav/path-value-check "object" stmt-path) false)
                         (and account? (nav/step-back-expected? "account" stmt-path :next-key "object")))
        ;; top level Agent or Group key within instructor or account IFI within instructor
        instruct?    (or (nav/step-back-expected? "instructor" stmt-path :next-key "context")
                         (and account? (nav/step-back-expected? "instructor" (pop stmt-path) :next-key "context")))
        ;; top level Group key within team or account IFI within team
        team?        (or (nav/step-back-expected? "team" stmt-path :next-key "context")
                         (and account? (nav/step-back-expected? "team" (pop stmt-path) :next-key "context")))]
    (or actor? obj? instruct? team?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Identified Group JSON Object without `member`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  ;; this is probably better in the top level inference ns.
 (defn memberless-identified-group?
  [stmt-path]
  (let [terminate-at (peek stmt-path)
        ;; does `stmt-path` point to something within the Account IFI?
        account?     (account-ifi? stmt-path)
        ifi?         (or account?
                         (case terminate-at
                           "mbox"         true
                           "mbox_sha1sum" true
                           "openid"       true
                           ;; `account-ifi?` on checks for properties OF/WITHIN the account ifi
                           "account"      true
                           false))
        resolve?     (or ifi?
                         ;; does `stmt-path` point to something within an Agent or Identified Group thats not an IFI?
                         (case terminate-at
                           "name"         true
                           "objectType"   true
                           false))
        ;; based on path to target xAPI Property
        given-path?  (property-which-can-be-a-group stmt-path)
        ;; should false be returned or a fn which will determine if memberless identified group
        maybe?       (and resolve? given-path?)
        ;; top level Group key within team or account IFI within team
        team?        (or (nav/step-back-expected? "team" stmt-path :next-key "context")
                         (and account? (nav/step-back-expected? "team" (pop stmt-path) :next-key "context")))]
    (if (and ifi? team?)
      true
      ;; `ifi?` tells us identified thing, `team?` tells us group. nothing more needed
      (if maybe?
        ;; need to check against `objectType` and potentially look for an `ifi` in the map
        {:test-result        :maybe
         :determining-factor (if ifi? "objectType" ["objectType" "IFI"])
         :path-to-factor     (if ifi?
                               ;; know the thing is identified, just need to confirm `objectType` is `Group`
                               (cond actor?    ["actor" "objectType"]
                                     obj?      ["object" "objectType"]
                                     instruct? ["context" "instructor" "objectType"])
                               ;; need to verify IFI exists AND `objectType` is `Group`
                               (cond actor? {:type-path ["actor" "objectType"]
                                             :ifi-path  [["actor" "account"]
                                                         ["actor" "mbox"]
                                                         ["actor" "mbox_sha1sum"]
                                                         ["actor" "openid"]]}
                                     obj? {:type-path ["object" "objectType"]
                                           :ifi-path  [["object" "account"]
                                                       ["object" "mbox"]
                                                       ["object" "mbox_sha1sum"]
                                                       ["object" "openid"]]}
                                     instruct? {:type-path ["context" "instructor" "objectType"]
                                                :ifi-path  [["context" "instructor" "account"]
                                                            ["context" "instructor" "mbox"]
                                                            ["context" "instructor" "mbox_sha1sum"]
                                                            ["context" "instructor" "openid"]]}))
         :determination-fn (fn [target-obj-type-or-m]
                             (let [obj-type (if (map? target-obj-type-or-m)
                                              (cond actor?
                                                    (get-in target-obj-type-or-m ["actor" "objectType"])
                                                    obj?
                                                    (get-in target-obj-type-or-m ["object" "objectType"])
                                                    instruct?
                                                    (get-in target-obj-type-or-m ["context" "instructor" "objectType"])
                                                    ;; FIXME: on else case, look into these places within a sub statement
                                                    )
                                              target-obj-type-or-m)]
                               (= "Group"
                                  ;; what value to we compare to `Group`
                                  (if ifi?
                                    ;; its identified, only need to check object type
                                    obj-type
                                    ;; need to ensure there IS an IFI
                                    (let [at-ifi (cond actor?
                                                       (or (get-in target-obj-type-or-m ["actor" "account"])
                                                           (get-in target-obj-type-or-m ["actor" "mbox"])
                                                           (get-in target-obj-type-or-m ["actor" "mbox_sha1sum"])
                                                           (get-in target-obj-type-or-m ["actor" "openid"]))
                                                       obj?
                                                       (or (get-in target-obj-type-or-m ["object" "account"])
                                                           (get-in target-obj-type-or-m ["object" "mbox"])
                                                           (get-in target-obj-type-or-m ["object" "mbox_sha1sum"])
                                                           (get-in target-obj-type-or-m ["object" "openid"]))
                                                       instruct?
                                                       (or (get-in target-obj-type-or-m ["context" "instructor" "account"])
                                                           (get-in target-obj-type-or-m ["context" "instructor" "mbox"])
                                                           (get-in target-obj-type-or-m ["context" "instructor" "mbox_sha1sum"])
                                                           (get-in target-obj-type-or-m ["context" "instructor" "openid"]))
                                                       ;; FIXME: on else case, look into these places within a sub statement
                                                       :else nil)]
                                      ;; if there was something non-nil at IFI compare to object type otherwise force
                                      ;; equality check to fail bc when clause will return nil
                                      ;; - no ifi means it was an anon group and anon group must have `member` property
                                      (when at-ifi obj-type))))))}
        false)))))

(comment
  (= true
     ;; fn call not necessary
     (memberless-identified-group? ["context" "team" "mbox"])
     ;; look into objectType for actor
     ((-> ["actor" "mbox"]
         memberless-identified-group?
         :determination-fn)
      {"actor" {"objectType" "Group"
                "mbox" "mailto:someemail@somedaomin.com"}})
     ;; lookup unnecessary as objectType resolved via other means
     ((-> ["actor" "mbox"]
          memberless-identified-group?
          :determination-fn)
      "Group")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Identified Group JSON Object with member
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  ;; probably better of in top level inference ns
 (defn identified-group-with-member?
  [stmt-path]
  ;; TODO: impl!
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anon Group JSON Object with member
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  ;; probably better of in top level inference ns
  (defn anon-group?
    [stmt-path]
    ;; TODO: impl!
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Any Group JSON Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  ;; probably better of in top level inference ns
  (defn group?
    [stmt-path]
    ;; TODO: impl!
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent JSON Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  ;; probably better of in top level inference ns
  (defn agent?
    [stmt-path]
    ;; TODO: impl!
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interaction Component JSON Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- some-interaction-component?
  "Stepback expectations are consistent across interaction components except for the name of the component"
  [stmt-path ic-key]
  (let [terminate-at  (peek stmt-path)
        without-l-tag (pop stmt-path)
        pred-result   (or
                       ;; at `id` or `desc` and `:placeholder` may have been used
                       (and (or (nav/step-back-expected? :placeholder stmt-path :next-key ic-key)
                                (nav/step-back-expected? ic-key stmt-path :next-key "definition"))
                            (or (= terminate-at "id")
                                (= terminate-at "description")))
                       ;; at `language-tag` within `description` and `:placeholder` may have been used
                       (and (or (nav/step-back-expected? "description" stmt-path :next-key ic-key)
                                (nav/step-back-expected? "description" stmt-path :next-key :placeholder))
                            ;; if `language-tag` removed from path, should be within `ic-key` regardless of `:placeholder`
                            (or (nav/step-back-expected? :placeholder without-l-tag :next-key ic-key)
                                (nav/step-back-expected? ic-key without-l-tag :next-key "definition"))))]
    (case ic-key
      "choices" pred-result
      "scale"   pred-result
      "source"  pred-result
      "target"  pred-result
      "steps"   pred-result
      false)))

(defn choices?
  "call `some-interaction-component?` with `ic-key` set to `choices`"
  [stmt-path]
  (some-interaction-component? stmt-path "choices"))

(defn scale?
  "call `some-interaction-component?` with `ic-key` set to `scale`"
  [stmt-path]
  (some-interaction-component? stmt-path "scale"))

(defn source?
  "call `some-interaction-component?` with `ic-key` set to `source`"
  [stmt-path]
  (some-interaction-component? stmt-path "source"))

(defn target?
  "call `some-interaction-component?` with `ic-key` set to `target`"
  [stmt-path]
  (some-interaction-component? stmt-path "target"))

(defn steps?
  "call `some-interaction-component?` with `ic-key` set to `steps`"
  [stmt-path]
  (some-interaction-component? stmt-path "steps"))

(comment
  (= true
     ;; choices
     (choices? ["definition" "choices" :placeholder "id"])
     (choices? ["definition" "choices" "id"])
     (choices? ["definition" "choices" :placeholder "description"])
     (choices? ["definition" "choices" "description"])
     (choices? ["definition" "choices" :placeholder "description" "en-US"])
     (choices? ["definition" "choices" "description" "en-US"])
     ;; scale
     (scale? ["definition" "scale" :placeholder "id"])
     (scale? ["definition" "scale" "id"])
     (scale? ["definition" "scale" :placeholder "description"])
     (scale? ["definition" "scale" "description"])
     (scale? ["definition" "scale" :placeholder "description" "en-US"])
     (scale? ["definition" "scale" "description" "en-US"])
     ;; source
     (source? ["definition" "source" :placeholder "id"])
     (source? ["definition" "source" "id"])
     (source? ["definition" "source" :placeholder "description"])
     (source? ["definition" "source" "description"])
     (source? ["definition" "source" :placeholder "description" "en-US"])
     (source? ["definition" "source" "description" "en-US"])
     ;; target
     (target? ["definition" "target" :placeholder "id"])
     (target? ["definition" "target" "id"])
     (target? ["definition" "target" :placeholder "description"])
     (target? ["definition" "target" "description"])
     (target? ["definition" "target" :placeholder "description" "en-US"])
     (target? ["definition" "target" "description" "en-US"])
     ;; steps
     (steps? ["definition" "steps" :placeholder "id"])
     (steps? ["definition" "steps" "id"])
     (steps? ["definition" "steps" :placeholder "description"])
     (steps? ["definition" "steps" "description"])
     (steps? ["definition" "steps" :placeholder "description" "en-US"])
     (steps? ["definition" "steps" "description" "en-US"])
     ;; WITHIN not AT
     (false? (choices? ["definition" "choices"]))
     (false? (scale? ["definition" "scale"]))
     (false? (source? ["definition" "source"]))
     (false? (target? ["definition" "target"]))
     (false? (steps? ["definition" "steps"]))))

(defn interaction-component?
  "does `stmt-path` point to something within one of the interaction components?"
  [stmt-path]
  (or (choices? stmt-path)
      (scale? stmt-path)
      (source? stmt-path)
      (target? stmt-path)
      (steps? stmt-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attachments JSON Object or Collection of
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- attachments-helper
  "step back to `:placeholder` then `attachments` or back to `attachments`"
  [path]
  (or (nav/step-back-expected? :placeholder path :next-key "attachments")
      (:test-result (nav/path-value-check "attachments" path) false)))

(comment
  (= true
     ;; but not just AT attachments
     (false? (attachments-helper ["attachments"]))
     ;; WITHIN attachments, AT the collection
     (attachments-helper ["attachments" :placeholder])
     ;; AT some arbitrary key WITHIN attachment object WITHIN collection
     (attachments-helper ["attachments" :placeholder "foo"])
     ;; AT some arbitrary key WITHIN attachment object
     (attachments-helper ["attachments" "foo"])))

(defn attachment?
  "within an attachment JSON Object or Collection of attachment JSON Objects"
  [stmt-path]
  (let [terminate-at  (peek stmt-path)
        within?       (attachments-helper stmt-path)]
    (case terminate-at
      ;; terminate at attachment property
      "usageType"   within?
      "display"     within?
      "description" within?
      "contentType" within?
      "length"      within?
      "sha2"        within?
      "fileUrl"     within?
      ;; possibly terminate at language tag within language map attachment property
      (let [no-l-tag (pop stmt-path)
            attach?  (attachments-helper no-l-tag)
            display? (or (nav/step-back-expected? "display" stmt-path :next-key :placeholder)
                         (nav/step-back-expected? "display" stmt-path :next-key "attachments"))
            desc?    (or (nav/step-back-expected? "description" stmt-path :next-key :placeholder)
                         (nav/step-back-expected? "description" stmt-path :next-key "attachments"))]
        (or (and attach? display?)
            (and attach? desc?))))))

(comment
  (= true
     ;; AT expected key WITHIN attachment object
     (attachment? ["attachments" "usageType"])
     ;; AT expected key WITHIN attachment object WITHIN collection
     (attachment? ["attachments" :placeholder "usageType"])
     ;; NOT AT unexpected key WITHIN attachment object
     (false? (attachment? ["attachments" "foo"]))
     ;; NOT AT unexpected key WITHIN attachment object WITHIN collection
     (false? (attachment? ["attachments" :placeholder "foo"]))
     ;; WITHIN language map at expected key WITHIN attachment object
     (attachment? ["attachments" :placeholder "display" "en-US"])
     (attachment? ["attachments" :placeholder "description" "en-US"])
     ;; WITHIN language map at expected key WITHIN attachment object WITHIN collection
     (attachment? ["attachments" "display" "en-US"])
     (attachment? ["attachments" "description" "en-US"])
     ;; NOT WITHIN language map AT unexpected key WITHIN attachment object
     (false? (attachment? ["attachments" "foo" "en-US"]))
     ;; NOT WITHIN language map AT unexpected key WITHIN attachment object WITHIN collection
     (false? (attachment? ["attachments" :placeholder "foo" "en-US"]))
     ;; Rest of expected keys WITHIN attachment object
     (attachment? ["attachments" "display"])
     (attachment? ["attachments" "description"])
     (attachment? ["attachments" "contentType"])
     (attachment? ["attachments" "length"])
     (attachment? ["attachments" "sha2"])
     (attachment? ["attachments" "fileUrl"])
     ;; Rest of expected keys WITHIN attachment object WITHIN collection
     (attachment? ["attachments" :placeholder "display"])
     (attachment? ["attachments" :placeholder "description"])
     (attachment? ["attachments" :placeholder "contentType"])
     (attachment? ["attachments" :placeholder "length"])
     (attachment? ["attachments" :placeholder "sha2"])
     (attachment? ["attachments" :placeholder "fileUrl"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Map JSON Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn language-map?
  "stepping back from an arbitrary language tag should result in either
   `display`, `description` or `name`. Otherwise not within a language map

   ensures the language map is found within an expected xAPI JSON Object
   or Collection of JSON Objects (as appropriate)
    - ie. `verb`, `activity-definition`, `interaction-component`, `attachments`"
  [stmt-path]
  (let [;; Step back to expected property?
        {in-display? :test-result} (nav/path-value-check "display" stmt-path)
        {in-desc?    :test-result} (nav/path-value-check "description" stmt-path)
        {in-name?    :test-result} (nav/path-value-check "name" stmt-path)
        ;; any of the expected properties?
        given-container?           (or in-display? in-desc? in-name?)
        ;; specific to `verb`
        verb?                      (nav/step-back-expected? "display" stmt-path :next-key "verb")
        ;; specific to an `activity-definition` but NOT the container FOR the `activity-definition`
        activity-def?              (or (nav/step-back-expected? "name" stmt-path :next-key "definition")
                                       (nav/step-back-expected? "description" stmt-path :next-key "definition"))
        ;; specific to one of the `interaction-components`
        ;; - ie. `choices`, `scale`, `source`, `target` or `steps`
        i-component?               (interaction-component? stmt-path)
        ;; specific to `attachment` JSON Object or collection of JSON Objects
        attachment?                (attachment? stmt-path)]
    ;; if one of the possible containers
    (if given-container?
      ;; scoped to key(s) within the JSON Object or collection of JSON Objects?
      (or
       ;; WITHIN `verb` `display`
       verb?
       ;; WITHIN `activity-definition` `name` or `description`
       activity-def?
       ;; WITHIN `interaction-component` `description`
       i-component?
       ;; WITHIN `attachment` `display` or `description`
       attachment?)
      false)))

(comment
  ;; not good enought to just be WITHIN one of the expected language map containers
  (= false
     (language-map? ["display" "en-US"])
     (language-map? ["description" "en-US"])
     (language-map? ["name" "en-US"])
     (language-map? ["some random key" "description" "en-US"])
     (language-map? ["some random key" "display" "en-US"])
     (language-map? ["some random key" "name" "en-US"]))
  ;; must be an expected language map!
  (= true
     ;; `verb`
     ;; WITHIN language map NOT AT languate map
     (false? (language-map? ["verb" "display"]))
     (language-map? ["verb" "display" "en-US"])
     ;; `activity-definition`
     ;; WITHIN language map NOT AT languate map
     (false? (language-map? ["definition" "description"]))
     (language-map? ["definition" "description" "en-US"])
     (false? (language-map? ["definition" "name"]))
     (language-map? ["definition" "name" "en-US"])
     (language-map? ["context" "contextActivities" "parent" :placeholder "definition" "name" "en-US"])
     ;; `interaction-component` WITHIN `activity-definition`
     ;; - `choices`
     (false? (language-map? ["choices" "description" "en-US"]))
     (false? (language-map? ["choices" "description"]))
     (false? (language-map? ["definition" "choices" "description"]))
     (language-map? ["definition" "choices" "description" "en-US"])
     (language-map? ["definition" "choices" :placeholder "description" "en-US"])
     ;; - `scale`
     (false? (language-map? ["scale" "description" "en-US"]))
     (language-map? ["definition" "scale" "description" "en-US"])
     (language-map? ["definition" "scale" :placeholder "description" "en-US"])
     ;; - `source`
     (false? (language-map? ["source" "description" "en-US"]))
     (language-map? ["definition" "source" "description" "en-US"])
     (language-map? ["definition" "source" :placeholder "description" "en-US"])
     ;; - `target`
     (false? (language-map? ["target" "description" "en-US"]))
     (language-map? ["definition" "target" "description" "en-US"])
     (language-map? ["definition" "target" :placeholder "description" "en-US"])
     ;; - `steps`
     (false? (language-map? ["steps" "description" "en-US"]))
     (language-map? ["definition" "steps" "description" "en-US"])
     (language-map? ["definition" "steps" :placeholder "description" "en-US"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Reference JSON Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn statement-reference?
  "was the `objectType` or `id` found within an Object or context.statement?
   - if found within context, garunteed to be within statement reference
   - if found within object, returns a fn which expects to be passed `objectType`
     - fn will return determination of 'within object that is a statement reference?'"
  [stmt-path]
  (let [terminate-at    (peek stmt-path)
        within-object?  (-> "object"
                            (nav/path-value-check stmt-path)
                            ;; return false instead of nil for explicit boolean instead of infered falsey
                            (:test-result false))
        within-context? (nav/step-back-expected? "statement" stmt-path :next-key "context")
        not-certain     (if within-object?
                          {:test-result        :maybe
                           :determining-factor "objectType"
                           ;; sub statement can not itself have a statement reference as its object
                           ;; - this path is always correct
                           :path-to-factor     ["object" "objectType"]
                           :determination-fn   (fn [target-obj-type-or-m]
                                                 (= "StatementRef"
                                                    (if (map? target-obj-type-or-m)
                                                      (get-in target-obj-type-or-m ["object" "objectType"])
                                                      target-obj-type-or-m)))}
                          false)]
    (case terminate-at
      "objectType" (or within-context? not-certain)
      "id"         (or within-context? not-certain)
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context Activities JSON Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- context-activity-array?
  "helper fn for determining if WITHIN one of the context Activities but not AT one of them"
  [stmt-path target-ctx-activity]
  (let [up-to-target (try (subvec stmt-path 0 3) (catch Exception e []))
        to-target?   (= (peek up-to-target) target-ctx-activity)
        at-target?   (= stmt-path up-to-target)]
    (if at-target?
      ;; only return true when WITHIN not AT
      false
      (and to-target?
           (nav/step-back-expected? "contextActivities" up-to-target :next-key "context")))))

(defn parent?
  "does `stmt-path` point at a property within the parent context activities array"
  [stmt-path]
  (context-activity-array? stmt-path "parent"))

(defn grouping?
  "does `stmt-path` point at a property within the grouping context activities array"
  [stmt-path]
  (context-activity-array? stmt-path "grouping"))

(defn category?
  "does `stmt-path` point at a property within the category context activities array"
  [stmt-path]
  (context-activity-array? stmt-path "category"))

(defn other?
  "does `stmt-path` point at a property within the other context activities array"
  [stmt-path]
  (context-activity-array? stmt-path "other"))

(comment
  (= true
     (parent? ["context" "contextActivities"])
     (parent? ["context" "contextActivities" "parent" :placeholder "id"])
     (parent? ["context" "contextActivities" "parent" "id"])
     (parent? ["context" "contextActivities" "parent" "extensions"])
     (parent? ["context" "contextActivities" "parent" "extensions" "some-iri-key"])
     (parent? ["context" "contextActivities" "parent" :placeholder "extensions" "some-iri-key"])
     ;; WITHIN not AT
     (false? (parent? ["context" "contextActivities" "parent"]))
     ;; only calibrated for the Parent context activity
     (false? (parent? ["context" "contextActivities" "other" "id"]))
     (other? ["context" "contextActivities" "other" "id"])
     (other? ["context" "contextActivities" "other" :placeholder "id"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context Activities JSON Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn context-activity?
  "does `stmt-path` point to something within one of the context activities"
  [stmt-path]
  (or (parent? stmt-path)
      (grouping? stmt-path)
      (category? stmt-path)
      (other? stmt-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object Extension JSON Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn object-extension?
  "does `stmt-path` point to something within an Object Extension"
  [stmt-path]
  (let [up-to-ext (try (subvec stmt-path 0 3) (catch Exception e []))]
    (and (= (peek up-to-ext) "extensions")
         (nav/step-back-expected? "definition" up-to-ext :next-key "object"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context Extension JSON Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn context-extension?
  "does `stmt-path` point to something within a Context Extension"
  [stmt-path]
  (let [up-to-ext (try (subvec stmt-path 0 2) (catch Exception e []))]
    (nav/step-back-expected? "extensions" up-to-ext :next-key "context")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Result Extension JSON Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn result-extension?
  "does `stmt-path` point to something within a Result Extension"
  [stmt-path]
  (let [up-to-ext (try (subvec stmt-path 0 2) (catch Exception e []))]
    (nav/step-back-expected? "extensions" up-to-ext :next-key "result")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activity Extension JSON Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn context-activity-extension?
  "does `stmt-path` point to something within an Activity Extension"
  [stmt-path]
  (let [end-idx   (if (h/containsv? stmt-path :placeholder) 6 5)
        up-to-ext (try (subvec stmt-path 0 end-idx) (catch Exception e []))
        at-ext    (peek up-to-ext)]
    (if (= stmt-path up-to-ext)
      ;; WITHIN not AT
      false
      (and
       (= at-ext "extensions")
       (or
        ;; parent w/ placeholder or without
        (parent? stmt-path)
        ;; grouping w/ placeholder or without
        (grouping? stmt-path)
        ;; category w/ placeholder or without
        (category? stmt-path)
        ;; other w/ placeholder or without
        (other? stmt-path))))))

(comment
  (context-activity-extension? ["context" "contextActivities" "parent" :placeholder "definition" "extensions" "some-iri-k"])
  (context-activity-extension? ["context" "contextActivities" "parent" "definition" "extensions" "some-iri-k"])
  (false?
   (context-activity-extension? ["context" "contextActivities" "foo" "definition" "extensions" "some-iri-k"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extension JSON Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn extension?
  "does `stmt-path` point to a key within an extension"
  [stmt-path]
  (let [ctx-ext?   (context-extension? stmt-path)
        r-ext?     (result-extension? stmt-path)
        obj-ext?   (object-extension? stmt-path)
        act-ext?   (context-activity-extension? stmt-path)]
    (or ctx-ext? r-ext? obj-ext? act-ext?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activity Definition JSON Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn activity-definition?
  [stmt-path]
  (let [terminate-at (peek stmt-path)
        ;; does a single step back result in `definition`
        within-def?  (:test-result (nav/path-value-check "definition" stmt-path) false)
        ;; at some language tag within a language map
        within-lmap? (language-map? stmt-path)
        ;; at some language map within a `definition`
        at-lmap?     (and
                      within-def?
                      (case terminate-at
                        "name"        true
                        "description" true
                        false))
        ;; within some extension, context, result, object activity, context activity
        within-ext?  (extension? stmt-path)
        ;; at extension key within a activity definition
        at-ext?      (and
                      within-def?
                      (= terminate-at "extensions"))
        ;; at the `type` key within an activity definition
        at-type?     (and
                      within-def?
                      (= terminate-at "type"))
        ;; at the `moreInfo` key within an activity definition
        at-more-info (and
                      within-def?
                      (= terminate-at "moreInfo"))
        ;; within one of the interaction components
        within-ia    (interaction-component? stmt-path)
        ;; within the correct response pattern array
        within-crp   (nav/step-back-expected? "correctResponsePattern" stmt-path :next-key "definition")
        ;; at one of the interaction keys within an activity definition
        at-iap?      (and
                      within-def?
                      (case terminate-at
                        "interactionType"         true
                        "correctResponsesPattern" true
                        "choices"                 true
                        "scale"                   true
                        "source"                  true
                        "target"                  true
                        "steps"                   true
                        false))]
    (or within-lmap?
        at-lmap?
        within-ext?
        at-ext?
        at-type?
        at-more-info
        within-ia
        within-crp
        at-iap?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activity JSON Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn activity?
  [stmt-path]
  (let [terminate-at (peek stmt-path)
        ;; based on the property
        top-lvl-k    (case terminate-at
                       "objectType" true
                       "id"         true
                       "definition" true
                       false)
        ;; if within an activity definition, also within an activity
        in-def?      (activity-definition? stmt-path)
        ;; based on expected locations
        ;; TODO: left off here
        ])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn instructor?
  [stmt-path]
  ;; TODO: impl!
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Team
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn team?
  [stmt-path]
  ;; TODO: impl!
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context Statement Reference
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn context-statement-ref?
  [stmt-path]
  ;; TODO: impl!
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Score JSON object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn score?
  [stmt-path]
  ;; TODO: impl!
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top level xAPI Properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; which Top level xAPI Property does `stmt-path` point to?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn actor?
  "does `stmt-path` point to something that is within actor?"
  [stmt-path]
  (let [terminate-at (peek stmt-path)
        ;; top level actor properties
        enclosing?   (nav/step-back-expected? "actor" stmt-path :expected-pop ["actor"])
        ;; within actor's account IFI for an Agent
        account?     (and (nav/step-back-expected? "account" stmt-path :next-key "actor")
                          (account-ifi? stmt-path))
        ;; within actor's member array, account IFI handled within `group-member?`
        member?      (and (group-member? stmt-path)
                          ;; restriction to just actor members based on first 2 keys of `stmt-path`
                          (= (subvec stmt-path 0 2) ["actor" "member"]))
        ;; terminate at :placeholder
        group?       (nav/step-back-expected? "member" stmt-path :expected-pop ["actor" "member"])]
    (case terminate-at
      ;; could be top level name or account.name
      "name"         (or member? account? enclosing?)
      "objectType"   (or member? enclosing?)
      "member"       enclosing?
      "mbox"         (or member? enclosing?)
      "mbox_sha1sum" (or member? enclosing?)
      "openid"       (or member? enclosing?)
      "account"      (or member? enclosing?)
      "homePage"     (or member? account?)
      :placeholder   group?
      false)))

(comment
  (= true
     (actor? ["actor" "objectType"])
     (actor? ["actor" "name"])
     (actor? ["actor" "mbox"])
     (actor? ["actor" "mbox_sha1sum"])
     (actor? ["actor" "openid"])
     (actor? ["actor" "account"])
     (actor? ["actor" "account" "homePage"])
     (actor? ["actor" "account" "name"])
     (actor? ["actor" "member"])
     (actor? ["actor" "member" :placeholder])
     (actor? ["actor" "member" :placeholder "objectType"])
     (actor? ["actor" "member" :placeholder "name"])
     (actor? ["actor" "member" :placeholder "mbox"])
     (actor? ["actor" "member" :placeholder "mbox_sha1sum"])
     (actor? ["actor" "member" :placeholder "openid"])
     (actor? ["actor" "member" :placeholder "account"])
     (actor? ["actor" "member" :placeholder "account" "homePage"])
     (actor? ["actor" "member" :placeholder "account" "name"])
     ;; at actor, not within
     (false? (actor? ["actor"]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn verb?
  "does `stmt-path` point to something that is within verb?"
  [stmt-path]
  (let [terminate-at   (peek stmt-path)
        id-or-display? (nav/step-back-expected? "verb" stmt-path :expected-pop ["verb"])
        ;; if JSON Path points to particular language map entry within display regardless of language tag
        in-display?    (nav/step-back-expected? "display" stmt-path :expected-pop ["verb" "display"])]
    (case terminate-at
      "id"      id-or-display?
      "display" id-or-display?
      ;; must be enclosed within verb display for arbitrary string key to return true
      ;; - TODO: use language-map? + start with ["verb" "display"]
      in-display?)))

(comment
  (= true
     (verb? ["verb" "id"])
     (verb? ["verb" "display"])
     (verb? ["verb" "display" "en-US"])
     ;; not within Verb, at it
     (false? (verb? ["verb"]))
     ;; language map is not for verb display
     (false? (verb? ["attachments" :placeholder "display" "en-US"]))
     (false? (verb? ["attachments" "display" "en-US"]))
     (false? (verb? ["attachments" :placeholder "display"]))
     (false? (verb? ["attachments" "display"]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn object?
  [stmt-path]
  ;; TODO: impl!
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn context?
  [stmt-path]
  ;; TODO: impl!
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Result
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn result?
  [stmt-path]
  ;; TODO: impl!
  )






