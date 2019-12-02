(ns com.yetanalytics.datasim.xapi.statement.location.inference
  (:require [com.yetanalytics.datasim.xapi.statement.helpers :as h]
            [com.yetanalytics.datasim.xapi.statement.location.determining-property
             :refer [path-to-determining-property?]]
            [com.yetanalytics.datasim.xapi.statement.location.nav-by-expectation
             :as nav]
            [com.yetanalytics.datasim.xapi.statement.location.inference.within-xapi-type
             :as within-xapi-type]
            [com.yetanalytics.datasim.xapi.statement.location.inference.terminate
             :as terminate]))

(defn path-to-non-determining-property?
  "determining properties are static given the spec so everything else
   must not be a determining property"
  [stmt-path]
  (not (path-to-determining-property? stmt-path)))

(comment
  ;; this is why `nav/step-back-expected?` exists.
  ;; - need to run inference to ensure the path points to a valid part of an xAPI statement
  (path-to-non-determining-property? ["foo" "baz"]))

(defn extension-for
  [stmt-path]
  (when (within-xapi-type/extension? stmt-path)
    (cond
      ;; activity extension within object
      (within-xapi-type/object? stmt-path)
      {:target       ["object" "definition" "extensions"]
       :lookup-type  ["ActivityExtension"]
       :profile-type ["concepts"]
       :path         stmt-path}
      ;; activity extension within parent
      (within-xapi-type/parent? stmt-path)
      {:target       ["context" "contextActivities" "parent"]
       :lookup-type  ["ActivityExtension"]
       :profile-type ["concepts"]
       :path         stmt-path}
      ;; activity extension within grouping
      (within-xapi-type/grouping? stmt-path)
      {:target       ["context" "contextActivities" "grouping"]
       :lookup-type  ["ActivityExtension"]
       :profile-type ["concepts"]
       :path         stmt-path}
      ;; activity extension within category
      (within-xapi-type/category? stmt-path)
      {:target       ["context" "contextActivities" "category"]
       :lookup-type  ["ActivityExtension"]
       :profile-type ["concepts"]
       :path         stmt-path}
      ;; activity extension within other
      (within-xapi-type/other? stmt-path)
      {:target       ["context" "contextActivities" "other"]
       :lookup-type  ["ActivityExtension"]
       :profile-type ["concepts"]
       :path         stmt-path}
      ;; context extension
      (within-xapi-type/context? stmt-path)
      {:target       ["context" "extensions"]
       :lookup-type  ["ContextExtension"]
       :profile-type ["concepts"]
       :path         stmt-path}
      ;; result extension
      (within-xapi-type/result? stmt-path)
      {:target       ["result" "extensions"]
       :lookup-type  ["ResultExtension"]
       :profile-type ["concepts"]
       :path         stmt-path})))

(defn iri-lookup-for
  [stmt-path]
  (when (terminate/at-iri? stmt-path)
    (let [terminate-at (peek stmt-path)]
      (case terminate-at
        "id" (cond (within-xapi-type/verb? stmt-path)
                   {:target ["verb" "id"]
                    ;; TODO: refactor from here on down!
                    ;; - use within? + at? as necessary
                    ;; - follow example set by `extension-for` + start of `id`
                    :lookup-type []}))
      (comment
        (cond
          ((within-xapi-type/object? stmt-path))
          {:possibilities ["object id"]}
          (and (nav/step-back-expected? "definition" stmt-path :next-key "object")
               (= terminate-at "type"))
          {:possibilities ["object type"]}
          (and (nav/step-back-expected? "definition" stmt-path :next-key "object")
               (= terminate-at "moreInfo"))
          {:possibilities ["object more info"]}
          ;; TODO: other places where valid to have an iri
          )))))

(comment
  (defn uuid-for
    [stmt-path]
    (when (path-terminate-at-uuid? stmt-path)
      (cond (nav/step-back-expected? nil stmt-path :expected-pop [])
            {:possibilities ["statement"]}
            (nav/step-back-expected? "object" stmt-path :expected-pop ["object"])
            {:possibilities ["statement reference"]}
            ;; TODO: other places where statement ref can exist
            )

      )
    ))

(comment
  (defn language-map-for
    [stmt-path]
    (when (path-terminate-at-language-map? stmt-path)
      (let [terminate-at (peek stmt-path)]
        (cond (nav/step-back-expected? "verb" stmt-path :expected-pop ["verb"])
              {:possibilities ["verb"]}
              (and (nav/step-back-expected? "definition" stmt-path :next-key "object")
                   (= terminate-at "description"))
              {:possibilities ["object description"]}
              (and (nav/step-back-expected? "definition" stmt-path :next-key "object")
                   (= terminate-at "name"))
              {:possibilities ["object name"]}
              ;; TODO: other places where language map can exist
              )))))

(comment
  (defn name-for
    [stmt-path]
    (when (= "name" (peek stmt-path))
      (cond (nav/step-back-expected? "actor" stmt-path :expected-pop ["actor"])
            {:possibilities ["actor"]}
            (nav/step-back-expected? "account" stmt-path :next-key "actor")
            {:possibilities ["actor account ifi"]}
            ;; TODO: other places where there can be a name
            ))))

;; TODO: for generation of correct response patterns, if :data doesn't resolve to possible values and any/all/none doesn't give an vals
;; - default to "answer-n" for ids where n is distinct for each answer id but unique to object id
;; - when its a number or range of numbers, just make sure min is less than max
;; - when boolean, just pick true or false
;; - when arbitrary string, use random text generator? like from mockup land

(defn array-of
  [stmt-path]
  (when (path-terminate-at-array? stmt-path)
    (let [terminate-at (peek stmt-path)]
      (cond (nav/step-back-expected? "actor" stmt-path :expected-pop ["actor"])
            {:possibilities ["agents"]}
            (and (or (nav/step-back-expected? "definition" stmt-path :next-key "object")
                     ;; TODO: other places where there can be an activity
                     )
                 (= terminate-at "correctResponsePattern"))
            ;; TODO: account for "case_matters" and "order_matters" and "lang" for string responses
            ;; TODO: update to align with refactor of above fns (as necessary)
            {:possibilities [{:interaction-type {:vals       ["true-false"]
                                                 :data       (h/replace-last stmt-path "interactionType") }
                              :crp              {:vals       [true false]
                                                 :type       :boolean
                                                 :count      :one}}
                             {:interaction-type {:vals       ["choice"]
                                                 :data       (h/replace-last stmt-path "interactionType")}
                              :crp              {:vals       ["choice" "choices seperated by [,]"]
                                                 :type       :id
                                                 :data       (h/replace-last stmt-path "choices")
                                                 :count      :unbounded}}
                             {:interaction-type {:vals       ["fill-in"]
                                                 :data       (h/replace-last stmt-path "interactionType")}
                              :crp              {:vals       ["response" "responses seperated by [,]"]
                                                 :type       :string
                                                 :count      :unbounded}}
                             {:interaction-type {:vals       ["long-fill-in"]
                                                 :data       (h/replace-last stmt-path "interactionType")}
                              :crp              {:vals       ["response" "responses seperated by [,]"]
                                                 :type       :string
                                                 :count      :unbounded}}
                             {:interaction-type {:vals       ["matching"]
                                                 :data       (h/replace-last stmt-path "interactionType")}
                              :crp              {:vals       ["source[.]target" "pairs seperated by [,]"]
                                                 :type       :pair
                                                 :data       {:source   (h/replace-last stmt-path "source")
                                                              :target   (h/replace-last stmt-path "target")}
                                                 :components {:source   :id
                                                              :target   :id}
                                                 :count      :unbounded}}
                             {:interaction-type {:vals       ["performance"]
                                                 :data       (h/replace-last stmt-path "interactionType")}
                              :crp              {:vals       ["step[.]response" "steps seperated by [,]"]
                                                 :type       :pair
                                                 :data       {:step     (h/replace-last stmt-path "steps")}
                                                 :components {:step     :id
                                                              :response ["fill-in" "long-fill-in" "numeric"]}
                                                 :count      :unbounded}}
                             {:interaction-type {:vals       ["sequencing"]
                                                 :data       (h/replace-last stmt-path "interactionType")}
                              :crp              {:vals       ["choices seperated by [,]"]
                                                 :type       :id
                                                 :data       (h/replace-last stmt-path "choices")
                                                 :count      :unbounded}}
                             {:interaction-type {:vals       ["likert"]
                                                 :data       (h/replace-last stmt-path "interactionType")}
                              :crp              {:vals       ["scale"]
                                                 :type       :id
                                                 :data       (h/replace-last stmt-path "scale")
                                                 :count      :one}}
                             {:interaction-type {:vals       ["numeric"]
                                                 :data       (h/replace-last stmt-path "interactionType")}
                              :crp              {:vals       ["min[:]max" "[:]max" "min[:]" "raw"]
                                                 :type       [:range :number]
                                                 :components {:min      :number
                                                              :max      :number
                                                              :raw      :number}
                                                 :count      :one}}
                             {:interaction-type {:vals       ["other"]
                                                 :data       (h/replace-last stmt-path "interactionType")}
                              :crp              {:vals       ["abitrary string"]
                                                 :type       :string
                                                 :count      :one}}]}
            (and (or (nav/step-back-expected? "definition" stmt-path :next-key "object")
                     ;; TODO: other places where there can be an activity
                     )
                 (= terminate-at "choices"))
            {:possibilities []}
            (and (or (nav/step-back-expected? "definition" stmt-path :next-key "object")
                     ;; TODO: other places where there can be an activity
                     )
                 (= terminate-at "scale"))
            {:possibilities []}
            (and (or (nav/step-back-expected? "definition" stmt-path :next-key "object")
                     ;; TODO: other places where there can be an activity
                     )
                 (= terminate-at "source"))
            {:possibilities []}
            (and (or (nav/step-back-expected? "definition" stmt-path :next-key "object")
                     ;; TODO: other places where there can be an activity
                     )
                 (= terminate-at "target"))
            {:possibilities []}
            (and (or (nav/step-back-expected? "definition" stmt-path :next-key "object")
                     ;; TODO: other places where there can be an activity
                     )
                 (= terminate-at "steps"))
            ;; TODO: other kinds of arrays
            ))))

(comment
  (defn enum-for
    [stmt-path]
    (when (path-terminate-at-enum? stmt-path)
      (cond (nav/step-back-expected? "actor" stmt-path :expected-pop ["actor"])
            {:possibilities ["Agent" "Group"]}
            (or (nav/step-back-expected? "object" stmt-path :expected-pop ["object"])
                ;; TODO: other places where there can be an activity
                )
            {:possibilities ["Activity" "StatementRef" "SubStatement" "Agent" "Group" nil]}
            (or (nav/step-back-expected? "definition" stmt-path :next-key "object")
                ;; TODO: other places where there can be an activity
                )
            {:possibilities ["true-false" "choice" "fill-in" "long-fill-in" "matching"
                             "performance" "sequencing" "likert" "numeric" "other"]}

            ;; TODO: other possible values for enums
            )
      )
    ))
