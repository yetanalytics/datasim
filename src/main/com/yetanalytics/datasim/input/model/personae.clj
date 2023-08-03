(ns com.yetanalytics.datasim.input.model.personae
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.xapi.actor :as actor]
            [com.yetanalytics.datasim.input.model.personae.agent :as-alias agent]
            [com.yetanalytics.datasim.input.model.personae.group :as-alias group]
            [com.yetanalytics.datasim.input.model.personae.role  :as-alias role]))

(defmulti persona-spec* :type)

(s/def ::agent/id ::actor/actor-ifi)
(s/def ::agent/type #{"Agent"})

(defmethod persona-spec* "Actor" [_]
  (s/keys :req-un [::agent/id ::agent/type]))

(s/def ::group/id ::actor/actor-ifi)
(s/def ::group/type #{"Group"})

(defmethod persona-spec* "Group" [_]
  (s/keys :req-un [::group/id ::group/type]))

(s/def ::role/id (s/and string? not-empty))
(s/def ::role/type #{"Role"})

(defmethod persona-spec* "Role" [_]
  (s/keys :req-un [::role/id ::role/type]))

(def persona-spec
  (s/multi-spec persona-spec* :type))

(def personae-spec
  (s/every persona-spec :kind vector? :min-count 1))
