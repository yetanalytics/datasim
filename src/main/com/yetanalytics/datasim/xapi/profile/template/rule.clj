(ns com.yetanalytics.datasim.xapi.profile.template.rule
  "Apply statement template rules"
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.pan.objects.templates.rules :as rules]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.datasim.json :as j]
            [com.yetanalytics.datasim.json.path :as json-path]
            [com.yetanalytics.datasim.json.zip :as jzip]
            [com.yetanalytics.datasim.xapi.path :as xp]
            [clojure.zip :as z]))
