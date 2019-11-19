(ns com.yetanalytics.datasim.xapi
  "Specs and conventions for xAPI"
  (:require [clojure.spec.alpha :as s]))

(s/def ::agent-id
  (s/and string?
         not-empty
         (fn [^String s]
           (or (.startsWith s "mbox::")
               (.startsWith s "account::")
               (.startsWith s "mbox_sha1sum::")
               (.startsWith s "openid::")))))
