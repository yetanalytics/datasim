(ns com.yetanalytics.datasim.xapi.agent
  (:require [clojure.spec.alpha :as s]))

(s/def ::agent-id
  (s/and string?
         not-empty
         (fn [^String s]
           (or (.startsWith s "mbox::")
               (.startsWith s "account::")
               (.startsWith s "mbox_sha1sum::")
               (.startsWith s "openid::")))))

(defn agent-id
  "Return a string representing the id of an agent in the sim. Will be prefixed
   with the ifi property and two colons ::. For accounts, the homepage will
   precede the name, and they will be delimited by a comma like:
   account::https://foo.bar,baz. If an IFI cannot be found, returns nil."
  [{:keys [mbox
           mbox_sha1sum
           openid]
    {:keys [name homePage]} :account}]
  (or (and mbox
           (format "mbox::%s" mbox))
      (and name homePage
           (format "account::%s,%s"
                   homePage name))
      (and mbox_sha1sum
           (format "mbox_sha1sum::%s" mbox_sha1sum))
      (and openid
           (format "openid::%s" openid))))
