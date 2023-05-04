(ns com.yetanalytics.datasim.util.xapi
  "Misc. xAPI-oriented utilities")

(defn agent-id
  "Return a string representing the id of an agent in the sim. Will be prefixed
   with the IFI property and two colons `::`. For accounts, the homepage will
   precede the name, and they will be delimited by a comma like:
   ```
   account::https://foo.bar,baz
   ```
   
   If an IFI cannot be found, returns `nil`."
  [{:keys [mbox
           mbox_sha1sum
           openid]
    {:keys [name homePage]} :account
    :as _agent}]
  (or (and mbox
           (format "mbox::%s" mbox))
      (and name homePage
           (format "account::%s,%s"
                   homePage name))
      (and mbox_sha1sum
           (format "mbox_sha1sum::%s" mbox_sha1sum))
      (and openid
           (format "openid::%s" openid))))

(defn make-ifi-map
  "Given an xAPI Group, make a map of IFIs."
  [{:keys [member] :as _group}]
  (into {}
        (for [actor member
              :let [id (agent-id actor)]
              :when id]
          [id actor])))
