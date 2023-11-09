(ns com.yetanalytics.datasim.xapi.actor
  "Utilities for Agents and Groups (collectively known as Actors)."
  (:require [clojure.string         :as cstr]
            [clojure.spec.alpha     :as s]
            [clojure.spec.gen.alpha :as sgen]
            [xapi-schema.spec       :as xs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private ifi-prefix-gen
  (sgen/elements ["mbox::" "mbox_sha1sum::" "account::" "openid::"]))

(def ^:private ifi-body-gen
  (sgen/such-that not-empty (sgen/string-ascii)))

(s/def ::actor-ifi
  (s/with-gen
    (s/and string?
           not-empty
           (fn [s]
             (or (cstr/starts-with? s "mbox::")
                 (cstr/starts-with? s "account::")
                 (cstr/starts-with? s "mbox_sha1sum::")
                 (cstr/starts-with? s "openid::"))))
    ;; Monads are fun!
    ;; This composes a the two IFI generators together to create a
    ;; (str ifi-prefix ifi-body) generator
    #(sgen/bind ifi-prefix-gen
                (fn [ifi-prefix]
                  (sgen/bind ifi-body-gen
                             (fn [ifi-body]
                               (sgen/return (str ifi-prefix ifi-body))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef actor-ifi
  :args (s/cat :actor ::xs/actor)
  :ret ::actor-ifi)

(defn actor-ifi
  "Return a string representing the IFI of an Agent or Group. Will be prefixed
   with the IFI property and two colons `::`.
   
   For accounts, the homepage will precede the name, and they will be delimited
   by a comma as so: `account::https://foo.bar,baz`.
   
   If an IFI cannot be found, returns `nil`."
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

(s/fdef groups->agent-group-ifi-map
  :args (s/cat :group-coll (s/every ::xs/identified-group))
  :ret (s/map-of ::actor-ifi ::actor-ifi))

(defn groups->agent-group-ifi-map
  "Convert `group-coll` into a map from member agent IFIs to group IFIs."
  [group-coll]
  (reduce
   (fn [m {agents :member :as personae}]
     (let [group-ifi (actor-ifi personae)]
       (reduce
        (fn [m* actor]
          (let [agent-ifi (actor-ifi actor)]
            (assoc m* agent-ifi group-ifi)))
        m
        agents)))
   {}
   group-coll))
