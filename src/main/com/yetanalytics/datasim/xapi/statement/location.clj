(ns com.yetanalytics.datasim.xapi.statement.location
  (:require [com.yetanalytics.datasim.xapi.statement.helpers :as h]
            [com.yetanalytics.datasim.xapi.statement.location.inference :as inf]
            [com.yetanalytics.datasim.xapi.statement.location.nav-by-expectation :as nav]
            [com.yetanalytics.datasim.xapi.statement.location.determining-property :refer [path-to-determining-property?]]))

(defn follow-stmt-path
  "responsible for creation of `generated` "
  [stmt-path & {:keys [rng] :as passdown}]
  (let [[top-lvl-k] stmt-path
        ?more       (not-empty (h/butfirst stmt-path))]
    (case top-lvl-k
      "id"          "FIXME: handle presence + any/all/none"
      "actor"       "FIXME: ?more is relevant"
      "verb"        "FIXME: ?more is relevant"
      "object"      "FIXME: ?more is relevant"
      "result"      "FIXME: ?more is relevant"
      "context"     "FIXME: ?more is relevant"
      "timestamp"   "FIXME: handle presence + any/all/none"
      "stored"      "FIXME: handle presence + any/all/none"
      "authority"   (throw (ex-info "authority is not currently supported!"
                                    {:stmt-path stmt-path}))
      "version"     (throw (ex-info "version is not currently supported!"
                                    {:stmt-path stmt-path}))
      "attachments" (throw (ex-info "attachments are not currently supported!"
                                    {:stmt-path stmt-path}))
      (throw (ex-info "unexpected JSONPath key after root!"
                      {:path       stmt-path
                       :after-root top-lvl-k
                       :more       ?more})))))
