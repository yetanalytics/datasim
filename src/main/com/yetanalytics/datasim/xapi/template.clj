(ns com.yetanalytics.datasim.xapi.template
  (:require [clojure.spec.alpha :as s]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.pathetic :as path]))

(defmulti rule-follows-spec?
  (fn [{:keys [location selector]}]
    (let [loc (path/parse-paths location {:strict? true})
          sel (path/parse-paths selector {:strict? true})]
      (vec (concat loc sel)))))

(defmethod rule-follows-spec? ['*] [{:keys [any all none]}]
  (every? (partial s/valid? ::xs/statement) (concat any all none)))

(defmethod rule-follows-spec? ["id"] [{:keys [any all none]}]
  (every? (partial s/valid? :statement/id) (concat any all none)))
