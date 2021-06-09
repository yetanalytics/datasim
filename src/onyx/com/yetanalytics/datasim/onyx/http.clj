(ns com.yetanalytics.datasim.onyx.http
  (:require [cheshire.core :as json]))

(defn after-read-batch
  "Encode statements and add request"
  [event {lrs-request ::lrs-request
          :as lifecycle}]
  (update event
          :onyx.core/batch
          (partial map
                   (fn [{:keys [statements]
                         :as segment}]
                     (merge segment
                            (assoc-in lrs-request
                                      [:args :body]
                                      ;; TODO: Buffr
                                      (json/generate-string statements)))))))

(defn after-batch
  "Tear things down"
  [event lifecycle]
  (update event
          :onyx.core/batch
          (partial map
                   (fn [{:keys [statements]
                         :as segment}]
                     (-> segment
                         (dissoc :statements)
                         (update :args dissoc :body :basic-auth)
                         (assoc :statement-count (count statements)))))))

(def out-calls
  {:lifecycle/after-read-batch after-read-batch
   :lifecycle/after-batch after-batch})

(defn post-success?
  [{:keys [status error]}]
  (and (= 200 status)
       (not error)))
