(ns com.yetanalytics.datasim.util.async
  (:require [clojure.core.async :as a]))

(defn heads-chans
  "Get a sorted map of `[head-val <channel>]`, where channel `chans` is open."
  ([chans]
   (heads-chans compare chans))
  ([compfn chans]
   (a/go-loop [chan-set    (set chans)
               head-chan-m (sorted-map-by compfn)]
     (if-let [chan-seq (seq chan-set)]
       (let [[head channel] (a/alts! chan-seq)]
         (recur (disj chan-set channel)
                (cond-> head-chan-m
                  head (assoc head channel))))
       head-chan-m))))

(defn sequence-messages
  "Given an output channel `out-chan`, comparator `compfn`, and one or more
   channels `chans` containing ordered values, returns a channel that will
   receive an ordered sequence of messages, closing when the last channel
   closes. If `out-chan` and `compfn` are not provided, then the default values
   `core.async/chan` are `core/compare` are used."
  ([chans]
   (sequence-messages (a/chan) compare chans))
  ([out-chan compfn chans]
   (a/go-loop [head-chan-m (a/<! (heads-chans compfn chans))]
     (if-let [[min-head chan] (first head-chan-m)]
       (do (a/>! out-chan min-head)
           (let [?next-head   (a/<! chan)
                 head-chan-m* (dissoc head-chan-m min-head)]
             (recur (cond-> head-chan-m*
                      ?next-head (assoc ?next-head chan)))))
       (a/close! out-chan)))
   out-chan))
