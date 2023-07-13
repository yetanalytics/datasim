(ns com.yetanalytics.datasim.util.async
  "`core.async` utilities, for parallel generation."
  (:require [clojure.core.async :as a]
            [clojure.core.async.impl.protocols :as ap]))

(defn chan?
  "Is `x` a proper `core.async` channel?"
  [x]
  (satisfies? ap/Channel x))

(defn heads-chans
  "Get a sorted map of `[head-val <channel>]`, where `channel` is open."
  ([chans]
   (heads-chans compare chans))
  ([compfn chans]
   (a/go-loop [chan-set    (set chans)
               head-chan-m (sorted-map-by compfn)]
     (if-let [chan-seq (seq chan-set)]
       (let [[head chan] (a/alts! chan-seq)]
         (recur (disj chan-set chan)
                (cond-> head-chan-m
                  head (assoc head chan))))
       head-chan-m))))

(defn sequence-messages
  "Given an output channel `out-chan`, comparator `compfn`, and one or more
   channels `chans` containing ordered values, returns a channel that will
   receive an ordered sequence of messages, closing when the last channel
   closes."
  ([chans]
   (sequence-messages (a/chan) compare chans))
  ([out-chan compfn chans]
   (a/go-loop [head-chan-m (a/<! (heads-chans compfn chans))]
     (if-let [[min-head chan] (first head-chan-m)]
       (do (a/>! out-chan min-head)
           (let [?next-h (a/<! chan)]
             (recur (-> head-chan-m
                        (dissoc min-head)
                        (cond->
                            ?next-h (assoc ?next-h chan))))))
       (a/close! out-chan)))
   out-chan))
