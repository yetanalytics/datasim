(ns com.yetanalytics.datasim.util.async
  (:require [clojure.core.async :as a]))

(defn heads-chans
  "Get a sorted map of [head-val <channel>], where channel is open."
  ([chans]
   (heads-chans compare chans))
  ([compfn chans]
   (a/go-loop [cset (set chans)
               hcmap (sorted-map-by compfn)]
     (if-let [cseq (seq cset)]
       (let [[h c :as hc] (a/alts! cseq)]
         (recur (disj cset c)
                (cond-> hcmap
                  h (assoc h c))))
       hcmap))))

(defn sequence-messages
  "Given an output channel, comparator, and one or more channels containing
  ordered values, returns a channel that will receive an ordered sequence of
  messages, closing when the last channel closes."
  ([chans]
   (sequence-messages (a/chan) compare chans))
  ([out-chan compfn chans]
   (a/go-loop [hcmap (a/<! (heads-chans compfn chans))]
     (if-let [[min-h c :as min-hc] (first hcmap)]
       (do (a/>! out-chan min-h)
           (let [?next-h (a/<! c)]
             (recur (-> hcmap
                        (dissoc min-h)
                        (cond->
                            ?next-h (assoc ?next-h c))))))
       (a/close! out-chan)))
   out-chan))
