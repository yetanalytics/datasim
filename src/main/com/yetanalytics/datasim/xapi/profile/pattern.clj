(ns com.yetanalytics.datasim.xapi.profile.pattern
  "Creation of `pattern-walk-fn` for Profile compilation."
  (:require [clojure.spec.alpha :as s]
            [java-time.api      :as t]
            [com.yetanalytics.pan.objects.template   :as template]
            [com.yetanalytics.pan.objects.pattern    :as pattern]
            [com.yetanalytics.datasim.model          :as model]
            [com.yetanalytics.datasim.model.temporal :as temporal]
            [com.yetanalytics.datasim.math.random    :as random]
            [com.yetanalytics.datasim.xapi.profile   :as-alias profile]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern Map Compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- root-pattern? [{:keys [id type alternates]}]
  (and (= ::root id)
       (= "Pattern" type)
       (not-empty alternates)))

(s/def ::pattern-map-id
  (s/or :root     #{::root}
        :pattern  ::pattern/id
        :template ::template/id))

(s/def ::pattern-map
  (s/map-of (s/or :root     #{::root}
                  :pattern  ::pattern/id
                  :template ::template/id)
            (s/or :root     root-pattern?
                  :pattern  ::pattern/pattern
                  :template ::template/template)))

(def ^:private root-pattern-base
  {:id         ::root
   :type       "Pattern"
   :prefLabel  {:en "Root Pattern"}
   :definition {:en "Dummy pattern to alternate primary patterns on."}})

(defn create-pattern-map
  [type-iri-map]
  (let [template-iri-map (get type-iri-map "StatementTemplate")
        pattern-iri-map  (get type-iri-map "Pattern")
        primary-ids      (->> pattern-iri-map
                              vals
                              (filter :primary)
                              (mapv :id))
        root-pattern     (assoc root-pattern-base
                                :alternates primary-ids)]
    (-> (merge template-iri-map pattern-iri-map)
        (assoc ::root root-pattern))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern Walk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-repeat-max 5)

(s/def ::alignments-map
  (s/map-of ::pattern-map-id ::model/pattern))

(s/def ::retry-id ::pattern-map-id)

(s/def ::alignments
  (s/merge ::model/pattern
           (s/keys :opt-un [::retry-id])))

(s/def ::template ::template/template)
(s/def ::timestamp ::temporal/date-time)
(s/def ::time-since-last t/duration?)
(s/def ::failure? boolean?)

(s/def ::template-map
  (s/keys :req-un [::template
                   ::timestamp
                   ::time-since-last]))

(s/def ::template-seq-meta
  (s/keys :req-un [::timestamp]
          :opt-un [::failure?
                   ::retry-id]))

(s/fdef walk-pattern
  :args (s/cat :context            (s/keys :req-un [::pattern-map
                                                    ::alignments-map
                                                    ::random/rng])
               :alignments         ::alignments
               :prev-timestamp     ::temporal/date-time
               :prev-timestamp-gen ::temporal/date-time
               :pattern            (s/or :pattern  ::pattern/pattern
                                         :template ::template/template))
  :ret (s/and (s/coll-of ::template-map)
              (s/conformer meta)
              ::template-seq-meta))

(defn- walk-pattern-dispatch
  [_ _ _ _ {:keys [sequence alternates optional oneOrMore zeroOrMore]}]
  (cond
    sequence   :sequence
    alternates :alternates
    optional   :optional
    oneOrMore  :one-or-more
    zeroOrMore :zero-or-more
    :else      :template))

(defmulti
  ^{:arglists '([ctx alignments prev-timestamp prev-timestamp-gen pattern])}
  walk-pattern
  walk-pattern-dispatch)

(defn- alternates->seq
  [{:keys [rng]} {:keys [weights]} alternates]
  (list (random/choose rng weights alternates)))

(defn- optional->seq
  [{:keys [rng]} {:keys [weights]} optional]
  (->> (random/choose rng weights [nil optional])
       list
       (filter some?)))

(defn- zero-or-more->seq
  [{:keys [rng]} {:keys [repeat-max]} zero-or-more]
  (-> (random/rand-int rng (inc (or repeat-max default-repeat-max)))
      (repeat zero-or-more)))

(defn- one-or-more->seq
  [{:keys [rng]} {:keys [repeat-max]} one-or-more]
  (-> (inc (random/rand-int rng (inc (or repeat-max default-repeat-max))))
      (repeat one-or-more)))

;; Test case TODOs:
;; - Different combos of time bounds and periods in general
;; - Nested bounds, e.g. if timestamp satisfies inner bound but
;;   not outer bound
;; - When generated timestamp ALWAYS exceeds the containing bound,
;;   causing gen to hang; need to solve w/ max-retries parameter

(defn- iterate-patterns
  [{:keys [pattern-map alignments-map] :as ctx}
   {:keys [retry] :as alignments}
   init-timestamp
   init-timestamp-gen
   pattern-id
   child-ids]
  (loop [prev-templates     (list)
         prev-timestamp     init-timestamp
         prev-timestamp-gen init-timestamp-gen
         child-ids          child-ids]
    (if-some [child-id (first child-ids)]
      (let [pattern    (get pattern-map child-id)
            pat-align  (get alignments-map child-id)
            pat-align* (cond-> (merge alignments pat-align)
                         retry (assoc :retry-id child-id))
            templates  (walk-pattern ctx
                                     pat-align*
                                     prev-timestamp
                                     prev-timestamp-gen
                                     pattern)
            {:keys [timestamp timestamp-gen failure? retry-id] :as temp-meta}
            (meta templates)]
        (cond
          (and failure?
               timestamp
               (= retry-id pattern-id))
          (case retry
            :template
            (throw (IllegalArgumentException. "No `:template` allowed"))
            :child
            (recur prev-templates timestamp prev-timestamp-gen child-ids)
            :pattern
            (recur (list) timestamp init-timestamp-gen child-ids)
            nil ; no alignments, no `retry`, or not the pattern w/ bound
            (with-meta (concat prev-templates templates)
              temp-meta))
          failure?
          (with-meta (concat prev-templates templates)
            temp-meta)
          :else
          (recur (concat prev-templates templates)
                 timestamp
                 timestamp-gen
                 (rest child-ids))))
      (with-meta prev-templates
        {:timestamp     prev-timestamp
         :timestamp-gen prev-timestamp-gen}))))

(defn- visit-template
  [{:keys [rng] :as ctx}
   {:keys [period bounds retry retry-id] :as alignments}
   prev-timestamp
   prev-timestamp-gen
   template]
  (let [timestamp (temporal/add-period prev-timestamp rng period)]
    (if (temporal/bounded-time? bounds timestamp)
      (with-meta (list {:template        template
                        :timestamp       timestamp
                        :time-since-last (t/duration prev-timestamp-gen timestamp)})
        {:timestamp     timestamp
         :timestamp-gen timestamp})
      (if-some [next-time (temporal/next-bounded-time bounds timestamp)]
        (if (or (= :template retry)
                (and (= :pattern retry) (nil? retry-id)))
          (visit-template ctx alignments next-time prev-timestamp-gen template)
          (with-meta (list)
            {:timestamp     next-time
             :timestamp-gen prev-timestamp-gen
             :failure?      true
             :retry-id      retry-id}))
        (with-meta (list)
          {:failure?      true
           :timestamp-gen prev-timestamp-gen
           :retry-id      retry-id})))))

(defmethod walk-pattern :sequence
  [ctx alignments prev-timestamp prev-timestamp-gen {:keys [id sequence]}]
  (->> sequence
       (iterate-patterns ctx alignments prev-timestamp prev-timestamp-gen id)))

(defmethod walk-pattern :alternates
  [ctx alignments prev-timestamp prev-timestamp-gen {:keys [id alternates]}]
  (->> alternates
       (alternates->seq ctx alignments)
       (iterate-patterns ctx alignments prev-timestamp prev-timestamp-gen id)))

(defmethod walk-pattern :optional
  [ctx alignments prev-timestamp prev-timestamp-gen {:keys [id optional]}]
  (->> optional
       (optional->seq ctx alignments)
       (iterate-patterns ctx alignments prev-timestamp prev-timestamp-gen id)))

(defmethod walk-pattern :zero-or-more
  [ctx alignments prev-timestamp prev-timestamp-gen {:keys [id zeroOrMore]}]
  (->> zeroOrMore
       (zero-or-more->seq ctx alignments)
       (iterate-patterns ctx alignments prev-timestamp prev-timestamp-gen id)))

(defmethod walk-pattern :one-or-more
  [ctx alignments prev-timestamp prev-timestamp-gen {:keys [id oneOrMore]}]
  (->> oneOrMore
       (one-or-more->seq ctx alignments)
       (iterate-patterns ctx alignments prev-timestamp prev-timestamp-gen id)))

(defmethod walk-pattern :template
  [ctx alignments prev-timestamp prev-timestamp-gen template]
  (visit-template ctx alignments prev-timestamp prev-timestamp-gen template))

(comment
  (def the-rng (random/seed-rng 1000))

  (def the-instant (t/instant "2023-09-11T15:00:00Z"))

  (->> (walk-pattern
        {:rng the-rng
         :pattern-map {::root {:alternates [:pattern-A #_:pattern-B]}
                       :pattern-A {:id :pattern-A
                                   :primary true
                                   :sequence [:template-A1 :template-A2]}
                       :pattern-B {:id :pattern-B
                                   :primary true
                                   :sequence [:template-B1]}
                       :template-A1 {:id "Template A1"}
                       :template-A2 {:id "Template A2"}
                       :template-B1 {:id "Template B1"}}
         :alignments-map
         {}
         #_{:pattern-A {:bounds
                        (temporal/convert-bounds
                         [{:years [2023 2024]
                           :minutes [[10 14]]}])
                        :retry :child}
            :pattern-B {:bounds
                        (temporal/convert-bounds
                         [{:years [2023 2024]
                           :minutes [[0 4]]}])
                        :retry :template}}}
        nil
        (t/local-date-time the-instant "UTC")
        {:alternates [:pattern-A #_:pattern-B]})
       (take 8))
  
  (temporal/next-bounded-time
   (temporal/convert-bounds [{:years [2023 2024]
                              :minutes [0]}])
   (t/local-date-time the-instant "UTC"))
  )
