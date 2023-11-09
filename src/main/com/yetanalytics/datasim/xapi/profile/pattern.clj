(ns com.yetanalytics.datasim.xapi.profile.pattern
  "Pattern map compilation and walking. This is where the magic of
   profile simulation happens."
  (:require [clojure.spec.alpha :as s]
            [java-time.api      :as t]
            [com.yetanalytics.pan.objects.template   :as template]
            [com.yetanalytics.pan.objects.pattern    :as pattern]
            [com.yetanalytics.datasim.model          :as model]
            [com.yetanalytics.datasim.model.bounds   :as bounds]
            [com.yetanalytics.datasim.model.periods  :as periods]
            [com.yetanalytics.datasim.util.random    :as random]
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

(s/def ::alignments-map
  (s/map-of ::pattern-map-id ::model/pattern))

(s/def ::max-restarts pos-int?)

(s/def ::restart-id ::pattern-map-id)

(s/def ::alignments ::model/pattern)

(s/def ::template ::template/template)
(s/def ::timestamp t/local-date-time?)
(s/def ::time-since-last t/duration?)
(s/def ::failure? boolean?)

(s/def ::template-map
  (s/keys :req-un [::template
                   ::timestamp
                   ::time-since-last]))

(s/def ::template-seq-meta
  (s/keys :req-un [::timestamp]
          :opt-un [::failure?
                   ::restart-id]))

(def context-spec
  (s/keys :req-un [::pattern-map
                   ::alignments-map
                   ::max-restarts
                   ::random/rng]))

(s/fdef walk-pattern
  :args (s/cat :context            context-spec
               :alignments-stack   (s/every ::alignments :kind vector?)
               :prev-timestamp     t/local-date-time?
               :prev-timestamp-gen t/local-date-time?
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
  ^{:arglists '([ctx alignments-stack prev-timestamp prev-timestamp-gen pattern])}
  walk-pattern
  walk-pattern-dispatch)

(defn- alternates->seq
  [{:keys [rng]} alignments-stack alternates]
  (let [{:keys [weights]} (peek alignments-stack)]
    (list (random/choose rng weights alternates))))

(defn- optional->seq
  [{:keys [rng]} alignments-stack optional]
  (let [{:keys [weights]} (peek alignments-stack)]
    (->> (random/choose rng weights [nil optional])
         list
         (filter some?))))

(defn- zero-or-more->seq
  [{:keys [rng]} alignments-stack zero-or-more]
  (let [{:keys [repeat-max]} (peek alignments-stack)
        repeat-max* (inc (or repeat-max model/default-repeat-max))]
    (-> (random/rand-int rng repeat-max*)
        (repeat zero-or-more))))

(defn- one-or-more->seq
  [{:keys [rng]} alignments-stack one-or-more]
  (let [{:keys [repeat-max]} (peek alignments-stack)
        repeat-max* (or repeat-max model/default-repeat-max)]
    (-> (inc (random/rand-int rng repeat-max*))
        (repeat one-or-more))))

(defn- iterate-patterns
  [{:keys [pattern-map alignments-map max-restarts] :as ctx}
   alignments-stack
   init-timestamp
   init-timestamp-gen
   pattern-id
   child-ids]
  (loop [prev-templates     (list)
         prev-timestamp     init-timestamp
         prev-timestamp-gen init-timestamp-gen
         child-ids*         child-ids
         restart-count      0]
    (if-some [child-id (first child-ids*)]
      (let [pattern     (get pattern-map child-id)
            pat-align   (-> (get alignments-map child-id) (assoc :id child-id))
            align-stack (conj alignments-stack pat-align)
            templates   (walk-pattern ctx
                                      align-stack
                                      prev-timestamp
                                      prev-timestamp-gen
                                      pattern)
            {:keys [timestamp timestamp-gen failure? restart-id] :as temp-meta}
            (meta templates)]
        (cond
          (and failure?
               timestamp
               (= restart-id pattern-id)
               (< restart-count max-restarts))
          (recur (list)
                 timestamp
                 init-timestamp-gen
                 child-ids
                 (inc restart-count))
          failure?
          (with-meta (concat prev-templates templates)
            temp-meta)
          :else
          (recur (concat prev-templates templates)
                 timestamp
                 timestamp-gen
                 (rest child-ids*)
                 restart-count)))
      (with-meta prev-templates
        {:timestamp     prev-timestamp
         :timestamp-gen prev-timestamp-gen}))))

(defn- repeat-at
  [alignments-stack timestamp]
  (loop [[alignments & rest-stack] alignments-stack]
    (if-some [{:keys [bounds bound-restarts]} alignments]
      (if (bounds/bounded-time? bounds timestamp)
        ;; Bound is satisfied
        (recur rest-stack)
        ;; Bound is NOT satisfied, find the highest-level pattern to restart
        ;; `some` works as alignments-stack vector goes from highest -> lowest
        (let [restart-id (when (not-empty bound-restarts)
                           (->> alignments-stack
                                (map :id)
                                (some bound-restarts)))]
          [bounds restart-id]))
      ;; All bounds are satisfied
      [nil nil])))

(defn- visit-template
  [{:keys [rng max-restarts]}
   alignments-stack
   init-timestamp
   init-timestamp-gen
   {template-id :id :as template}]
  (let [periods (some :periods alignments-stack)]
    (loop [prev-timestamp init-timestamp
           restart-count  0]
      (let [timestamp (periods/add-periods prev-timestamp rng periods)
            [?bounds ?restart-id] (repeat-at alignments-stack timestamp)]
        (if-not ?bounds
          (with-meta (list {:template        template
                            :timestamp       timestamp
                            :time-since-last (t/duration init-timestamp-gen
                                                         timestamp)})
            {:timestamp     timestamp
             :timestamp-gen timestamp})
          (if-some [next-time (bounds/next-bounded-time ?bounds timestamp)]
            (if (and (or (not ?restart-id)
                         (= template-id ?restart-id))
                     (< restart-count max-restarts))
              (recur next-time (inc restart-count))
              (with-meta (list)
                {:timestamp     next-time
                 :timestamp-gen init-timestamp-gen
                 :failure?      true
                 :restart-id    ?restart-id}))
            (with-meta (list)
              {:failure?      true
               :timestamp-gen init-timestamp-gen
               :restart-id    ?restart-id})))))))

(defmethod walk-pattern :sequence
  [ctx alignments-stack prev-timestamp prev-timestamp-gen {:keys [id sequence]}]
  (->> sequence
       (iterate-patterns ctx alignments-stack prev-timestamp prev-timestamp-gen id)))

(defmethod walk-pattern :alternates
  [ctx alignments-stack prev-timestamp prev-timestamp-gen {:keys [id alternates]}]
  (->> alternates
       (alternates->seq ctx alignments-stack)
       (iterate-patterns ctx alignments-stack prev-timestamp prev-timestamp-gen id)))

(defmethod walk-pattern :optional
  [ctx alignments-stack prev-timestamp prev-timestamp-gen {:keys [id optional]}]
  (->> optional
       (optional->seq ctx alignments-stack)
       (iterate-patterns ctx alignments-stack prev-timestamp prev-timestamp-gen id)))

(defmethod walk-pattern :zero-or-more
  [ctx alignments-stack prev-timestamp prev-timestamp-gen {:keys [id zeroOrMore]}]
  (->> zeroOrMore
       (zero-or-more->seq ctx alignments-stack)
       (iterate-patterns ctx alignments-stack prev-timestamp prev-timestamp-gen id)))

(defmethod walk-pattern :one-or-more
  [ctx alignments-stack prev-timestamp prev-timestamp-gen {:keys [id oneOrMore]}]
  (->> oneOrMore
       (one-or-more->seq ctx alignments-stack)
       (iterate-patterns ctx alignments-stack prev-timestamp prev-timestamp-gen id)))

(defmethod walk-pattern :template
  [ctx alignments-stack prev-timestamp prev-timestamp-gen template]
  (visit-template ctx alignments-stack prev-timestamp prev-timestamp-gen template))

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
  )
