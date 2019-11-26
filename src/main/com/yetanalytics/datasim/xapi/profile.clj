(ns com.yetanalytics.datasim.xapi.profile
  "Understanding elements of xAPI profiles
  Generate Profile walks"
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.iri :as iri]
            [com.yetanalytics.datasim.random :as random]
            [com.yetanalytics.pan.objects.profile :as profile]
            [com.yetanalytics.pan.objects.concept :as concept]
            [com.yetanalytics.pan.objects.pattern :as pattern]
            [com.yetanalytics.pan.objects.template :as template]
            [clojure.zip :as z])
  (:import [java.util Random]))


(s/def ::iri-map
  (s/map-of iri/iri-spec
            (s/or :concept ::concept/concept
                  :pattern ::pattern/pattern
                  :template ::template/template)))

(s/fdef profiles->map
  :args (s/cat :profiles (s/+ ::profile/profile))
  :ret ::iri-map)

(defn profiles->map
  "Given any number of profiles, return all IRIs mapped to what they reference"
  [& profiles]
  (assert (seq profiles) "At least one profile is required.")
  (into {}
        (for [profile profiles
              [_ things] (select-keys profile [:concepts :patterns :templates])
              {:keys [id] :as thing} things]
          [id thing])))

(defn loc-iri-map
  "Given a zipper loc, get the map of profile subobjects by IRI"
  [loc]
  (get (meta loc) ::iri-map))

(defn loc-object
  "Given a zipper loc, look up the object from the IRI"
  [loc]
  (get (loc-iri-map loc) (z/node loc)))

(defn root-node?
  [node-iri]
  (= ::root node-iri))

(defn pattern-zip
  "Given one or more profiles, create a zipper that traverses the patterns and
  templates. Root is a special keyword ::root with the primary patterns as
  `alternates` children."
  [& profiles]
  (let [iri-map' (apply profiles->map profiles)
        primary-pattern-iris
        (keep
         (fn [{id :id
               node-type :type
               primary :primary}]
           (when (and (= node-type "Pattern")
                      primary)
             id))
         (vals iri-map'))]
    (assert (seq primary-pattern-iris)
            "Profile input must contain primary patterns!")
    ;; Add the virtual top level before proceeding
    (let [iri-map (assoc iri-map'
                         ::root
                         {:id ::root
                          :type "Pattern"
                          :alternates (into [] primary-pattern-iris)})]
      (->
       (z/zipper
        (fn branch? [node-iri]
          (let [{node-type :type} (get iri-map node-iri)]
            (= node-type "Pattern")))
        ;; The default children function leaves all options open
        ;; We can override :zip/children to make it decide things.
        (fn children [node-iri]
          (let [{:keys [sequence
                        alternates
                        optional
                        oneOrMore
                        zeroOrMore
                        ]} (get iri-map node-iri)]
            (or sequence
                alternates
                (and optional
                     [optional])
                (and oneOrMore
                     [oneOrMore])
                (and zeroOrMore
                     [zeroOrMore]))))
        ;; Make node doesn't really mean much here, as we don't edit.
        (fn make-node
          [node-iri kid-iris]
          node-iri)
        ::root)
       ;; We store the map in meta, which is always preserved
       (vary-meta assoc ::iri-map iri-map)))))


(defn rand-pattern-zip
  "Building on the comprehension from pattern-zip, return a zipper that uses a
  deterministic pseudorandom walk for choosing children/paths."
  [profiles
   alignment
   ^Random rng
   & {:keys [repeat-max]
      :or {repeat-max 5}}]
  (let [pzip (apply pattern-zip profiles)
        iri-map (::iri-map (meta pzip))]
    (vary-meta pzip
               assoc
               :zip/children
               (fn rand-children
                 [node-iri]
                 (let [{:keys [id
                               type
                               primary

                               alternates
                               optional
                               oneOrMore
                               sequence
                               zeroOrMore
                               ]
                        {label :en} :prefLabel
                        :as profile-obj} (get iri-map node-iri)]
                   (assert profile-obj "Can't navigate if not a pattern")
                   (cond
                     alternates
                     [(random/choose rng alignment alternates)]

                     optional
                     (if-let [yeah (random/choose rng alignment [nil optional])]
                       [optional]
                       [])
                     ;; TODO: Figure out effect, if any
                     oneOrMore
                     (repeat (inc (random/rand-int* rng repeat-max))
                             oneOrMore)
                     zeroOrMore
                     (repeat (random/rand-int* rng repeat-max)
                             zeroOrMore)
                     sequence
                     sequence
                     ))))))

;; TODO: Pass antecedent patterns in the result of registration seq

(defn walk-once
  "From the root of a pattern zip, perform a single walk of a primary pattern,
  returning a seq of locs"
  [loc]
  (->> loc
       (iterate z/next)
       (take-while (complement z/end?))
       ;; cut the root node off the top
       rest
       ;; empty seq nodes will be nil, so only keep the good'ns
       (filter z/node)))

(defn registration-seq
  "Given a seed, alignment and profiles, return an infinite seq of maps where
  `:registration` is a string uuid
  `:template` is a statement template
  `:seed` is a derived seed for generating the statement."
  ([profiles
    alignment
    seed]
   (lazy-seq
    (let [^Random rng (random/seed-rng seed)
          root-loc (rand-pattern-zip profiles alignment rng)]
      (registration-seq root-loc rng))))
  ([root-loc
    ^Random rng]
   (lazy-seq
    (let [registration (random/rand-uuid rng)]
      (concat
       (->> root-loc
            ;; do one deterministic walk
            walk-once
            ;; Get the statement templates
            (keep (fn [loc]
                    (let [{obj-type :type
                           :as o} (loc-object loc)
                          iri-map (::iri-map (meta loc))]
                      (when (= "StatementTemplate" obj-type)
                        {:registration registration
                         :template o
                         :seed (random/rand-long rng)
                         :pattern-ancestors
                         (into []
                               (for [iri (rest (z/path loc))
                                     :let [{:keys [primary]} (get iri-map iri)]]
                                 {:id iri
                                  :primary (true? primary)}))})))))
       (registration-seq
        root-loc
        rng))))))

(comment


  (require '[com.yetanalytics.datasim.input :as i])

  (def p
    (i/from-location
     :profile :json "dev-resources/profiles/cmi5/fixed.json"))

  (-> p
      :templates
      (->> (mapcat :rules))
      clojure.pprint/pprint)



  ;; traverse randomly
  (-> (rand-pattern-zip
       {} (Random. ) #_(random/seed-rng 2)
       [p])
      walk-once
      (->> (map z/node))
      last
      )
  (->> (registration-seq [p] {} 42)
       first
       clojure.pprint/pprint
       )


  (def loc (atom (rand-pattern-zip
                  [p] {} (random/seed-rng 45)
                  )))


  (z/node @loc
          )
  (some #(when (nil? (z/node %))
           %)
        (iterate z/next @loc))




  )
