(ns com.yetanalytics.datasim.xapi.registration
  (:require [clojure.spec.alpha :as s]
            [clojure.zip :as z]
            [xapi-schema.spec :as xs]
            [com.yetanalytics.pan.objects.template :as template]
            [com.yetanalytics.pan.objects.pattern  :as pattern]
            [com.yetanalytics.datasim.math.random  :as random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Consolidate these specs with those in `xapi.statement`
(s/def ::seed int?)
(s/def ::pattern-ancestors (s/every ::pattern/pattern))

(s/def ::registration-map
  (s/keys :req-un [::template/template
                   ::xs/registration
                   ::seed
                   ::pattern-ancestors]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- pattern-zip
  "Create a zipper over the Patterns and Statement Templates found in
   `type-iri-map`. A special `::root` sentinel Pattern is created as an
   alternates Pattern of all the primary Patterns in the profiles.
   The zipper can then be walked; traversal will be done in a deterministic,
   pseudorandom fashion, in which `rng` and `alignment` is used to choose
   the children of each node in the zipper."
  [type-iri-map alignment rng & {:keys [repeat-max]
                                 :or   {repeat-max 5}}]
  (let [pat-iri-map     (get type-iri-map "Pattern")
        primary-pat-ids (->> pat-iri-map vals (filter :primary) (mapv :id))
        root-pattern    {:id         ::root
                         :type       "Pattern"
                         :alternates primary-pat-ids}
        pat-iri-map*    (assoc pat-iri-map ::root root-pattern)]
    (z/zipper
     (fn branch? [node-id] ; it is a branch if it's a pattern
       (contains? pat-iri-map* node-id))
     (fn children [node-id] ; choose children using rng
       (let [{:keys [sequence alternates optional oneOrMore zeroOrMore]}
             (get pat-iri-map* node-id)]
         (cond
           sequence   sequence
           alternates [(random/choose rng alignment alternates)]
           optional   (or (some->> [nil optional]
                                   (random/choose rng alignment)
                                   vector)
                          [])
           oneOrMore  (repeat (inc (random/rand-int* rng repeat-max))
                              oneOrMore)
           zeroOrMore (repeat (random/rand-int* rng repeat-max)
                              zeroOrMore))))
     (fn make-node [node-id _child-ids] ; this is a no-op
       node-id)
     ::root)))

(defn- walk-once
  "From the root of a pattern zip, perform a single walk of a primary Pattern,
   returning a seq of locs. Which primary Pattern is walked will be chosen
   in a pseudorandom, deterministic fashion (see how the root node is
   constructed in `pattern-zip`)."
  [loc]
  (->> loc
       (iterate z/next)
       (take-while (complement z/end?))
       rest              ; cut the root node off the top
       (filter z/node))) ; empty seq nodes will be `nil`, so filter them out

;; This `registration-seq-instance` public function exists in order to test
;; pattern zip creation and registration seq generation; it's not used in
;; production (but `registration-seq-instance*` is).

(s/fdef registration-seq-instance
  :args (s/cat :type-iri-map map? #_::type-iri-map
               :alignment map? ; TODO: Better spec
               :seed number?)
  :ret (s/every ::registration-map))

(defn- registration-seq-instance*
  [type-iri-map pattern-zip rng]
  (let [registration   (random/rand-uuid rng)
        node->template (fn [node-id]
                         (get-in type-iri-map ["StatementTemplate" node-id]))
        node->pattern  (fn [node-id]
                         (get-in type-iri-map ["Pattern" node-id]))
        loc->reg-map   (fn [loc]
                         (when-some [template (node->template (z/node loc))]
                           {:registration registration
                            :template     template
                            :seed         (random/rand-long rng)
                             ;; Every previous node that is a Pattern
                            :pattern-ancestors (->> loc
                                                    z/path
                                                    rest
                                                    (keep node->pattern)
                                                    vec)}))]
    ;; Do one deterministc walk, then get the Statement Templates
    (->> pattern-zip walk-once (keep loc->reg-map))))

(defn registration-seq-instance
  "Given `seed`, `alignment` and a `type-iri-map`, return a sequence
   of registration maps. This is similar to `registration-seq` except that
   it will generate one Pattern's seq, instead of continuing infinitely."
  [type-iri-map alignment seed]
  (let [rng (random/seed-rng seed)
        pattern-zip (pattern-zip type-iri-map alignment rng)]
    (registration-seq-instance* type-iri-map pattern-zip rng)))

(s/fdef registration-seq
  :args (s/cat :type-iri-map map? #_::type-iri-map
               :alignment map? ; TODO: Better spec
               :seed number?)
  :ret (s/every ::registration-map :kind #(instance? clojure.lang.LazySeq %)))

(defn- registration-seq*
  [type-iri-map pattern-zip rng]
  (lazy-seq
   (concat (registration-seq-instance* type-iri-map pattern-zip rng)
           (registration-seq* type-iri-map pattern-zip rng))))

;; TODO: Configurable keep-max arg
(defn registration-seq
  "Given `seed`, `alignment` and a `type-iri-map`, return an infinite lazy seq
   of registration maps with the following properties:
   - `:registration` is a UUID string that will be the Statement's Context
     Registration property
   - `:template` is the Statement Template used to generate the Statement
   - `:seed` is a derived seed for generating the Statement
   - `:pattern-ancestors` is the vector of Patterns leading up to the Statement
     Template in the current Pattern path.
   
   Each registration map will be able to generate a single Statement."
  [type-iri-map alignment seed]
  (let [rng (random/seed-rng seed)
        pattern-zip (pattern-zip type-iri-map alignment rng)]
    (registration-seq* type-iri-map pattern-zip rng)))
