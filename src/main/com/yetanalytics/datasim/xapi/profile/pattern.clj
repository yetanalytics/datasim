(ns com.yetanalytics.datasim.xapi.profile.pattern
  "Creation of `pattern-walk-fn` for Profile compilation."
  (:require [clojure.spec.alpha :as s]
            [clojure.zip        :as z]
            [com.yetanalytics.pan.objects.template     :as template]
            [com.yetanalytics.pan.objects.pattern      :as pattern]
            [com.yetanalytics.datasim.input.alignments :as alignment]
            [com.yetanalytics.datasim.math.random      :as random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::repeat-max
  pos-int?)

(s/def ::pattern-ancestors
  (s/every ::pattern/pattern))

;; `pattern-walk-fn` has the arglist `[alignment rng {:keys [repeat-max]}]`
;; and returns a template w/ `:pattern-ancestors` metadata
(s/def ::pattern-walk-fn
  (s/fspec
   :args (s/cat :alignment ::alignment/alignment
                :rng ::random/rng
                :kwargs (s/keys* :opt-un [::repeat-max]))
   :ret (s/every (s/and ::template/template
                        (s/conformer meta)
                        (s/keys :req-un [::pattern-ancestors])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern Walker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- pattern-zipper
  "Create a zipper over the Patterns and Statement Templates found in
   `type-iri-map`. A special `::root` sentinel Pattern is created as an
   alternates Pattern of all the primary Patterns in the profiles.
   The zipper can then be walked; traversal will be done in a deterministic,
   pseudorandom fashion, in which `rng` and `alignment` is used to choose
   the children of each node in the zipper."
  [type-iri-map alignment rng repeat-max]
  (let [temp-iri-map    (get type-iri-map "StatementTemplate")
        pat-iri-map     (get type-iri-map "Pattern")
        primary-pat-ids (->> pat-iri-map vals (filter :primary) (mapv :id))
        root-pattern    {:id         ::root
                         :type       "Pattern"
                         :alternates primary-pat-ids}
        pat-iri-map*    (assoc pat-iri-map ::root root-pattern)]
    (-> (z/zipper
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
         ::root)
        (vary-meta assoc
                   ::template-map temp-iri-map
                   ::pattern-map  pat-iri-map))))

(defn- pattern-loc->template
  [{template-m ::template-map pattern-m ::pattern-map} pattern-loc]
  (let [node->template #(get template-m %)
        node->pattern  #(get pattern-m %)]
    (when-some [template (->> pattern-loc z/node node->template)]
      (vary-meta template
                 assoc
                 :pattern-ancestors
                 (->> pattern-loc z/path rest (keep node->pattern) vec)))))

(defn- walk-pattern-zipper
  "From the root of `pattern-zip`, perform a single walk of a primary Pattern,
   returning a sequence of Templates. Which primary Pattern is walked will be
   chosen in a pseudorandom, deterministic fashion (see how the root node is
   constructed in `pattern-zipper`)."
  [pattern-zip]
  (->> pattern-zip
       (iterate z/next)
       (take-while (complement z/end?))
       rest            ; cut the root node off the top
       (filter z/node) ; empty seq nodes will be `nil`, so filter them out
       (keep (partial pattern-loc->template (meta pattern-zip)))))

;; TODO: Bring in type-iri-map spec using :as-alias in Clojure 1.11
(s/fdef create-pattern-walk-fn
  :args (s/cat :type-iri-map map?)
  :ret ::pattern-walk-fn)

(defn create-pattern-walk-fn
  "Return a function that, when called with the args `alignment rng
   & {:keys [repeat-max]}`, returns a lazy sequence of Statement Templates
   that have `:pattern-ancestors` metadata."
  [type-iri-map]
  (fn [alignment rng & {:keys [repeat-max]
                        :or {repeat-max 5}}]
    (walk-pattern-zipper
     (pattern-zipper type-iri-map alignment rng repeat-max))))
