(ns com.yetanalytics.datasim.xapi.registration
  "Registration map sequence generation. Each registration map is to be used
   for Statement generation."
  (:require [clojure.spec.alpha :as s]
            [xapi-schema.spec] ; for registration spec
            [com.yetanalytics.datasim.model                 :as model]
            [com.yetanalytics.datasim.math.random           :as random]
            [com.yetanalytics.datasim.xapi.profile          :as profile]
            [com.yetanalytics.datasim.xapi.profile.template :as template]
            [com.yetanalytics.datasim.xapi.profile.pattern  :as pattern]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::registration :context/registration)

(s/def ::seed int?)

(s/def ::registration-map
  (s/keys :req-un [::registration
                   ::seed
                   ::template/template
                   ::pattern/pattern-ancestors]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef registration-seq
  :args (s/cat :type-iri-map ::profile/type-iri-map
               :alignments   ::model/alignments
               :seed         number?)
  :ret (s/every ::registration-map :kind #(instance? clojure.lang.LazySeq %)))

(defn- registration-seq**
  [pattern-walk-fn alignments rng]
  (let [registration (random/rand-uuid rng)]
    (->> (pattern-walk-fn alignments rng)
         (map (fn [template]
                (merge
                 {:registration      registration
                  :seed              (random/rand-unbound-int rng)
                  :template          template}
                 (select-keys (meta template)
                              [:pattern-ancestors :bounds :period])))))))

(defn- registration-seq*
  [pattern-walk-fn alignments rng]
  (lazy-seq
   (concat (registration-seq** pattern-walk-fn alignments rng)
           (registration-seq* pattern-walk-fn alignments rng))))

;; TODO: Configurable keep-max arg
(defn registration-seq
  "Given `seed`, `alignments` and a `pattern-walk-fn`, return an infinite lazy
   seq of registration maps with the following properties:
   - `:registration` is a UUID string that will be the Statement's Context
     Registration property
   - `:template` is the Statement Template used to generate the Statement
   - `:seed` is a derived seed for generating the Statement
   - `:pattern-ancestors` is the vector of Patterns leading up to the Statement
     Template in the current Pattern path.
   
   Each registration map will be able to generate a single Statement."
  [{:keys [pattern-walk-fn]} alignments seed]
  (let [rng (random/seed-rng seed)]
    (registration-seq* pattern-walk-fn alignments rng)))
