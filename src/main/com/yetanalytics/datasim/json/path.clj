(ns com.yetanalytics.datasim.json.path
  (:require [blancas.kern.core :as k]
            [blancas.kern.lexer.basic :as kl]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.json :as json]
            [com.yetanalytics.datasim.json.zip :as jzip]
            [clojure.zip :as z]
            [clojure.math.combinatorics :as combo]))

(s/def ::root
  #{'$})

(def root
  (k/<$>
   (constantly '$)
   (k/sym* \$)))

(s/def ::wildcard
  #{'*})

(def wildcard
  (k/<$>
   (constantly '*)
   (k/sym* \*)))

(s/def ::keyset
  (s/or :keys (s/every string?
                       :type set?
                       :into #{}
                       :min-count 1)
        :indices (s/every int?
                          :type set?
                          :into #{}
                          :min-count 1)))

(def index
  (k/<$>
   (fn [^String x]
     (Long/parseLong x))
   (k/<+>
    (k/optional (k/sym* \-))
    kl/dec-lit)))

(s/def :range/start
  int?)

(s/def :range/end
  int?)

(s/def :range/step
  int?)

(s/def :range/bounded? ;; was this range bounded, or does it use a MAX_VALUE?
  boolean?)



(defrecord RangeSpec [start end step bounded?])


(s/fdef range-spec?
  :args (s/cat :item any?)
  :ret boolean?)

(defn range-spec?
  [item]
  (instance? RangeSpec item))

(s/def ::range
  (s/keys :req-un [:range/start
                   :range/end
                   :range/step
                   :range/bounded?]))

(def index-range
  (k/bind [start (k/option 0
                           index)
           _ kl/colon
           end (k/option Long/MAX_VALUE
                         index)
           _ (k/optional kl/colon)
           step (k/option 1
                          index)]
          (k/return
           (->RangeSpec (if (number? start)
                          start
                          (Long/parseLong start))
                        (if (number? end)
                          end
                          (Long/parseLong end))

                        (if (number? step)
                          step
                          (Long/parseLong step))
                        ;; if the option is used, we're unbounded
                        (number? end)))))

(defn escaped-by
  [c & [charset-p]]
  (k/>> (k/sym* c)
        (or charset-p k/any-char)))

(def escaped-char
  (escaped-by (char 92)))

(def safe-char
  (k/none-of* (str
               ;; double-quote
               (char 34)
               ;; single quote
               (char 39)
               ;; escape char
               (char 92))))

(def json-key
  (k/<+>
   (k/many
    (k/<|>
     escaped-char
     safe-char))))

(def json-key-lit
  (k/<+>
   (k/between (k/sym* \') json-key)))

(defn union-of1
  "A comma separated union of at least 1 of something.
   Returns a set."
  [p]
  (k/<$>
   set
   (k/sep-by1 (k/sym* \,) p)))

(def child-normal
  "Normal bracket-style child"
  (kl/brackets
   (k/<|>
    wildcard
    (k/<:> index-range)
    (union-of1 index)
    (union-of1 json-key-lit))))

(def child-dot
  "Stupid dot syntax"
  (k/>>
   kl/dot
   (k/<|>
    ;; normal key
    (k/<$>
     (partial hash-set)
     (k/<+> (k/many1 k/alpha-num)))
    ;; dot wildcard
    wildcard
    ;; double-dot wildcard
    (k/<$>
     (constantly '*)
     (k/look-ahead
      (k/>> kl/dot
            (k/many1 k/alpha-num)))))))


(def json-path
  (k/>> root
        (k/many (k/<|>
                 child-normal
                 child-dot))))

(s/def ::json-path
  (s/every (s/or :keyset ::keyset
                 :wildcard ::wildcard
                 :range ::range)))

(s/fdef parse
  :args (s/cat :path string?)
  :ret ::json-path)

(defn parse
  "Given a JSON-path, parse it into data"
  [path]
  (:value (k/parse json-path
                   path)))

(s/fdef satisfied
  :args (s/cat :json-path ::json-path
               :key-path ::jzip/key-path)
  :ret (s/nilable ::json-path))

(defn satisfied
  "Given a json path and a key path, return nil if they diverge, or if they partially
  match return a seq of the covered pattern"
  [path key-path]
  (when-let [partial-sat
             (map first
                  (take-while
                   (fn [[p pk]]
                     (cond
                       (= p '*) true
                       (set? p) (contains? p pk)
                       :else (let [{:keys [start
                                           end
                                           step]} p]
                               (some (partial = pk)
                                     (range start end step)))))
                   (map vector
                        path
                        key-path)))]
    (cond

      ;; if there is more left in key path this is a failure
      (not-empty
       (drop (count partial-sat)
             key-path))
      nil
      ;; totally satisfied, we can keep it
      (= path partial-sat)
      path
      ;; partially satisfied
      :else partial-sat)))

(s/fdef select-deep
  :args (s/cat :path ::json-path
               :json ::json/any)
  :ret ::json/any)

(defn select-deep
  "Given json data and a parsed path, return only the selections with everything
  else pruned"
  [json path]
  (loop [loc (jzip/json-zip json)]
    (if (z/end? loc)
      (z/root loc)
      (if (jzip/internal? loc)
        (recur (z/next loc))
        (let [key-path (jzip/k-path loc)]
          (if-let [sat (satisfied path key-path)]
            ;; if we have satisfied at least some of the spec, we
            ;; want to keep going
            (recur (z/next loc))
            ;; if it doesn't match, kill this or any internal nodes
            ;; leading to it.
            (recur (jzip/prune loc))))))))

(s/fdef select
  :args (s/cat :path ::json-path
               :json ::json/any)
  :ret (s/every ::json/any))



(defn select
  "Given json data and a parsed path, return a selection vector."
  [json path]
  (loop [loc (jzip/json-zip json)
         selection []]
    (if (z/end? loc)
      selection
      (if (jzip/internal? loc)
        (recur (z/next loc)
               selection)
        (let [key-path (jzip/k-path loc)]
          (if-let [sat (satisfied path key-path)]
            (if (= sat path)
              ;; if we have totally satisfied the spec we can keep and prune
              (recur (z/next (jzip/prune loc))
                     (conj selection (z/node loc)))
              ;; if we have partially satisfied the spec we want to keep going
              (recur (z/next loc)
                     selection))
            ;; if it doesn't match, kill this or any internal nodes
            ;; leading to it, reducing our search space
            (recur (jzip/prune loc)
                   selection)))))))

(s/def :excise/prune-empty?
  boolean?)

(s/fdef excise
  :args (s/cat :path ::json-path
               :json ::json/any
               :options (s/keys* :opt-un [:excise/prune-empty?]))
  :ret (s/every ::json/any)
  :fn (fn [{:keys [ret]
            {path :path
             json :json} :args}]
        (empty? (select json path))))

(defn excise
  "Given json data and a parsed path, return the data without the selection, and
  any empty container.
  If :prune-empty? is true, will remove empty arrays and maps"
  [json path & {:keys [prune-empty?]}]
  (loop [loc (jzip/json-zip json)]
    (if (z/end? loc)
      (z/root loc)
      (cond
        (jzip/internal? loc)
        (recur (z/next loc))

        (and prune-empty?
             (let [node (z/node loc)]
               (and (coll? node)
                    (empty? node))))
        (recur (jzip/prune loc))
        :else
        (let [key-path (jzip/k-path loc)
              sat (satisfied path key-path)]
          (if (= sat path)
            (recur (jzip/prune loc))
            (recur (z/next loc))))))))


(s/def :enumerate/limit
  number?)

(s/fdef enumerate
  :args (s/cat :path ::json-path
               :options (s/keys* :opt-un [:enumerate/limit]))
  :ret (s/every ::jzip/key-path))

(defn enumerate
  "Given a json path, return a lazy seq of concrete key paths. wildcards/ranges
  will be enumerated up to :limit, which defaults to 10"
  [path & {:keys [limit]
           :or {limit 10}}]
  (apply combo/cartesian-product
         (map
          (fn [element]
            (cond
              (set? element)
              element

              (= '* element)
              (range limit)
              ;; otherwise, itsa range spec
              :else
              (let [{:keys [start end step]} element]
                (take limit
                      (range start end step)))))
          path)))
