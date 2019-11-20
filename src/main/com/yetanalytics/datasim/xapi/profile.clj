(ns com.yetanalytics.datasim.xapi.profile
  "Understanding elements of xAPI profiles"
  (:require [clojure.test.check.generators :as gen]))

(defn all-iris
  "Given a profile, return all IRIs"
  [profile]
  (for [[_ things] (select-keys profile [:concepts :patterns :templates])
        {:keys [id] :as thing} things]
    id))


(defn iri-map
  "Given a profile, return all IRIs mapped to what they reference"
  [profile]
  (into {}
        (for [[_ things] (select-keys profile [:concepts :patterns :templates])
              {:keys [id] :as thing} things]
          [id thing])))

(defn align-likelihood
  [actor-alignment
   iri
   & {:keys [base]
      :or {base 1.0}}]
  (max 0.01
       (+ base
          (get actor-alignment iri 0.0))))

(defn pattern-gen*
  [profile-map
   actor-alignment
   iri]
  (let [{:keys [id
                type
                primary

                alternates
                optional
                oneOrMore
                sequence
                zeroOrMore
                ]
         {label :en}:prefLabel}
        ;; We currently assume everything in one profile, this needs to change
        (get profile-map iri)]
    (case type
      "Pattern"
      (cond
        alternates
        (gen/bind
         (gen/shuffle alternates)
         (fn [alternates']
           (gen/frequency (mapv
                           (fn [alt-iri]
                             [(align-likelihood
                               actor-alignment
                               alt-iri)
                              (pattern-gen*
                               profile-map
                               actor-alignment
                               alt-iri)])
                           alternates'))))

        ;; TODO: figure out more rigourous effect
        optional
        (gen/frequency [[1.0
                         (gen/return [])]
                        [(align-likelihood
                          actor-alignment
                          (:id optional))
                         (pattern-gen*
                          profile-map
                          actor-alignment
                          (:id optional))]])
        ;; TODO: Figure out effect, if any
        oneOrMore
        (gen/vector (pattern-gen*
                     profile-map
                     actor-alignment
                     (:id oneOrMore))
                    1 100)
        sequence
        (apply gen/tuple (mapv (partial
                                pattern-gen*
                                profile-map
                                actor-alignment)
                               sequence))
        zeroOrMore
        (gen/frequency
         [[1.0 (gen/return [])]
          [(align-likelihood
            actor-alignment
            (:id zeroOrMore))
           (gen/vector (pattern-gen*
                        profile-map
                        actor-alignment
                        (:id zeroOrMore))
                       1 100)]]))
      "StatementTemplate"
      (gen/return iri))))

(defn pattern-gen
  "Generator that, given a profile and a primary pattern iri, returns a valid
  walk of the pattern. If IRI is omitted, it will constuct a gen that picks
  primary patterns based on alignment."
  [profile
   actor-alignment
   & [iri]]
  (let [profile-map (iri-map profile)]
    (gen/bind
     (if iri
       (gen/return iri)
       (gen/frequency
        (mapv
         (fn [id]
           [(align-likelihood actor-alignment id)
            (gen/return id)])
         (keep
          (fn [[id {:keys [type primary]}]]
            (when (and (= type "Pattern") primary)
              id))
          profile-map))))
     (fn [iri]
       (let [{:keys [primary
                     type]} (get profile-map iri)]
         (assert (= type "Pattern") "IRI must be for a pattern")
         (assert primary "Can only generate a primary pattern.")
         (gen/fmap flatten
                   (pattern-gen* profile-map
                                 actor-alignment
                                 iri)))))))



(comment
  (require '[com.yetanalytics.datasim.input :as i])

  (def p
    (i/from-location
     :profile :json "dev-resources/profiles/cmi5/fixed.json"))

  (clojure.pprint/pprint (keep (fn [[id {:keys [type primary]}]]
                                   (when (and (= type "Pattern")
                                              primary)
                                     id))
                                 (iri-map p)))

  (time (first (gen/generate
   (pattern-gen
    p ;; profile
    {} #_{"https://w3id.org/xapi/cmi5#satisfied" -1.0} ;; alignment
    ;; "https://w3id.org/xapi/cmi5#toplevel" ;; entry pattern
    )
   3 ;; gen 'size'
   1234
   )))






  )
