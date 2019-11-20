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


(defn pattern-gen*
  [profile-map
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
        (get profile-map iri)]
    (case type
      "Pattern"
      (cond
        alternates
        (gen/one-of (mapv (partial
                           pattern-gen*
                           profile-map)
                          alternates))
        optional
        (gen/one-of [(gen/return [])
                     (pattern-gen*
                      profile-map
                      (:id optional))])
        oneOrMore
        (gen/vector (pattern-gen*
                     profile-map
                     (:id oneOrMore))
                    1 100)
        sequence
        (apply gen/tuple (mapv (partial
                                pattern-gen*
                                profile-map)
                               sequence))
        zeroOrMore
        (gen/vector (pattern-gen*
                     profile-map
                     (:id zeroOrMore))))
      "StatementTemplate"
      (gen/return iri))))

(defn pattern-gen
  "Generator that, given a profile and a primary pattern iri, returns a valid
  walk of the pattern."
  [profile iri]
  (let [profile-map (iri-map profile)
        {:keys [primary
                type]} (get profile-map iri)]
    (assert (= type "Pattern") "IRI must be for a pattern")
    (assert primary "Can only generate a primary pattern.")
    (gen/fmap flatten
              (pattern-gen* profile-map
                            iri))))



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

  (gen/generate
   (pattern-gen
    p
    "https://w3id.org/xapi/cmi5#toplevel")
   4)


  )
