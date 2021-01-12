(ns com.yetanalytics.datasim.input.alignments
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.datasim.protocols :as p]
            [com.yetanalytics.pan.objects.profile :as profile]
            [com.yetanalytics.datasim.iri :as iri]
            [com.yetanalytics.datasim.xapi :as xapi]))

;;Alignment: Map of Component and Weight

(s/def ::component
  iri/iri-spec)

(s/def ::weight
  (s/double-in :min -1.0 :max 1.0
               :infinite? false
               :NaN? false))

(s/def ::alignment
  (s/keys :req-un [::component
                   ::weight]))

;;Actor-Alignment: Map of Actor to collection of Alignments

(s/def ::id
  ::xapi/agent-id)

(s/def ::type
  #{"Agent" "Group" "Role"})

(s/def ::alignments
  (s/every ::alignment))

(s/def ::actor-alignment
  (s/keys :req-un [::id
                   ::type
                   ::alignments]))

;;Alignment-vector: Collection of Actor-Alignment

(s/def ::alignment-vector
  (s/every ::actor-alignment))

;;Alignment input
(s/def ::alignments-input
  (s/keys :req-un [::alignment-vector]))

(defrecord Alignments [alignment-vector]
  p/FromInput
  (validate [this]
    (s/explain-data ::alignments-input
                    this))
  p/JSONRepresentable
  (read-key-fn [this k]
    (keyword nil (name k)))
  (read-body-fn [this json-result]
    (map->Alignments
     {:alignment-vector json-result}))
  (write-key-fn [this k]
    (name k))
  (write-body-fn [this]
    alignment-vector))


(comment



  (def alignment1 {:component "http://www.whateveer.com/activity1"
                   :weight 0.9})

  (def alignment2 {:component "http://www.whateveer.com/activity2"
                   :weight 0.8})

  (def actor-alignment1 {
                         :id "mbox::mailto:cliff@yetanalytics.com"
                         :type "Agent"
                         :alignments [alignment1 alignment2]
                         })

  (def actor-alignment2 {
                         :id "mbox::mailto:cliff1@yetanalytics.com"
                         :type "Agent"
                         :alignments [alignment1 alignment2]
                         })

  (def alignments-example [actor-alignment1 actor-alignment2])


  (s/explain-data ::actor-alignments alignments-example)

  (satisfies? p/FromInput alignments-example)


  (s/valid? ::alignment alignment1)
  (s/valid? ::alignment alignment2)
  (s/valid? ::actor-alignment actor-alignment1)
  (s/valid? ::actor-alignment actor-alignment2)
  (s/valid? ::actor-alignments alignments-example)



  )
