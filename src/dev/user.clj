(ns user
  (:require [clojure.repl :refer [source doc apropos]]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as cset]
            [com.yetanalytics.datasim.runtime :as runtime]
            [com.yetanalytics.datasim.sim :as sim]
            [com.yetanalytics.datasim.xapi.activity :as activity]
            [com.yetanalytics.datasim.input :as input]))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dev tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clean-external-profiles
  [profile target-id-set]
  (-> profile
      (dissoc :templates :patterns)
      (update :concepts (fn [concepts] (filterv (fn [{:keys [id]}] (cset/subset? #{id} target-id-set)) concepts)))))

;; FIXME: move files to datasim

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATASIM input - profiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def test-profile
  ;; nil
  (input/from-location
   :profile
   :json
   "/Users/williamhoyt/projects/profile-tools/resources/tccc_cuf_hc.jsonld"))

(def video-profile
  (clean-external-profiles
   (input/from-location
    :profile
    :json
    "/Users/williamhoyt/projects/profile-tools/resources/video.jsonld")
   #{"https://w3id.org/xapi/video/verbs/paused"
     "https://w3id.org/xapi/video/verbs/played"
     "https://w3id.org/xapi/video/verbs/seeked"
     "https://w3id.org/xapi/video/activity-type/video"
     "https://w3id.org/xapi/video/extensions/length"
     "https://w3id.org/xapi/video/extensions/volume"}))

(def activity-stream-profile
  (clean-external-profiles
   (input/from-location
    :profile
    :json
    "/Users/williamhoyt/projects/profile-tools/resources/activity_stream_profile.jsonld")
   #{"http://activitystrea.ms/start"
     "http://activitystrea.ms/submit"}))

(def acrossx-profile
  (clean-external-profiles
   (input/from-location
    :profile
    :json
    "/Users/williamhoyt/projects/profile-tools/resources/acrossx.jsonld")
   #{"https://w3id.org/xapi/acrossx/activities/page"}))

(def tincan-profile
  (clean-external-profiles
   (input/from-location
    :profile
    :json
    "/Users/williamhoyt/projects/profile-tools/resources/tincan.jsonld")
   #{"http://id.tincanapi.com/verb/skipped"}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATASIM input - rest
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def test-personae
  ;; nil
  (input/from-location
   :personae
   :json
   "/Users/williamhoyt/projects/profile-tools/resources/datasim/persona.json"))

(def test-alignments
  ;; nil
  (input/from-location
   :alignments
   :json
   "/Users/williamhoyt/projects/profile-tools/resources/datasim/tccc_test_alignments.json"))

(def test-parameters
  ;; nil
  (input/from-location
   :parameters
   :json
   "/Users/williamhoyt/projects/profile-tools/resources/datasim/test_parameters.json"))

(def test-input
  ;; nil
  {:profiles [test-profile video-profile activity-stream-profile acrossx-profile tincan-profile]
   :personae test-personae
   :alignments test-alignments
   :parameters test-parameters})

(defn run-sim!
  []
  (sim/build-skeleton test-input))

(comment

  (def test-cosmos
    (activity/derive-cosmos
     test-input
     (-> test-input :parameters :seed)))

  (clojure.pprint/pprint test-cosmos)

  (def sim-run (run-sim!))

  

  (->> sim-run
       first
       second
       (sort-by (fn [{:strs [timestamp]}] timestamp))
       clojure.pprint/pprint)

  )
