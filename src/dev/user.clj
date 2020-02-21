(ns user
  (:require [clojure.repl :refer [source doc apropos]]
            [clojure.pprint :refer [pprint]]
            [com.yetanalytics.datasim.runtime :as runtime]
            [com.yetanalytics.datasim.sim :as sim]
            [com.yetanalytics.datasim.input :as input]))

(set! *warn-on-reflection* true)

(def test-profile
  ;; nil
  (input/from-location
   :profile
   :json
   "/Users/williamhoyt/projects/profile-tools/resources/tccc_cuf_hc.jsonld"))

(def video-profile
  ;; nil
  (dissoc
   (input/from-location
    :profile
    :json
    "/Users/williamhoyt/projects/profile-tools/resources/video.jsonld")
   :templates
   :patterns))

(def activity-stream-profile
  (input/from-location
   :profile
   :json
   "/Users/williamhoyt/projects/profile-tools/resources/activity_stream_profile.jsonld"))

(def acrossx-profile
  (input/from-location
   :profile
   :json
   "/Users/williamhoyt/projects/profile-tools/resources/acrossx.jsonld"))

(def tincan-profile
  (input/from-location
   :profile
   :json
   "/Users/williamhoyt/projects/profile-tools/resources/tincan.jsonld"))

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

  (def sim-run (run-sim!))

  

  (->> sim-run
       first
       second
       (sort-by (fn [{:strs [timestamp]}] timestamp))
       clojure.pprint/pprint)

  )
