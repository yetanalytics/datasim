(ns user
  (:require [clojure.repl :refer [source doc apropos]]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as cset]
            [clojure.string :as s]
            [com.yetanalytics.datasim.runtime :as runtime]
            [com.yetanalytics.datasim.json.path :as json-path]
            [com.yetanalytics.datasim.sim :as sim]
            [com.yetanalytics.datasim.xapi.activity :as activity]
            [com.yetanalytics.datasim.xapi.profile.template.rule :as rule]
            [com.yetanalytics.datasim.input :as input]
            [clojure.java.io :as io]
            [clojure.core.match :refer [match]]
            [com.yetanalytics.datasim.util.sequence :as su]
            [cheshire.core :as json]))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dev tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pp-json-to-file
  "helper for pprinting JSON `data` to `path`"
  [path data]
  (json/generate-stream data (io/writer (io/as-file path)) {:pretty {:indent-arrays? true}}))

(defn clean-external-profiles
  [profile target-id-set]
  (-> profile
      (dissoc :templates :patterns)
      (update :concepts (fn [concepts] (filterv (fn [{:keys [id]}] (cset/subset? #{id} target-id-set)) concepts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATASIM input - profiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def test-profile
  (input/from-location
   :profile
   :json
   "./dev-resources/profiles/tccc/cuf_hc_video_and_asm_student_survey_profile.jsonld"))

(def video-profile
  (clean-external-profiles
   (input/from-location
    :profile
    :json
    "./dev-resources/profiles/video/profile.jsonld")
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
    "./dev-resources/profiles/activity_streams/profile.jsonld")
   #{"http://activitystrea.ms/start"
     "http://activitystrea.ms/submit"}))

(def acrossx-profile
  (clean-external-profiles
   (input/from-location
    :profile
    :json
    "./dev-resources/profiles/acrossx/profile.jsonld")
   #{"https://w3id.org/xapi/acrossx/activities/page"}))

(def tincan-profile
  (clean-external-profiles
   (input/from-location
    :profile
    :json
    "./dev-resources/profiles/tincan/profile.jsonld")
   #{"http://id.tincanapi.com/verb/skipped"}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATASIM input - rest
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def test-personae
  (input/from-location
   :personae
   :json
   "./dev-resources/personae/tccc_dev.json"))

(def test-alignments
  (input/from-location
   :alignments
   :json
   "./dev-resources/alignments/tccc_dev.json"))

(def test-parameters
  (input/from-location
   :parameters
   :json
   "./dev-resources/parameters/tccc_dev.json"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATASIM input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def test-input
  {:profiles [test-profile video-profile activity-stream-profile acrossx-profile tincan-profile]
   :personae test-personae
   :alignments test-alignments
   :parameters test-parameters})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATASIM run
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
