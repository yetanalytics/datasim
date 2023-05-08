(ns com.yetanalytics.datasim.test-fixtures
  "Fixtures for input items, i.e. profiles, personae, alignments, and
   parameters."
  (:require [com.yetanalytics.datasim.input :as input]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filepath Fixtures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Profiles

(def minimal-profile-filepath
  "dev-resources/profiles/minimal.jsonld")
(def cmi5-profile-filepath
  "dev-resources/profiles/cmi5/fixed.json")
(def video-profile-filepath
  "dev-resources/profiles/video/profile.jsonld")
(def acrossx-profile-filepath*
  "dev-resources/profiles/acrossx/profile.jsonld")
(def acrossx-profile-filepath
  "dev-resources/profiles/acrossx/fixed.jsonld")
(def activity-profile-filepath*
  "dev-resources/profiles/activity_streams/profile.jsonld")
(def activity-profile-filepath
  "dev-resources/profiles/activity_streams/fixed.jsonld")
(def mom-profile-filepath
  "dev-resources/profiles/tla/mom.jsonld")
(def referential-profile-filepath
  "dev-resources/profiles/referential.jsonld")
(def tc3-profile-filepath
  "dev-resources/profiles/tccc/cuf_hc_video_and_asm_student_survey_profile.jsonld")

;; Personae

(def simple-personae-filepath
  "dev-resources/personae/simple.json")
(def tc3-personae-filepath
  "dev-resources/personae/tccc_dev.json")

;; Alignments

(def simple-alignments-filepath
  "dev-resources/alignments/simple.json")
(def overrides-alignments-filepath
  "dev-resources/alignments/simple_with_overrides.json")
(def tc3-alignments-filepath
  "dev-resources/alignments/tccc_dev.json")

;; Parameters

(def simple-parameters-filepath
  "dev-resources/parameters/simple.json")

;; Combined Input

(def simple-input-filepath
  "dev-resources/input/simple.json")
(def overrides-input-filepath
  "dev-resources/alignments/simple_with_overrides.json")
(def mom-input-filepath
  "dev-resources/input/mom64.json")

;; Miscellaneous

(def simple-statement-filepath
  "dev-resources/xapi/statements/simple.json")
(def long-statement-filepath
  "dev-resources/xapi/statements/long.json")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON Fixtures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Profiles

(def minimal-profile
  (input/from-location :profile :json minimal-profile-filepath))

(def cmi5-profile
  (input/from-location :profile :json cmi5-profile-filepath))

(def video-profile
  (input/from-location :profile :json video-profile-filepath))

(def acrossx-profile* 
  (input/from-location :profile :json acrossx-profile-filepath*))

(def acrossx-profile
  (input/from-location :profile :json acrossx-profile-filepath))

(def activity-profile*
  (input/from-location :profile :json activity-profile-filepath*))

(def activity-profile
  (input/from-location :profile :json activity-profile-filepath))

(def mom-profile
  (input/from-location :profile :json mom-profile-filepath))

(def referential-profile
  (input/from-location :profile :json referential-profile-filepath))

(def tc3-profile
  (input/from-location :profile :json tc3-profile-filepath))

;; Personae

(def simple-personae
  (input/from-location :personae :json simple-personae-filepath))

(def tc3-personae
  (input/from-location :personae :json tc3-personae-filepath))

;; Alignments

(def override-alignments
  (input/from-location :alignments :json overrides-input-filepath))

;; Combined Input

(def simple-input
  (input/from-location :input :json simple-input-filepath))
