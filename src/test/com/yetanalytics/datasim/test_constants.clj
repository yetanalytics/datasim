(ns com.yetanalytics.datasim.test-constants
  "Constants for input items, i.e. profiles, personae, models, and
   parameters."
  (:require [com.yetanalytics.datasim.input :as input]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filepath Names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Profiles

(def minimal-profile-filepath
  "dev-resources/profiles/minimal.jsonld")
(def no-concept-profile-filepath
  "dev-resources/profiles/no_concept.jsonld")
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

;; Models

(def simple-models-filepath
  "dev-resources/models/simple.json")
(def overrides-models-filepath
  "dev-resources/models/simple_with_overrides.json")
(def temporal-models-filepath
  "dev-resources/models/simple_with_temporal.json")
(def tc3-models-filepath
  "dev-resources/models/tccc_dev.json")

;; Parameters

(def simple-parameters-filepath
  "dev-resources/parameters/simple.json")

;; Combined Input

(def simple-input-filepath
  "dev-resources/input/simple.json")

;; Miscellaneous

(def simple-statement-filepath
  "dev-resources/xapi/statements/simple.json")
(def long-statement-filepath
  "dev-resources/xapi/statements/long.json")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON Datas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Profiles

(def minimal-profile
  (input/from-location :profile :json minimal-profile-filepath))

(def no-concept-profile
  (input/from-location :profile :json no-concept-profile-filepath))

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

;; Models

(def overrides-models
  (input/from-location :models :json overrides-models-filepath))

(def temporal-models
  (input/from-location :models :json temporal-models-filepath))

;; Combined Input

(def simple-input
  (input/from-location :input :json simple-input-filepath))
