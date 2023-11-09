(ns com.yetanalytics.datasim.cli.input
  "CLI options and functions for sim inputs (including input validation)."
  (:require [com.yetanalytics.datasim.cli.util    :as u]
            [com.yetanalytics.datasim.input       :as input]
            [com.yetanalytics.datasim.util.random :as random]
            [com.yetanalytics.datasim.util.errors :as errors]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLI Input Options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def profiles-desc
  "The location of an xAPI profile, can be used multiple times.")

(def personae-array-desc
  "The location of an Actor Personae document indicating the actors in the sim.")

(def models-desc
  "The location of an Persona Model document, to describe alignments and overrides for the personae.")

(def parameters-desc
  "The location of simulation parameters document. Uses the current time and timezone as defaults if they are not present.")

(def input-desc
  "The location of a JSON file containing a combined simulation input spec.")

(def validated-input-desc
  "The location of the validated input to be produced. If not provided, the validated input will be printed to stdout instead.")

;; NOTE: For the `validate-input` subcommand, we skip tools.cli validation and
;; do more in-depth validation involving combined inputs.
(def validate-input-options
  [["-p" "--profile URI" "xAPI Profile Location"
    :id       :profiles
    :desc     profiles-desc
    :parse-fn (partial input/from-location :profile :json)
    :assoc-fn u/conj-input]
   ["-a" "--actor-personae URI" "Actor Personae Location"
    :id       :personae-array
    :desc     personae-array-desc
    :parse-fn (partial input/from-location :personae :json)
    :assoc-fn u/conj-input]
   ["-m" "--models URI" "Persona Model Location"
    :id       :models
    :desc     models-desc
    :parse-fn (partial input/from-location :models :json)]
   ["-o" "--parameters URI" "Parameters Location"
    :id       :parameters
    :desc     parameters-desc
    :parse-fn (partial input/from-location :parameters :json)]
   ["-i" "--input URI" "Pre-validated input location"
    :id       :input
    :desc     input-desc
    :parse-fn (partial input/from-location :input :json)]
   ["-v" "--validated-input URI" "Validated combined input location"
    :id   :validated-input
    :desc validated-input-desc]])

(def input-options
  [["-p" "--profile URI" "xAPI Profile Location"
    :id       :profiles
    :desc     profiles-desc
    :parse-fn (partial input/from-location :profile :json)
    :validate [(partial input/validate-throw :profile)
               "Failed to validate profile."]
    :assoc-fn u/conj-input]
   ["-a" "--actor-personae URI" "Actor Personae Location"
    :id       :personae-array
    :desc     personae-array-desc
    :parse-fn (partial input/from-location :personae :json)
    :validate [(partial input/validate-throw :personae)
               "Failed to validate personae."]
    :assoc-fn u/conj-input]
   ["-m" "--models URI" "Persona Model Location"
    :id       :models
    :desc     models-desc
    :parse-fn (partial input/from-location :models :json)
    :validate [(partial input/validate-throw :models)
               "Failed to validate Models."]]
   ["-o" "--parameters URI" "Parameters Location"
    :id       :parameters
    :desc     parameters-desc
    :parse-fn (partial input/from-location :parameters :json)
    :validate [(partial input/validate-throw :parameters)
               "Failed to validate Parameters."]]
   ["-i" "--input URI" "Pre-validated input location"
    :id       :input
    :desc     input-desc
    :parse-fn (partial input/from-location :input :json)
    :validate [(partial input/validate-throw :input)
               "Failed to validate input."]]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sim-input
  "Given `options`, return a map of just the simulation options
   (either just `:input`, or a combo `:profiles`, `:personae-array`,
   `:parameters`, and `:models`)."
  [options]
  (let [sim-options (select-keys options [:input
                                          :profiles
                                          :personae-array
                                          :parameters
                                          :models])
        {:keys [override-seed]} options]
    (cond-> (or (:input sim-options)
                (dissoc sim-options :input))
      override-seed
      (assoc-in [:parameters :seed]
                (if (= -1 override-seed)
                  (random/rand-unbound-int (random/rng))
                  override-seed)))))

(defn assert-valid-input
  "Perform validation on `input` and fail w/ early termination if
   it is not valid.
   
   When this is called, we should have valid individual inputs. However, there
   may be cross-validation that needs to happen, so we compose the
   comprehensive spec from the options and check that."
  [input]
  (when-let [errors (not-empty (input/validate :input input))]
    (u/bail! (errors/map-coll->strs errors))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subcommand
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- write-input!
  ([input]
   (input/to-out input :json))
  ([input location]
   (input/to-file input :json location)
   (println (format "Input specification written to %s" location))))

(defn validate-input
  "Combine and validate the arguments given in `args` and write them
   to `location` (if `location` is provided)."
  [args]
  (u/exec-subcommand
   (conj validate-input-options
         ["-h" "--help"])
   (fn [{:keys [validated-input] :as options}]
     (let [input (sim-input options)]
       (assert-valid-input input)
       (if validated-input
         (write-input! input validated-input)
         (write-input! input))))
   args))
