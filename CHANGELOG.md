# Change Log

## [0.4.1] - 2024-01-25
- Fix a number of bugs associated with Statement generation:
  - Fix places where `:definition` was used when `:activityDefinition` was supposed to be used in the code, causing Activities not to be associated with their respective Activity Types.
  - Stringify keys for map values of `any`, `all`, and `none` in Statement Template rules.
- Update Cheshire dependency to 5.12.0.

## [0.4.0] - 2024-01-03
- Change `alignments` inputs to `models` inputs that incorporate additional temporal properties.
- Make `models` an array in which the user can apply different `personae` to.
- Separate alignments by component types: `verbs`, `activities`, `activityTypes`, `templates`, `patterns`, and `objectOverrides`.
- The new temporal properties are as follows:
  - `bounds`: the time intervals in which a particular Pattern's or Template's statements are allowed to generate in.
  - `boundRestarts`: which Pattern or Template should repeat if a bound is exceeded.
  - `periods`: the frequency in which a Pattern's or Template's statements are generated.
- Make `repeatMax` a model property for Patterns instead of a hardcoded constant.
- Rework the CLI so that subcommands go in the front, not the end, of the top-level command.

## [0.3.2] - 2023-10-03
- Update server Jetty dependencies to v9.4.52 to address CVEs.

## [0.3.1] - 2023-07-24
- Fix bug where the same `any` and `all` values are chosen within the same generated sequence.

## [0.3.0] - 2023-07-13
- Major reworking and refactoring of internal namespaces; some notable changes:
  - Completely reworked the structure of the `datasim.xapi` directory
  - Removed the `datasim.json` directory in favor of using the [pathetic](https://github.com/yetanalytics/pathetic) and [schemer](https://github.com/yetanalytics/schemer) libraries
  - Move `timestamp` and `random` namespaces to a new `datasim.math` namespace
  - Change input functions to use multimethods instead of records
  - Refactored CLI and server namespaces
- Add top-level API functions to the `datasim` namespace:
  - `read-input` that wraps `input/from-location` for JSON input maps
  - `generate-seq` and `generate-map` for synchronous statement sequence and skeleton map gen, respectively
  - `generate-seq-async` and `generate-map-async` for async versions of the above
- Changes to Statement generation:
  - Template Context Activities and Attachment Usage Type properties are now supported
  - Performs basic "statement healing" by filling in required properties after Template rule/property application; verbs and activities are healed mainly by selecting values from the profile cosmos, while other objects are healed using spec generation
  - Supports complected JSONPath locations and selectors, e.g. those that use the pipe operator
  - Removes object rules and properties if an object override is present, preventing redundant rule application and potential clashes
  - Rules with `recommended` presence now have their values be applied/generated
  - Fix a bug where ARMA squences, which form the basis of statement generation, were incorrectly generated
  - Precompile Statement Template properties and rules before statement generation, as an optimization measure.

For more information on all changes, see Pull Requests #89, #93 to #97, #98 to #103, and #106 to #109.

## [0.2.0] - 2023-07-07
- Update the following dependencies:
  - clojure to `1.11.1` (this is a potential breaking change to any downstream apps that use DATASIM with a previous Clojure version)
  - core.async to `1.6.673`
  - core.memoize to `1.0.257`
  - test.check to `1.1.1`
  - clojure.java-time to `1.2.0`
  - http-kit to `2.7.0`

## [0.1.16] - 2023-06-27
- Update dependencies to address CVEs:
  - Update Pedestal dependencies to `0.6.0`
  - Update BouncyCastle and Buddy security lib deps:
    - Update buddy-auth to `3.0.323`
    - Update buddy-sign to `3.5.346`
    - Change BC lib from bcprov-jdk15on to bcprov-18on and update to `1.75`

## [0.1.15] - 2023-05-08
- Update dependencies to address CVEs:
  - Update Jetty dependencies to `9.4.51.v20230217`
  - Update Cheshire to `5.11.0`
  - Update Apache Commons Fileupload to `1.5`
  - Update Apache Jena to `4.8.0`
- Update NVD scanning to use the stand-alone [workflow-nvd](https://github.com/yetanalytics/workflow-nvd)

## [0.1.14] - 2022-11-16
- Exclude msgpack dependency to clear CVE-2022-41719

## [0.1.13] - 2022-11-03
- Update CI and CD pipelines to remove GitHub Action deprecation warnings. 

Note that this update does not affect the API/application.

## [0.1.12] - 2022-10-24
- Updated Jackson dependencies to address CVE-2022-42004 and CVE-2022-42003.

## [0.1.11] - 2022-07-15
- Apply suppression to NVD scanning for the false positives CVE-2022-23172 and CVE-2022-23173.

Note that this update does not affect the API/application.

## [0.1.10] - 2022-07-11
- Fix CVEs CVE-2022-2048 and CVE-2022-2047.
- Apply suppression to NVD scanning for the false positive CVE-2022-2191.

## [0.1.9] - 2022-07-06
- Removed Import by URL functionality (GET /api/v1/download-url) from API (see [here](https://github.com/yetanalytics/datasim/pull/80))

## [0.1.8] - 2022-07-01
- Add `gen-profiles` and `gen-patterns` parameters to only generate based on the specified Profiles and primary patterns, respectively.
- Add corresponding `--gen-profile` and `--gen-pattern` CLI arguments.

## [0.1.7] - 2022-06-06
- Fix broken Profile error printing. ([#76](https://github.com/yetanalytics/datasim/pull/76))

## [0.1.6] - 2022-06-03
- Fix bug where reading in large inputs fails. ([#74](https://github.com/yetanalytics/datasim/pull/74))

## [0.1.5] - 2022-05-24
- Add environment variables for webserver configuration.
- Add public Docker image.

## [0.1.4] - 2022-05-16
- Update Apache Jena to `4.5.0`.

## [0.1.3] - 2022-04-26
- Add GitHub Actions CI and CD for testing, bundle building, and vulnerability scanning.
- Update dependencies to patch vulnerabilities (see [here](https://github.com/yetanalytics/datasim/pull/67) for more information).

## [0.1.2] - 2022-03-04
- Update xAPI-Schema and Pan libraries to their latest versions. ([Details](https://github.com/yetanalytics/datasim/pull/65))
- Change error output to be compatible with new Pan version, as well as DATASIM UI.

## [0.1.1] - 2021-05-04
- Fix bug where DATASIM server was not safe for AOT compilation. ([Details](https://github.com/yetanalytics/datasim/pull/63))

## [0.1.0] - 2021-03-23
Initial release of DATASIM.
