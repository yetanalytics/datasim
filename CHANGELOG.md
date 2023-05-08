# Change Log

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
