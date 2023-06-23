# Change Log

## [0.1.15] - 2023-06-26
- Update dependencies to address CVEs:
  - Update Pedestal dependencies to `0.6.0`
  - Update BouncyCastle and Buddy security lib deps:
    - Update buddy-auth to `3.0.323`
    - Update buddy-sign to `3.5.346`
    - Change BC lib from bcprov-jdk15on to bcprov-18on and update to `1.75`

## [0.1.14] - 2023-05-08
- Update dependencies to address CVEs:
  - Update Jetty dependencies to `9.4.51.v20230217`
  - Update Cheshire to `5.11.0`
  - Update Apache Commons Fileupload to `1.5`
  - Update Apache Jena to `4.8.0`
- Update NVD scanning to use the stand-alone [workflow-nvd](https://github.com/yetanalytics/workflow-nvd)

## [0.1.13] - 2022-11-03
- Update CI and CD pipelines to remove GitHub Action deprecation warnings.

## [0.1.12] - 2022-10-24
- Updated Jackson dependencies to address CVE-2022-42004 and CVE-2022-42003.

## [0.1.9] - 2022-07-06
- Removed Import by URL functionality (GET /api/v1/download-url) from API (see [here](https://github.com/yetanalytics/datasim/pull/80))

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
