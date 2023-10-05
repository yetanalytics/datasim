#  Data and Training Analytics Simulated Input Modeler (DATASIM)

[![Docker Image Version (latest semver)](https://img.shields.io/docker/v/yetanalytics/datasim?label=docker&style=plastic&color=blue)](https://hub.docker.com/r/yetanalytics/datasim)

## What is DATASIM?

DATASIM is an open source R&D project designed to provide specifications and a reference model application for the purpose of generating simulated xAPI data at scale.

DATASIM provides DoD distributed learning stakeholders and the broader xAPI community with the ability to simulate learning activities and generate the resulting xAPI statements at scale both in order to benchmark and stress-test the design of applications with the Total Learning Architecture and to provide stakeholders a way to evaluate the implementation of xAPI data design using the xAPI Profile specification. Ultimately, DATASIM can be used to support conformance testing of applications across the future learning ecosystem.

DATASIM is funded by the Advanced Distributed Learning Initiative at US DoD.

This documentation and repository refer to the simulation engine of DATASIM, which will run as a standalone CLI, and may also be deployed as a REST API. For the User Interface (which interacts with the API) please see https://github.com/yetanalytics/datasim-ui.

## Installation

To use the core DATASIM library in your project, use the following dependency in your `deps.edn` file:

```clojure
com.yetanalytics/datasim {:mvn/version "0.4.0"}
```

If you wish to install DATASIM as an application with features such as CLI or the webserver, perform the following steps:
1. Clone the DATASIM GitHub repo
2. Execute the `make bundle` command

See [Deployment Models](#deployment-models) for more information about the differences between using DATASIM as a library and as an app.

## Usage

### Simulation Inputs

The inputs to DATASIM consist of four parts, each represented by JSON. They are as follows:

#### xAPI Profile(s)

One or more valid xAPI Profiles are required for DATASIM to generate xAPI Statements. You can learn more about the xAPI Profile Specification [here](https://github.com/adlnet/xapi-profiles). This input can either be a single Profile JSON-LD document or an array of JSON-LD format profiles. At this time all referenced concepts in a Profile must be included in the input. For instance if in "Profile A" I have a Pattern that references a Statement Template found in "Profile B", both Profiles must be included in an array as the Profile input.

Note that by default, any patterns with a `primary` property set to `true` in the provided profiles will be used for generation. You can control which profiles these primary patterns are sourced from with the `gen-profiles` option by supplying one or more profile IDs. You can further control which specific primary patterns are used with the `gen-patterns` option by supplying one or more pattern IDs.

#### Personae

Predefined xAPI Actors (upon whom the simulation will be based) are required to run a DATASIM simulation. This takes the form of a JSON array of xAPI Groups, each object containing an array of conformant Actor members, an example of which is below:

```json
    [
        {
            "name": "trainees1",
            "objectType": "Group",
            "member": [
                {
                    "name": "Bob Fakename",
                    "mbox": "mailto:bob@example.org"
                },
                {
                    "name": "Alice Faux",
                    "mbox": "mailto:alice@example.org"
                }
            ]
        },
        {
            "name": "trainees2",
            "objectType": "Group",
            "member": [
                {
                    "name": "Fred Ersatz",
                    "mbox": "mailto:fred@example.org"
                }
            ]
        }
    ]
```

#### Models

Models represents user-provided influences on xAPI simulation. Each model is a JSON object that consists of the following properties:
- `personae`: An array of Actors, Groups, or Role objects that define who the model applies to. If this is missing, then the model serves as the default model for the simulation. Each `personae` array must be unique, though Actors, Groups, or Roles may repeat across different models.
- `verbs`: An array of objects with Verb `id` and `weight` values. Valid `weight` values range from `0` to `1`, where `0` denotes that that component will not be chosen (unless all other weights are also `0`). If not present, a default weight of `0.5` will be used.
- `activities`: An array of objects with Activity `id` and `weight` values (as described under `verbs`).
- `activityTypes`: An array of objects with Activity Type `id`
and `weight` values (as described under `verbs`).
- `patterns`: An array of objects with Pattern `id` and the following additional optional values:
  - `weights`: An array of child Pattern/Template `id` and `weight` values. Each weight affects how likely each of the Pattern's child patterns are chosen (for `alternates`) or how likely the child Pattern will be selected at all (for `optional`, for these `null` is also a valid option). This has no effect on `sequence`, `zeroOrMore`, or `oneOrMore` Patterns.
  - `repeat-max`: A positive integer representing the maximum number of times (exclusive) the child pattern can be generated. Only affects `zeroOrMore` and `oneOrMore` patterns.
  - `bounds`: An array of objects containing key-value pairs where each value is an array of singular values (e.g. `"January"`) or pair arrays of start and end values (e.g. `["January", "October"]`). For example `{"years": [2023], "months": [[1 5]]}` describes an inclusive bound from January to May 2023. The following are valid bound values:
    - `years`: Any positive integer
    - `months`: `1` to `12`, or their name equivalents, i.e. `"January"` to `"December"`
    - `daysOfMonth:` `1` to `31` (though `29` or `30` are skipped at runtime for months that do not include these days)
    - `daysOfWeek`: `0` to `6`, or their name equivalents, i.e. `"Sunday"` to `"Saturday"`
    - `hours`: `0` to `23`
    - `minutes`: `0` to `59`
    - `seconds`: `0` to `59`
  - `period`: an object with `mean`, `min`, and `unit` properties. `min` specifies a minimum delay, `mean` the average delay (added on top of `min`), and `unit` the time unit for both (valid values are `millis`, `seconds`, `minutes`, `hours`, `days`, and `weeks`). This only applies to Statement Templates and Patterns; child Patterns or Templates will override any `period` properties set by parent Patterns.
  - `retry`: One of four options that determine Statement generation retry behavior in the event where a time bound is exceeded:
    - `null` (or not present): Terminate the generation on the current Pattern immediately, and move again with the next Pattern's generation.
    - `"pattern"`: Retry generation of this Pattern if this Pattern's bound is exceeded.
    - `"child"`: Retry generation of whichever child Pattern or Statement Template in which this Pattern's bound is exceeded.
    - `"template"`: Retry generation of the Statement Template that exceeded this Pattern's bound.
- `templates`: An array of objects with Statement Template `id` and optional `bounds`, `period`, and `retry` values, as explained above in `patterns`. Note that `weights` and `repeat-max` do not apply here.
- `objectOverrides`: An array of objects containing (xAPI) `object` and `weight`. If present, these objects will overwrite any that would have been set by the Profile.

An example of a model array with valid `personae`, `verbs`, and `templates` is shown below:

```json
    [
      {
        "personae": [
            {
                "id": "mbox::mailto:bob@example.org",
                "type": "Agent"
            }
        ],
        "verbs": [
            {
                "component": "https://example.org/verb/did",
                "weight": 0.8
            }
        ],
        "templates": [
            {
                "component": "https://w3id.org/xapi/cmi5#satisfied",
                "bounds": [
                    {
                        "years": [2023],
                        "months": [["January", "May"]]
                    }
                ],
                "period": {
                    "min": 1,
                    "mean": 2.0,
                    "unit": "second"
                }
            }
        ]
      }
    ]
```

#### Simulation Parameters

The simulation parameters input covers the details of the simulation not covered by other pieces. This includes Start Time, End Time, Timezone, Max (number of statements) and *seed*. When run, the simulation will create a time sequence from the Start Time to the End Time and generated xAPI statements will have corresponding dates and times. The *seed* is important as it controls the inputs to all random value generation and corresponds to repeatability. A simulation run with the same inputs and the same seed will deterministically create the same xAPI Statements, but changing the seed value will create an entirely different simulation. An example of simulation parameters is below:

```json
    {
        "start": "2019-11-18T11:38:39.219768Z",
        "end": "2019-11-19T11:38:39.219768Z",
        "max": 200,
        "timezone": "America/New_York",
        "seed": 42
    }
```

#### (Alternatively) Simulation Specification

The simulation specification is a single object containing of all of the above. This is exported during a simulation run and can serve as the sole input to another simulation.

```json
    {
        "profiles": [ ... ],
        "parameters": ...,
        "personae-array": [ ... ],
        "models": [...]
    }
```

### System Requirements

Java (JDK 8+, OpenJDK or Oracle)

[Clojure CLI (1.10.2+)](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)

### Deployment Models

This reference implementation of DATASIM can either be used as a CLI tool, or as a library embedded in another JVM application.

#### CLI

In the form of a CLI application, DATASIM takes the inputs listed above as JSON files as command line arguments and runs a simulation based on them. It also outputs the *Simulation Specification* during this process.

##### Local CLI

For the CLI the first step is to build the project so that it can be run on a JVM.

```
    make bundle
```

Now that we have this, navigate to target/bundle and run

```
    bin/run.sh
```

With no commands or `--help` it will give you the list of subcommands:

| Subcommand       | Description
| ---              | ---
| `validate-input` | Validate the input and create an input JSON file.
| `generate`       | Generate statements from input and print to stdout.
| `generate-post`  | Generate statements from input and POST them to an LRS.

The `validate-input` subcommand is used to validate and combine input files. These are its arguments:

| Argument | Description
| --- | ---
| `-p, --profile URI` | The location of an xAPI profile, can be used multiple times.
| `-a, --actor-personae URI` | The location of an Actor Personae document indicating the actors in the sim.
| `-m, --models URI` | The location of an Persona Model document, to describe alignments and overrides for the personae.
| `-o, -parameters URI` | The location of simulation parameters document. Uses the current time and timezone as defaults if they are not present. (The "o" stands for "options.")
| `-i, --input URI` | The location of a JSON file containing a combined simulation input spec.
| `-c, --combined-input URI` | The location of the validated input to be produced.

The `generate` subcommand is used to generate statements from an input and print them to standard output. The inputs can be a combined `--input` location or a combination of `-p`, `-a`, `-m`, and `-o` inputs. The additional arguments are as follows:
| Argument | Description
| --- | ---
| `--seed SEED` | An integer seed to override the one in the input spec. Use -1 for a random seed.
| `--actor AGENT_ID` | Pass an agent id in the format 'mbox::mailto:[email]' to select actor(s)
| `--gen-profile IRI` | Only generate based on primary patterns in the given profile. May be given multiple times to include multiple profiles.
| `--gen-pattern IRI` | Only generate based on the given primary pattern. May be given multiple times to include multiple patterns.

The `generate-post` subcommand is used to generate statements from an input and POST them to an LRS. In addition to the `generate` arguments, this subcommands has these additional arguments:
| Argument | Description
| --- | ---
| `-E, --endpoint URI` | The xAPI endpoint of an LRS to POST to, ex: `https://lrs.example.org/xapi`
| `-U, --username URI` | The Basic Auth username for the LRS.
| `-P, --password URI` | The Basic Auth password for the LRS.
| `-B, --batch-size SIZE` | The batch size, i.e. how many statements to send at a time, for POSTing.
| `-C, --concurrency CONC` | The max concurrency of the LRS POST pipeline.
| `-L, --post-limit LIMIT` | The total number of statements that will be sent to the LRS before termination. Overrides sim params. Set to -1 for no limit.
| `-A, --[no-]async` | Async operation. Use `--no-async`` if statements must be sent to server in timestamp order.

The following is an example of a simple run. We first create a combined input file using `validate-input`:
```
bin/run.sh validate-input \
    -p dev-resources/profile/cmi5/fixed.json \
    -a dev-resources/personae/simple.json \
    -m dev-resources/models/simple.json \
    -o dev-resources/parameters/simple.json \ 
    -c dev-resources/input/simple.json
```

Once we have that sim specification, we can run the simulation using the `generate`:
```
bin/run.sh generate -i dev-resources/input/simple.json
```

If we have an endpoint and credentials for an LRS we can directly POST the simulated statements using `generate-post`:

```
bin/run.sh generate-post \
    -i dev-resources/input/simple.json \
    -E http://localhost:8080/xapi \
    -U username \
    -P password \
    -B 20 \
    -L 1000 \
```

As statements are successfully sent to the LRS their IDs will be sent to stdout.

**NOTE: If the input specification doesn't have an end parameter and we set the option `-L -1`, DATASIM will continue posting to the LRS indefinitely.**

##### Docker

Build:

    make clean bundle && docker build -t yetanalytics/datasim:latest .

Run (CLI):

    docker run -v "$(pwd)"/dev-resources:/dev-resources  \
               -i yetanalytics/datasim:latest \
               -i /dev-resources/input/simple.json \
               generate

Run (API):

    docker run -it --entrypoint bin/server.sh yetanalytics/datasim:latest

#### Library

As a library, this reference model can be integrated with any JVM application and its algorithms can be passed inputs and executed from code. It can be imported as a dep in Clojure, or compiled class files can be referenced from Java.

#### API

##### Starting the API

To start the API, run the following command from this directory:

    make server

By default the server starts at http://localhost:9090

##### API Config

The API is configurable with the following runtime environment variables:

| Variable            | Default                                                                                          | Notes                                                                                                  | Example           |
|---------------------|--------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------|-------------------|
| CREDENTIALS         | <none>                                                                                           | Basic Authentication credentials required to call the API endpoints in the form of `username:password` | `datasim:datasim` |
| API_ROOT_PATH       | <none>                                                                                           | Root path to prefix API routes. Must begin with a `/`, cannot end with a `/`.                          | `/foo`            |
| API_HOST            | `0.0.0.0`                                                                                        | Host on which to bind the API server.                                                                  | `localhost`       |
| API_PORT            | `9090`                                                                                           | Port on which to bind the API server.                                                                  | `8080`            |
| API_ALLOWED_ORIGINS | <details>`https://yetanalytics.github.io,http://localhost:9091`<summary>(URLs)</summary></details> | CORS allowed origins for the API server, separated by commas.                                          | `*`               |

Currently defaults are configured to work with the default settings in the DATASIM-UI project locally.

##### API Endpoints

When launched as a REST API webapp, it has a few endpoints to allow dataset generation. The API is secured by Basic Authentication headers at this time (see API Config). The application has the following endpoints:

###### GET /health

This endpoint is simply a health check for the API. It should return a 200-OK if the app is up and running.

###### POST /api/v1/generate

This endpoint takes a set of simulation inputs, returns a file with the output dataset and optionally pushes the data to an LRS. It accepts the inputs in the Content Type multipart/form-data of the following fields:

    profiles: Array of json-ld xAPI Profiles

    personae-array: Array of JSON Objects containing Actors formatted as above

    models: Array of JSON Objects containing Models formatted as above

    parameters: Simulation Parameters JSON Object

    lrs-endpoint: String with a valid LRS endpoint

    api-key: String with LRS API Key

    api-secret-key: String with LRS API Secret Key

    send-to-lrs: Boolean indicating whether or not to send data to the LRS if applicable

#### Distributed Generation

DATASIM deterministically generates streams of statements on a per-actor basis making it possible to distribute the generation of simulation data across multiple processes or physical servers.

##### Onyx Peer Cluster

DATASIM uses [Onyx](http://www.onyxplatform.org/) and [ZooKeeper](https://zookeeper.apache.org/) to coordinate distributed generation. One or more DATASIM _peers_ can be launched in a _cluster_.

The cluster accepts DATASIM combined input files and LRS target information as input. The cluster peers will coordinate to generate data and post it to the target LRS.

###### Capacity Planning

In order to generate and send the data the cluster must contain enough peers to generate and execute the specified input.

The user specifies desired concurrency by use of the `-c` option. This option must be a positive integer not greater than the number of actors in the simulation.

DATASIM will evenly partition the data into as many "buckets" as specified and attempt to send them simultaneously.

For each partition of simulation actors, two peers are required. Therefore:

    total-required-peers = concurrency * 2

For example, the DATASIM "simple" example input found at `dev-resources/input/simple.json` contains 3 actors. If we choose the maximum concurrency of 3 then:

    total-required-peers = 3 * 2 = 6

If we wanted to sacrifice throughput we could run it with the _minimum concurrency_ of 1:

    total-required-peers = 1 * 2 = 2

Note that if a cluster does not have sufficient peers to execute a job it will wait until it does and complete it. Each physical instance in a cluster can run as many "virtual" peers as it has processors.

##### Utility CLI

DATASIM has a separate CLI for distributed operation:

    bin/onyx.sh --help ## in dev, do: clojure -Monyx:onyx-dev -m com.yetanalytics.datasim.onyx.main --help

    DATASIM Cluster CLI

    Usage: bin/onyx.sh [options] action

    Options:
      -n, --n-vpeers N_VPEERS               Number of VPEERS to launch. Overrides config value.
      -t, --tenancy-id TENANCY_ID           Onyx Tenancy ID
      -i, --input-loc INPUT_LOC             DATASIM input location
      -c, --concurrency                     Desired concurrency of job.
      -e, --endpoint ENDPOINT               xAPI LRS Endpoint like https://lrs.example.org/xapi
      -u, --username USERNAME               xAPI LRS BASIC Auth username
      -p, --password PASSWORD               xAPI LRS BASIC Auth password
          --x-api-key X_API_KEY             API Gateway API key
          --[no-]strip-ids                  Strip IDs from generated statements
          --[no-]remove-refs                Filter out statement references
      -b, --[no-]block                      Block until the job is done
          --nrepl-bind NREPL_BIND  0.0.0.0  If provided on peer launch will start an nrepl server bound to this address
          --nrepl-port NREPL_PORT           If provided on peer launch will start an nrepl server on this port
      -h, --help

    Actions:
      start-peer    Start an onyx peer
      start-driver  Start an aeron media driver
      submit-job    Submit a datasim input for submission to an LRS
      repl          Start a local repl

##### AWS Deployment

A set of [AWS CloudFormation](https://aws.amazon.com/cloudformation/) templates capable of deploying the cluster is included for demonstration purposes. Note that these templates should not be used for production systems.

###### VPC (Optional) - `template/0_vpc.yml`

To deploy the cluster you'll need an AWS VPC with at least 1 subnet. The included template will create a VPC with 4 subnets, 2 public and 2 private.

###### ZooKeeper (Optional) - `template/1_zk.yml`

The cluster requires a working Apache Zookeeper Ensemble version 3.5. This template creates a simple static-ip based ensemble of 3 nodes. Make sure to choose a private subnet and ensure that the chosen IPs fall within its CIDR range.

###### Cluster - `template/2_cluster.yml`

Make sure you've done the following (refer to the template params referenced):

* Compile the project with `make clean bundle`
* Zip the `target/bundle` directory to a file called `<ArtifactId>-<ArtifactVersion>`
* Upload the zip to an s3 bucket with an enclosing path of your choosing like: `s3://<ArtifactBucketName>/<ArtifactBasePath>/<ArtifactId>-<ArtifactVersion>`

Deploy the template to the same VPC as ZooKeeper to a subnet that can reach the ZooKeeper instances. Make sure to choose the correct security group for the ZooKeeper ensemble for `ZooKeeperGroupId`.

For documentation on other parameters, see the template.

###### Submitting a Job

You can submit a job as follows:

SSH in to a cluster node:

    sudo su        # be root
    cd /datasim    # correct working dir

    # optionally get input first
    curl https://raw.githubusercontent.com/yetanalytics/datasim/master/dev-resources/input/simple.json -o simple.json
    # note the CloudFormation Stack params -> env

    TENANCY_ID=<TenancyId> \ # optional if -t or --tenancy-id is provided below
    ONYX_PROFILE=prod \
    ZK_ADDRESS=<ZooKeeperAddress> \
    ZK_SERVER_PORT=<ZooKeeperPort> \
    ZK_TIMEOUT=<ZooKeeperTimeout> \
    PEER_PORT=<PeerPort> \
    N_VPEERS=<VPeersCount> \
    LANG=en_US.UTF-8 \
    AWS_REGION=<AWS::Region> \
    X_RAY_ENABLED=true \
    AWS_XRAY_CONTEXT_MISSING=LOG_ERROR \
    AWS_XRAY_TRACING_NAME=datasim-cluster:us-east-1 \
    BIND_ADDR=<IP of Instance> \
    ./bin/submit_job.sh \
      -t <override tenancy (optional)> \
      --concurrency 3 \
      -i simple.json \
      -e https://lrs.example.org/xapi \
      -u <LRS BASIC Auth Username> \
      -p <LRS BASIC Auth Password>

## License

DATASIM is licensed under the Apache License, Version 2.0. See [LICENSE](LICENSE) for the full license text

THE DATASIM SOFTWARE (“SOFTWARE”) IS PUBLISHED AS OPEN SOURCE SOFTWARE TO ENABLE USERS TO TEST CERTAIN CAPABILITIES OF THEIR SYSTEMS INCLUDING THE LEVEL OR CAPACITY OF xAPI DATA THAT CAN BE HANDLED BY A USER’S SYSTEM. THE SOFTWARE IS EXPRESSLY INTENDED TO TEST CAPACITY AND SYSTEM LIMITS AND CAN CAUSE SYSTEM OUTAGES WHEN A SYSTEM’S CAPACITY IS EXCEEDED. IT MUST BE USED WITH CAUTION.

THE PROVIDER AND PUBLISHER OF THE SOFTWARE (“PROVIDER”) PROVIDES NO WARRANTY, EXPRESS OR IMPLIED, WITH RESPECT TO THE SOFTWARE, ITS RELATED DOCUMENTATION OR OTHERWISE. THE SOFTWARE AND DOCUMENTATION ARE PROVIDED ON AN “AS IS” BASIS WITH ALL FAULTS. THE PROVIDER HEREBY DISCLAIMS ALL WARRANTIES AND CONDITIONS, EXPRESS OR IMPLIED, WRITTEN OR ORAL, INCLUDING, BUT NOT LIMITED TO, WARRANTIES OF MERCHANTABLE QUALITY, MERCHANTABILITY AND FITNESS FOR A PARTICULAR USE OR PURPOSE, NON-INFRINGEMENT AND THOSE ARISING BY STATUTE OR FROM A COURSE OF DEALING OR USAGE OF TRADE WITH RESPECT TO THE SOFTWARE, DOCUMENTATION AND ANY SUPPORT.

IN NO EVENT WILL PROVIDER OR ITS SUBSIDIARIES, OR AFFILIATES, NOR ANY OF THEIR RESPECTIVE SHAREHOLDERS, OFFICERS, DIRECTORS, EMPLOYEES, AGENTS OR REPRESENTATIVES HAVE ANY LIABILITY TO ANY USER OR TO ANY THIRD PARTY FOR ANY LOST PROFITS OR REVENUES OR FOR ANY DIRECT, INDIRECT, SPECIAL, INCIDENTAL, CONSEQUENTIAL, COVER OR PUNITIVE DAMAGES HOWEVER CAUSED, WHETHER IN CONTRACT, TORT OR UNDER ANY OTHER THEORY OF LIABILITY, AND WHETHER OR NOT THE PROVIDER HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES. THE FOREGOING DISCLAIMER WILL NOT APPLY ONLY TO THE EXTENT PROHIBITED BY APPLICABLE LAW.
BY MAKING USE OF THE SOFTWARE AND DOCUMENTATION, EACH USER HEREBY AGREES TO THE FORGOING DISCLAIMERS AND LIMITATIONS, AND HEREBY AGREES TO (I) RELEASE AND FOREVER DISCHARGE PROVIDER AND EACH OF ITS SUBSIDIARIES AND AFFILIATES, AND EACH OF THEIR RESPECTIVE SHAREHOLDERS, OFFICERS, DIRECTORS, EMPLOYEES, AGENTS OR REPRESENTATIVES (COLLECTIVELY, THE “RELEASED PARTIES”) FROM ANY CLAIM, DEMAND, CAUSE, ACTION, OR DAMAGE ARISING OUT OF OR IN CONNECTION WITH ANY USE OF THE SOFTWARE OR DOCUMENTATION (EACH, A “CLAIM”), AND (II) INDEMNIFY, DEFEND AND SAVE EACH RELEASED PARTY FROM ANY CLAIM AND ANY LOSS, DAMAGE, COST OR EXPENSE ARISING OUT OF OR IN CONNECTION WITH ANY CLAIM INCLUDING CLAIMS OF ANY THIRD PARTY RESULTING FROM USER’S USE OF THE SOFTWARE OR DOCUMENTATION.
IF YOU, AS THE USER, DO NOT AGREE TO THE FORGOING, THEN YOU ARE NOT AUTHORIZED TO USE THE SOFTWARE OR DOCUMENTATION AND ANY SUCH USE IS STRICTLY PROHIBITED.
