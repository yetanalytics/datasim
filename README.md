#  Data and Training Analytics Simulated Input Modeler (DATASIM)

## What is DATASIM?

DATASIM is an open source R&D project designed to provide specifications and a reference model application for the purpose of generating simulated xAPI data at scale.

DATASIM provides DoD distributed learning stakeholders and the broader xAPI community with the ability to simulate learning activities and generate the resulting xAPI statements at scale both in order to benchmark and stress-test the design of applications with the Total Learning Architecture and to provide stakeholders a way to evaluate the implementation of xAPI data design using the xAPI Profile specification. Ultimately, DATASIM can be used to support conformance testing of applications across the future learning ecosystem.

DATASIM is funded by the Advanced Distributed Learning Initiative at US DoD.

This documentation and repository refer to the simulation engine of DATASIM, which will run as a standalone CLI, and may also be deployed as a REST API. For the User Interface (which interacts with the API) please see https://github.com/yetanalytics/datasim-ui.

## Usage

### Simulation Inputs

The inputs to DATASIM consist of four parts, each represented by JSON. They are as follows:

#### xAPI Profile

A valid xAPI Profile is required for DATASIM to generate xAPI Statements. You can learn more about the xAPI Profile Specification [here](https://github.com/adlnet/xapi-profiles). This input can either be a single Profile JSON-LD document or an array of JSON-LD format profiles. At this time all referenced concepts in a Profile must be included in the input. For instance if in "Profile A" I have a Pattern that references a Statement Template found in "Profile B", both Profiles must be included in an array as the Profile input.

#### Actors

Predefined xAPI Actors (upon whom the simulation will be based) are required to run a DATASIM simulation. This takes the form of a JSON object containing an array of conformant Actors, an example of which is below:

    {"name": "trainees",
     "objectType": "Group",
     "member": [{"name": "Bob Fakename",
                 "mbox": "mailto:bob@example.org"},
                {"name": "Alice Faux",
                 "mbox": "mailto:alice@example.org"},
                {"name": "Fred Ersatz",
                 "mbox": "mailto:fred@example.org"}]}

#### Alignments

An alignment represents a way to influence the simulation by explicitly weighting an Actor's relationship to a part of the xAPI Profile. Each actor can have alignments to multiple parts of the Profile, and the weight system ranges from -1 to 1 (with 1 being an extremely high propensity for interaction in the simulation and -1 indicating that zero statements should be created for that Actor and that Profile Component). During the simulation these weights factor in but do not completely predict the outcome as there is still randomness in Actor behavior. The records are an array of objects where each object is a combination of Actor (id in IFI format), type ("Agent", "Group", or "Role") and an array of IRIs to align to, and weights for each.

    [
      {
        "id": "mbox::mailto:bob@example.org",
        "type": "Agent",
        "alignments": [
          {
            "component": "https://example.org/course/1440130447",
            "weight": -1.0
          }
        ]
      }
    ]


#### Simulation Parameters

The simulation parameters input covers the details of the simulation not covered by other pieces. This includes Start Time, End Time, Timezone, Max (number of statements) and *seed*. When run, the simulation will create a time sequence from the Start Time to the End Time and generated xAPI statements will have corresponding dates and times. The *seed* is important as it controls the inputs to all random value generation and corresponds to repeatability. A simulation run with the same inputs and the same seed will deterministically create the same xAPI Statements, but changing the seed value will create an entirely different simulation. An example of simulation parameters is below:

    {"start": "2019-11-18T11:38:39.219768Z",
     "end": "2019-11-19T11:38:39.219768Z",
     "max": 200,
     "timezone": "America/New_York",
     "seed": 42}


#### (Alternatively) Simulation Specification

The simulation specification is a single object containing of all of the above. This is exported during a simulation run and can serve as the sole input to another simulation.

    {"profiles":[ ... ],
     "parameters": ...,
     "personae": ...,
     "alignments": ... }

### System Requirements

Java (JDK 8+, OpenJDK or Oracle)

Clojure (1.10.1+)

### Deployment Models

This reference implementation of DATASIM can either be used as a CLI tool, or as a library embedded in another JVM application.

#### CLI

In the form of a CLI application, DATASIM takes the inputs listed above as JSON files as command line arguments and runs a simulation based on them. It also outputs the *Simulation Specification* during this process.

##### Local CLI

For the CLI the first step is to build the project so that it can be run on a JVM.

    make bundle

Now that we have this, navigate to target/bundle and run

    bin/run.sh

With no commands or `--help` it will give you the list of parameters:

    -p, --profile URI              The location of an xAPI profile, can be used multiple times.
    -a, --actor-personae URI       The location of an Actor Personae document indicating the actors in the sim.
    -l, --alignments URI           The location of an Actor Alignments Document.
    -o, --parameters URI     {...} The location of a Sim Parameters Document.
    -i, --input URI                The location of a JSON file containing a combined simulation input spec.
        --seed SEED                An integer seed to override the one in the input spec. Use -1 for random.
        --actor AGENT_ID           Pass an agent id in the format mbox::malto:bob@example.org to select actor(s)
    -E, --endpoint URI             The xAPI endpoint of an LRS to POST to, ex: https://lrs.example.org/xapi
    -U, --username URI             The basic auth username for the LRS you wish to post to
    -P, --password URI             The basic auth password for the LRS you wish to post to
    -B, --batch-size SIZE     25   The batch size for POSTing to an LRS
    -C, --concurrency CONC    4    The max concurrency of the LRS POST pipeline
    -L, --post-limit LIMIT    999  The total number of statements that will be sent to the LRS before termination. Overrides sim params. Set to -1 for no limit.
    -A, --[no-]async               Async operation. Use --no-async if statements must be sent to server in timestamp order.
    -h, --help                     Show this list.

For a simple run, we will first create the simulation specification by combining the inputs, validating them, and outputting to a simulation input file like so:

    bin/run.sh -p [profile json file] \
               -a [actors json filename] \
               -l [alignments json filename] \
               -o [sim params json filename] \
               validate-input [desired output filename]

Once we have that simulation specification, we can run the sim just from that like so:

    bin/run.sh -i dev-resources/input/simple.json generate

###### CLI LRS POST

If we have an endpoint and credentials for an LRS we can direcly POST the statements to it:

    bin/run.sh -i dev-resources/input/simple.json \
               -E [LRS xAPI endpoint ex. https://lrs.example.org/xapi] \
               -U [basic auth username] \
               -P [basic auth password] \
               -B [batch size] \
               -L [limit statements posted, -1 is no limit] \
               generate post

As statements are successfully sent to the LRS their IDs will be sent to stdout.

**NOTE: If the input specification doesn't have an end parameter and we set the option `-L -1`, DATASIM will continue posting to the LRS indefinitely.**

##### Docker

Build:

    make clean bundle && docker build -t yetanalytics/datasim:latest .

Run:

    docker run -v "$(pwd)"/dev-resources:/dev-resources  \
               -i yetanalytics/datasim:latest \
               -i /dev-resources/input/simple.json \
               generate

#### Library

As a library, this reference model can be integrated with any JVM application and its algorithms can be passed inputs and executed from code. It can be imported as a dep in Clojure, or compiled class files can be referenced from Java.

#### API

##### Starting the API

To start the API, run the following command from this directory:

    make server

By default the server starts at http://localhost:9090

##### API Config

There are two main ways you may want to add additional configuration to the API. The first is the Basic Authentication credentials required to call the API endpoints. Currently on startup the application looks for an Environment Variable called "credentials" in the form of "username:password". So if the username were datasim, and the password were datasim it would be "datasim:datasim". You can set this before launching by setting a local env variable on your machine.

The second is the port and allowed-origins if running from a browser. You can edit **http/allowed-origins** and **http/port** the following object in */src/server/com/yetanalytics/datasim/server.clj* to do so:

    {::http/routes          routes
     ::http/type            :immutant
     ::http/allowed-origins ["https://yetanalytics.github.io"
                         "http://localhost:9091"]
     ::http/host "0.0.0.0"
     ::http/port            9090
     ::http/join?           false}

Currently they are both configured to work with the default settings in the DATASIM-UI project locally.

##### API Endpoints

When launched as a REST API webapp, it has a few endpoints to allow dataset generation. The API is secured by Basic Authentication headers at this time (see API Config). The application has the following endpoints:

###### GET /health

This endpoint is simply a health check for the API. It should return a 200-OK if the app is up and running.

###### GET /api/v1/download-url

This endpoint is a convenience for the frontend to retrieve web resources like xAPI Profiles and is unlikely to be useful to anything else. This takes a single GET parameter, url, which contains a url-encoded string of the destination IRI.

###### POST /api/v1/generate

This endpoint takes a set of simulation inputs, returns a file with the output dataset and optionally pushes the data to an LRS. It accepts the inputs in the Content Type multipart/form-data of the following fields:

    profiles: Array of json-ld xAPI Profiles

    personae: JSON Object containing Actors formatted as above

    alignments: JSON Object containing Alignments formatted as above

    parameters: Simulation Parameters JSON Object

    lrs-endpoint: String with a valid LRS endpoint

    api-key: String with LRS API Key

    api-secret-key: String with LRS API Secret Key

    send-to-lrs: Boolean indicating whether or not to send data to the LRS if applicable

## License

DATASIM is licensed under the Apache License, Version 2.0. See [LICENSE](LICENSE) for the full license text

THE DATASIM SOFTWARE (“SOFTWARE”) IS PUBLISHED AS OPEN SOURCE SOFTWARE TO ENABLE USERS TO TEST CERTAIN CAPABILITIES OF THEIR SYSTEMS INCLUDING THE LEVEL OR CAPACITY OF xAPI DATA THAT CAN BE HANDLED BY A USER’S SYSTEM. THE SOFTWARE IS EXPRESSLY INTENDED TO TEST CAPACITY AND SYSTEM LIMITS AND CAN CAUSE SYSTEM OUTAGES WHEN A SYSTEM’S CAPACITY IS EXCEEDED. IT MUST BE USED WITH CAUTION.
  
THE PROVIDER AND PUBLISHER OF THE SOFTWARE (“PROVIDER”) PROVIDES NO WARRANTY, EXPRESS OR IMPLIED, WITH RESPECT TO THE SOFTWARE, ITS RELATED DOCUMENTATION OR OTHERWISE. THE SOFTWARE AND DOCUMENTATION ARE PROVIDED ON AN “AS IS” BASIS WITH ALL FAULTS. THE PROVIDER HEREBY DISCLAIMS ALL WARRANTIES AND CONDITIONS, EXPRESS OR IMPLIED, WRITTEN OR ORAL, INCLUDING, BUT NOT LIMITED TO, WARRANTIES OF MERCHANTABLE QUALITY, MERCHANTABILITY AND FITNESS FOR A PARTICULAR USE OR PURPOSE, NON-INFRINGEMENT AND THOSE ARISING BY STATUTE OR FROM A COURSE OF DEALING OR USAGE OF TRADE WITH RESPECT TO THE SOFTWARE, DOCUMENTATION AND ANY SUPPORT.

IN NO EVENT WILL PROVIDER OR ITS SUBSIDIARIES, OR AFFILIATES, NOR ANY OF THEIR RESPECTIVE SHAREHOLDERS, OFFICERS, DIRECTORS, EMPLOYEES, AGENTS OR REPRESENTATIVES HAVE ANY LIABILITY TO ANY USER OR TO ANY THIRD PARTY FOR ANY LOST PROFITS OR REVENUES OR FOR ANY DIRECT, INDIRECT, SPECIAL, INCIDENTAL, CONSEQUENTIAL, COVER OR PUNITIVE DAMAGES HOWEVER CAUSED, WHETHER IN CONTRACT, TORT OR UNDER ANY OTHER THEORY OF LIABILITY, AND WHETHER OR NOT THE PROVIDER HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES. THE FOREGOING DISCLAIMER WILL NOT APPLY ONLY TO THE EXTENT PROHIBITED BY APPLICABLE LAW.
BY MAKING USE OF THE SOFTWARE AND DOCUMENTATION, EACH USER HEREBY AGREES TO THE FORGOING DISCLAIMERS AND LIMITATIONS, AND HEREBY AGREES TO (I) RELEASE AND FOREVER DISCHARGE PROVIDER AND EACH OF ITS SUBSIDIARIES AND AFFILIATES, AND EACH OF THEIR RESPECTIVE SHAREHOLDERS, OFFICERS, DIRECTORS, EMPLOYEES, AGENTS OR REPRESENTATIVES (COLLECTIVELY, THE “RELEASED PARTIES”) FROM ANY CLAIM, DEMAND, CAUSE, ACTION, OR DAMAGE ARISING OUT OF OR IN CONNECTION WITH ANY USE OF THE SOFTWARE OR DOCUMENTATION (EACH, A “CLAIM”), AND (II) INDEMNIFY, DEFEND AND SAVE EACH RELEASED PARTY FROM ANY CLAIM AND ANY LOSS, DAMAGE, COST OR EXPENSE ARISING OUT OF OR IN CONNECTION WITH ANY CLAIM INCLUDING CLAIMS OF ANY THIRD PARTY RESULTING FROM USER’S USE OF THE SOFTWARE OR DOCUMENTATION.
IF YOU, AS THE USER, DO NOT AGREE TO THE FORGOING, THEN YOU ARE NOT AUTHORIZED TO USE THE SOFTWARE OR DOCUMENTATION AND ANY SUCH USE IS STRICTLY PROHIBITED.
