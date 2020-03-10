#  Data and Training Analytics Simulated Input Modeler (DATASIM)

## What is DATASIM?

The Data and Training Analytics Simulated Input Modeler is a research and development project designed to provide open source specifications and an open source reference model application for the purpose of generated simulated xAPI data at scale.

The purpose of DATASIM is to provide DoD distributed learning stakeholders the ability to simulate learning activities and generate the resulting xAPI statements at scale both in order to benchmark and stress-test the design of applications with the Total Learning Architecture and to provide stakeholders a way to evaluate the implementation of xAPI data design using the xAPI Profile specification. Ultimately, DATASIM can be used to support conformance testing of applications across the future learning ecosystem.

## Usage

### Inputs

The inputs to DATASIM consist of four parts, each represented by JSON. They are as follows:

#### xAPI Profile

A valid xAPI Profile is required for DATASIM to generate xAPI Statements. You can learn more about the xAPI Profile Specification [here](https://github.com/adlnet/xapi-profiles).

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

An alignment represents a way to influence the simulation by explicitly weighting an Actor's relationship to a part of the xAPI Profile. Each actor can have alignments to multiple parts of the Profile, and the weight system ranges from -1 to 1 (with 1 being an extremely high propensity for interaction in the simulation and -1 being the opposite). During the simulation these weights factor in but do not completely predict the outcome as there is still randomness in Actor behavior. The records are a combination of Actor, IRI to align to, and weight.

    {"mbox::mailto:bob@example.org":
      {"https://example.org/activity/a": 0.5,
       "https://example.org/activity/c": -0.2},
     "mbox::mailto:alice@example.org":
      {"https://example.org/activity/c": 0.7,
       "https://example.org/activity/d": -0.02}}

#### Simulation Parameters

The simulation parameters input covers the details of the simulation not covered by other pieces. This includes Start Time, End Time, Timezone, and *seed*. When run, the simulation will create a time sequence from the Start Time to the End Time and generated xAPI statements will have corresponding dates and times. The *seed* is important as it controls the inputs to all random value generation and corresponds to repeatability. A simulation run with the same inputs and the same seed will deterministically create the same xAPI Statements, but changing the seed value will create an entirely different simulation. An example of simulation parameters is below:

    {"start": "2019-11-18T11:38:39.219768Z",
     "end": "2019-11-19T11:38:39.219768Z",
     "timezone": "America/New_York",
     "seed": 42}


#### (Alternatively) Simulation Specification

The simulation specification is a single object containing of all of the above. This is exported during a simulation run and can serve as the sole input to another simulation.

    {"profiles":[ ... ],
     "parameters": ...,
     "personae": ...,
     "alignments": ... }


### Deployment Models

This reference implementation of DATASIM can either be used as a CLI tool, or as a library embedded in another JVM application.

*When complete, the reference model will also include an application server and UI which can be used to interact with it. Documentation will be updated to include instructions for that model upon completion*

#### CLI

In the form of a CLI application, DATASIM takes the inputs listed above as JSON files as command line arguments and runs a simulation based on them. It also outputs the *Simulation Specification* during this process.

##### CLI

For the CLI the first step is to build the project so that it can be run on a JVM.

    make bundle

Now that we have this, navigate to target/bundle and run

    bin/run.sh

With no commands it will give you the list of parameters.

For a simple run, we will first create the simulation specification by combining the inputs, validating them, and outputting to a simulation input file like so:

    bin/run.sh -p [profile json file] -a [actors json filename] -l [alignments json filename] -o [sim params json filename] validate-input [desired output filename]

Once we have that simulation specification, we can run the sim just from that like so:

    bin/run.sh -i dev-resources/input/simple.json generate

#### Docker

Build:

    $ make clean bundle && docker build -t yetanalytics/datasim:latest .

Run:

    $ docker run -v "$(pwd)"/dev-resources:/dev-resources  -i yetanalytics/datasim:latest -i /dev-resources/input/simple.json generate

#### Library

As a library, this reference model can be integrated with any JVM application and its algorithms can be passed inputs and executed from code.

## License

DATASIM is licensed under the Apache License, Version 2.0. See [LICENSE](LICENSE) for the full license text
