# datasim

A Clojure library designed to ... well, that part is up to you.

## Usage

### Clojure Cli

    clojure -Acli:run -h

### Docker

Build:

    docker build -t yetanalytics/datasim:latest .

Run:

    docker run -v "$(pwd)"/dev-resources:/dev-resources  -i yetanalytics/datasim:latest -i /dev-resources/input/simple.json generate

## License

Copyright © 2019 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
