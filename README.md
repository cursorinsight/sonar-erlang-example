# sonar-erlang-example

Example project to test SonarQube's `sonar-erlang` plugin

## Build

Execute `make all` to compile the code, build the documentation, run the
automatic tests and generate test coverage report.

### Build with Docker

Given a capable, `>20.10` Docker Server, you can easily release the project
with:

```
$ docker build -t sonar-erlang-example .
```

## Uploading check results with Sonar Scanner using Docker

Specifying the `sonar` target, you can easily prepare the project to run Sonar
Scanner later automatically:

```
$ docker build --target sonar -t sonar-erlang-example-with-scanner .
```

To start the Scanner, first export a previously generated access token, e.g.:

```
$ export SONAR_TOKEN=squ_7075e56d3bd13040512437ebc7ce02dbc72598b5
```

...and then run the container -- given the Sonar Server and the scanner that
will be run share the same Docker Server:

```
$ docker run \
    --rm -it \
    --add-host host.docker.internal:host-gateway \
    -e SONAR_HOST_URL=http://host.docker.internal:9000 \
    -e SONAR_TOKEN \
    sonar-erlang-example-with-scanner
```

## Documentation

`make doc` will generate documentation from docstrings and make it
available in `doc/index.html`.

## Running tests manually

A single test suite can be executed alone, e.g.:

```
SUITE=test/<app>_SUITE make ct-suite
```
