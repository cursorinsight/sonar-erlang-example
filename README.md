# sonar-erlang-example

Example project to test SonarQube's `sonar-erlang` plugin

## Build

Execute `make all` to compile the code, build the documentation, run the
automatic tests and generate test coverage report.

## Documentation

`make doc` will generate documentation from docstrings and make it
available in `doc/index.html`.

## Running tests manually

A single test suite can be executed alone, e.g.:

    SUITE=test/<app>_SUITE make ct-suite
