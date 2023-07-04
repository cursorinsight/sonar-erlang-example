# syntax=docker/dockerfile:1.2-labs

# Building
# ========
#
# Use the following command to build the image:
#
#     $ docker build . -t sonar-erlang-example

# Target Alpine version
ARG ALPINE_VERSION=3.16

# Target Erlang version
ARG ERLANG_VERSION=23

# Target Sonar Scanner CLI version
ARG SONAR_SCANNER_VERSION=4.8

#===============================================================================
# Builder
#===============================================================================

FROM erlang:${ERLANG_VERSION}-alpine AS build

# Set up configurable environment variables
ENV APP_NAME=sonar_erlang_example

# Install git for fetching non-hex depenencies.
#
# Add any other Alpine libraries needed to compile the project here.
# See https://wiki.alpinelinux.org/wiki/Local_APK_cache for details
# on the local cache and need for the symlink
RUN --mount=type=cache,id=apk-global,sharing=locked,target=/var/cache/apk \
    ln -s /var/cache/apk /etc/apk/cache && \
    apk update && \
    apk upgrade && \
    apk add --update build-base git openssh-client

# Set up workplace
WORKDIR /opt/app

# Prepare application dependencies
COPY rebar.config rebar.lock Makefile .
RUN make install-deps

# Compile the dependencies.
#
# Compiling the dependencies before `COPY . .` makes re-build faster. Without
# this step, Docker recompiles all dependencies when a file in
# graboxy-trap-server changes.
RUN --mount=type=cache,id=application,sharing=locked,target=/root/.cache/rebar3 \
    --mount=type=ssh \
    make compile

# Copy application code (excluding entries in .dockerignore!)
COPY . .

# Build application release
RUN --network=none \
    --mount=type=cache,id=application,sharing=locked,target=/root/.cache/rebar3 \
    make release

# Copy release to preparation room
RUN mkdir -p /opt/build && \
    cp -a /opt/app/_build/default/rel/${APP_NAME}/* /opt/build

#===============================================================================
# Elvis sidecar container
#===============================================================================

FROM erlang:${ERLANG_VERSION}-alpine AS elvis

# Set up workplace
WORKDIR /usr/src

# Install git for fetching non-hex depenencies.
RUN --mount=type=cache,id=apk-global,sharing=locked,target=/var/cache/apk \
    ln -s /var/cache/apk /etc/apk/cache && \
    apk update && \
    apk upgrade && \
    apk add --update git

# Clone the repo
RUN --mount=type=cache,id=elvis,sharing=locked,target=/root/.cache/rebar3 \
    git clone https://github.com/inaka/elvis .

# Fetch dependencies
RUN --mount=type=cache,id=elvis,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 get-deps

# Create an executable
RUN --network=none \
    --mount=type=cache,id=elvis,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 escriptize

#===============================================================================
# Run tests
#===============================================================================

FROM build AS test

# Copy Elvis static checker to this test image
COPY --from=elvis /usr/src/_build/default/bin/elvis /usr/local/bin/

# Run tests and static checks
RUN --network=none \
    --mount=type=cache,id=application,sharing=locked,target=/root/.cache/rebar3 \
    make docs compile xref dialyzer eunit ct cover elvis-check

#===============================================================================
# Sonar scanner
#===============================================================================

# Prepare
FROM sonarsource/sonar-scanner-cli:${SONAR_SCANNER_VERSION} AS sonar

# Copy build artifacts to `/usr/src` where `sonar-scanner-cli` image expects
# it.
COPY --from=test /opt/app /usr/src

#===============================================================================
# Runtime
#===============================================================================

FROM alpine:${ALPINE_VERSION} AS runtime

# Set up environment
ENV APP_NAME=sonar_erlang_example

# Install required tools, dependencies
#
# * `ncurses` is needed by ERTS
#
RUN --mount=type=cache,id=apk-global,sharing=locked,target=/var/cache/apk \
    ln -s /var/cache/apk /etc/apk/cache && \
    apk add --update ncurses

# Set up a destination for the application
WORKDIR /opt/app

# Set up secure defaults
RUN chown nobody:root -Rh /opt/app

# Copy release contents
COPY --from=build --chown=nobody:root /opt/build ./

# Make the runtime compatible with K8S's arbitrary user assignment
RUN chmod g+w -R /opt/app/releases/0.0.0

# Copy entrypoint
COPY docker/entrypoint.sh /

# Start application as a non-root user
USER nobody

# Set up default entrypoint and command
ENTRYPOINT ["/entrypoint.sh"]
CMD ["foreground"]
