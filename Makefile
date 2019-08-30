#-------------------------------------------------------------------------------
# Copyright (C) 2019 Cursor Insight
#
# All rights reserved.
#-------------------------------------------------------------------------------

# Build tools
REBAR := $(shell which rebar3)

# Common directories and paths
TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
ABS_DIR := $(abspath $(TOP_DIR))
BUILD_DIR := $(TOP_DIR)/_build

# Specific Erlang flags that is compatible with this project
BEAM_FLAGS := ERL_FLAGS="$(ERL_FLAGS)"

# Default targets
.PHONY: all
all: test

.PHONY: test
test: docs compile xref dialyzer eunit ct cover

.PHONY: everything
everything: mrproper test

# Check for missing build tools
ifeq "$(strip $(REBAR))" ""
REBAR := rebar
endif

$(REBAR):
	@echo Please install \`$@\' manually!
	@exit 1

#-------------------------------------------------------------------------------
# Targets
#-------------------------------------------------------------------------------

.PHONY: mrproper
mrproper:
	rm -rf $(BUILD_DIR)

.PHONY: clean
clean: $(REBAR)
	$(REBAR) clean

.PHONY: deps
deps: $(REBAR)
	$(REBAR) upgrade

.PHONY: docs doc
docs: doc
doc: $(REBAR)
	$(REBAR) edoc

.PHONY: compile
compile: $(REBAR)
	$(REBAR) compile

.PHONY: xref
xref: $(REBAR)
	$(REBAR) xref

.PHONY: dialyzer
dialyzer: $(REBAR)
	$(REBAR) dialyzer

.PHONY: eunit
eunit: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) as mock eunit --cover

.PHONY: ct
ct: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) as mock ct --cover

.PHONY: ct-suite
ct-suite: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) as mock ct --cover --suite=$(SUITE)

.PHONY: retry-ct
retry-ct: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) as mock ct --cover --retry

.PHONY: cover
cover: $(REBAR)
	$(REBAR) as mock cover --verbose

.PHONY: reset-cover
reset-cover: $(REBAR)
	$(REBAR) as mock cover --reset

.PHONY: shell
shell: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) as mock shell
