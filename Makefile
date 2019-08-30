#-------------------------------------------------------------------------------
# Copyright (C) 2019 Cursor Insight
#
# All rights reserved.
#-------------------------------------------------------------------------------

# Build tools
REBAR := $(shell which rebar3)
ERL := $(shell which erl)

# Common directories and paths
TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
ABS_DIR := $(abspath $(TOP_DIR))
BUILD_DIR := $(TOP_DIR)/_build

# Specific Erlang flags that is compatible with this project
BEAM_FLAGS := ERL_FLAGS="$(ERL_FLAGS)"

# Necessary directories
TEST_LOGS_DIR := $(BUILD_DIR)/test/logs

# Cover files
COVER_DIR := $(BUILD_DIR)/test/cover
COVER_FILES := $(COVER_DIR)/eunit.coverdata $(COVER_DIR)/ct.coverdata

# Default targets
.PHONY: all
all: test

.PHONY: test
test: docs compile xref dialyzer eunit ct cover merge-cover

.PHONY: everything
everything: mrproper test

# Check for missing build tools
ifeq "$(strip $(REBAR))" ""
REBAR := rebar
endif

$(REBAR):
	@echo Please install \`$@\' manually!
	@exit 1

ifeq "$(strip $(ERL))" ""
ERL := erl
endif

$(ERL):
	@echo Please install \`$@\' manually!
	@exit 1

$(TEST_LOGS_DIR):
	@mkdir -p $(TEST_LOGS_DIR)

#-------------------------------------------------------------------------------
# Helpers
#-------------------------------------------------------------------------------

# Copied from erlang.mk's `core.mk`
define newline


endef

# Copied from erlang.mk's `core.mk`
define escape_dquotes
$(subst ",\",$1)
endef

# Copied from erlang.mk's `core.mk`
define erlang
$(ERL) $2 -eval "$(subst $(newline),,$(call escape_dquotes,$1))" -- erlang.mk
endef

# Copied from erlang.mk's `cover.mk`
define cover_export.erl
	$(foreach f,$(COVER_FILES),cover:import("$(f)") == ok orelse halt(1),)
	cover:export("$(COVER_DIR)/$@"), halt(0).
endef

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
xref: $(REBAR) $(TEST_LOGS_DIR)
	$(REBAR) xref | grep -o "Warning: .*" > $(TEST_LOGS_DIR)/xref.log

.PHONY: dialyzer
dialyzer: $(REBAR)
	$(REBAR) dialyzer

.PHONY: eunit
eunit: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) eunit --cover -v

.PHONY: ct
ct: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) ct --cover -v

.PHONY: ct-suite
ct-suite: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) ct --cover -v --suite=$(SUITE)

.PHONY: retry-ct
retry-ct: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) ct --cover -v --retry

.PHONY: cover
cover: $(REBAR)
	$(REBAR) cover --verbose

all.coverdata: $(REBAR)
	$(call erlang,$(cover_export.erl))

.PHONY: merge-cover
merge-cover: all.coverdata

.PHONY: reset-cover
reset-cover: $(REBAR)
	$(REBAR) cover --reset

.PHONY: sonar
sonar: $(SONAR_SCANNER)
	$(SONAR_SCANNER) -X

.PHONY: shell
shell: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) shell
