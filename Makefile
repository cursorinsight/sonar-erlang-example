#-------------------------------------------------------------------------------
# Copyright (C) 2019- Cursor Insight
#
# All rights reserved.
#-------------------------------------------------------------------------------

# Common directories and paths
TOP_DIR := $(dir $(firstword $(MAKEFILE_LIST)))
ABS_DIR := $(abspath $(TOP_DIR))

# Specify default target
.PHONY: default
default:

# Check for and set up required tools
define set-up-tools-template
$(1) := $$(shell which $(2))

ifeq "$$(strip $$($(1)))" ""
$(1) := $(2)
endif

$$($(1)):
	@echo Please install \`$$@\' manually!
	@exit 1
endef

# Set up necessary tools
$(eval $(call set-up-tools-template,ELVIS,elvis))
$(eval $(call set-up-tools-template,ERL,erl))
$(eval $(call set-up-tools-template,GIT,git))
$(eval $(call set-up-tools-template,GREP,grep))
$(eval $(call set-up-tools-template,MKDIR,mkdir))
$(eval $(call set-up-tools-template,REBAR,rebar3))
$(eval $(call set-up-tools-template,SONAR_SCANNER,sonar-scanner))
$(eval $(call set-up-tools-template,TEE,tee))

# Project version
ifneq (,$(wildcard .git))
PROJECT_VERSION := $(shell $(GIT) describe --always --dirty --broken --long --tags)
else
# There is no .git directory, so we cannot call Git to calculate the project
# version.
PROJECT_VERSION := no-project-version
endif

# Common build directory
BUILD_DIR := $(TOP_DIR)/_build

# Specific Erlang flags that is compatible with this project
BEAM_FLAGS := ERL_FLAGS="$(ERL_FLAGS)"
ELVIS_FLAGS := $(ELVIS_FLAGS) -c $(TOP_DIR)/elvis.config --output-format parsable

# Necessary directories
TEST_LOGS_DIR := $(BUILD_DIR)/test/logs

# Cover files
COVER_DIR := $(BUILD_DIR)/test/cover
COVER_FILES := $(COVER_DIR)/eunit.coverdata $(COVER_DIR)/ct.coverdata

# Default targets
.PHONY: all
all: test

.PHONY: test
test: docs compile xref dialyzer eunit ct cover elvis-check

.PHONY: everything
everything: mrproper test

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
	$(RM) -r $(BUILD_DIR) $(ABS_DIR)/doc $(ABS_DIR)/data.* \
		$(ABS_DIR)/log.* $(ABS_DIR)/.scannerwork

.PHONY: clean
clean: $(REBAR)
	$(REBAR) clean

.PHONY: install-deps
install-deps: $(REBAR)
	$(REBAR) get-deps

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
	$(REBAR) dialyzer > $(TEST_LOGS_DIR)/dialyzer.txt

.PHONY: eunit
eunit: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) eunit --cover -v

.PHONY: ct
ct: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) ct --cover -v

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

.PHONY: elvis-check
elvis-check: $(ELVIS) #: Run the Elvis style reviewer.
	$(ELVIS) $(ELVIS_FLAGS) rock > $(TEST_LOGS_DIR)/elvis_rock.txt || exit 0

.PHONY: release
release: $(REBAR)
	$(BEAM_FLAGS) $(REBAR) release
