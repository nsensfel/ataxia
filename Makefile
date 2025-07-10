################################################################################
## USER CONFIGURATION OPTIONS ##################################################
################################################################################
## Main Directories
SRC_DIR ?= ${CURDIR}/src
CONFIG_DIR ?= ${CURDIR}/conf

## Optional Directories
BIN_DIR ?= ${CURDIR}/ebin
INCLUDE_DIR ?= ${CURDIR}/include

## Local only?
#ERL_NAME_VS_SNAME ?= -name
ERL_NAME_VS_SNAME ?= -sname

## Binaries
ERLC ?= erlc
ERLC_OPTS ?= -I $(INCLUDE_DIR)

DIALYZER ?= dialyzer
DIALYZER_OPTS ?= -I $(INCLUDE_DIR)

M4 ?= m4
M4_OPTS ?=

## Filenames
DIALYZER_BASE_PLT_FILE ?= base.plt
DIALYZER_PLT_FILE ?= ataxia.plt

################################################################################
## MAKEFILE MAGIC ##############################################################
################################################################################
## General
OPTIONAL_DIRS = $(BIN_DIR) $(INCLUDE_DIR)

## Preprocessor
PREPROCESSOR_CONFIG_FILES = $(shell find -L $(CONFIG_DIR) -name "*.m4.conf")
PREPROCESSABLE_FILES = $(shell find -L ${CURDIR} -name "*.m4")
PREPROCESSED_FILES = $(patsubst %.m4,%,$(PREPROCESSABLE_FILES))

## Erlang
PREPROCESSED_ERL_SRC_FILES = $(filter %.erl,$(PREPROCESSED_FILES))
CURRENT_ERL_SRC_FILES = $(shell find -L $(SRC_DIR) -name "*.erl")
ERL_SRC_FILES = \
	$(filter-out $(PREPROCESSED_ERL_SRC_FILES),$(CURRENT_ERL_SRC_FILES)) \
	$(PREPROCESSED_ERL_SRC_FILES)
ERL_BIN_FILES = $(patsubst $(SRC_DIR)%.erl,$(BIN_DIR)/%.beam,$(ERL_SRC_FILES))

## Binaries
ERLC_EXEC = $(ERLC) $(ERLC_OPTS)
ERL_EXEC = $(ERL) $(ERL_OPTS)
DIALYZER_EXEC = $(DIALYZER) $(DIALYZER_OPTS)
M4_EXEC = $(M4) $(M4_OPTS)

################################################################################
## SANITY CHECKS ###############################################################
################################################################################

################################################################################
## PREPROCESSOR-VISIBLE MAKEFILE VARIABLES #####################################
################################################################################
MAKEFILE_TO_M4 = \
	--define=__MAKEFILE_BIN_DIR=$(BIN_DIR) \
	--define=__MAKEFILE_LOG_DIR=$(LOG_DIR) \
	--define=__MAKEFILE_INCLUDE_DIR=$(INCLUDE_DIR)

################################################################################
## TARGET RULES ################################################################
################################################################################
all: build

debug: debug_run

build: $(OPTIONAL_DIRS)	$(PREPROCESSED_FILES) $(ERL_BIN_FILES)

clean:
	# Preprocessor
	rm -rf $(PREPROCESSED_FILES)
	# Erlang
	rm -rf $(BIN_DIR)/*

reset:
	# Preprocessor
	rm -rf $(PREPROCESSED_FILES)
	rm -rf $(OPTIONAL_DIRS)

################################################################################
## INTERNAL RULES ##############################################################
################################################################################
debug_rebuild:
	$(MAKE) clean
	$(MAKE) ERLC_OPTS="$(ERLC_OPTS) +debug_info"

$(DIALYZER_BASE_PLT_FILE):
	- $(DIALYZER_EXEC) --build_plt --apps erts kernel stdlib crypto \
		--output_plt $@

debug_run: $(DIALYZER_BASE_PLT_FILE)
	$(MAKE) debug_rebuild
	cp $< $(DIALYZER_PLT_FILE)
	$(DIALYZER_EXEC) --add_to_plt --get_warnings --plt $(DIALYZER_PLT_FILE) -r $(BIN_DIR)
	$(DIALYZER_EXEC) --check_plt --plt $(DIALYZER_PLT_FILE)
	$(DIALYZER_EXEC) --get_warnings $(ERL_SRC_FILES) $(PREPROCESSED_ERL_SRC_FILES)\
		--src --plt $(DIALYZER_PLT_FILE)

$(PREPROCESSED_FILES): %: %.m4 .PHONY
	$(M4_EXEC) -P $(MAKEFILE_TO_M4) $(PREPROCESSOR_CONFIG_FILES) $< > $@

$(OPTIONAL_DIRS): %:
	mkdir -p $@

$(ERL_BIN_FILES): $(BIN_DIR)/%.beam: $(SRC_DIR)/%.erl
	mkdir -p $(dir $@)
	$(ERLC_EXEC) -o $(dir $@) $<

.PHONY:

