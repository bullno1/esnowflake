PROJECT_PLT=$(CURDIR)/.project_plt
 
# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
ERL = $(shell which erl)
 
ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif
 
REBAR=$(shell which rebar)
 
ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif
 
.PHONY: all compile clean test dialyzer typer shell distclean pdf \
	get-deps escript clean-common-test-data
 
all: compile
 
get-deps:
	$(REBAR) get-deps
	$(REBAR) compile
 
compile:
	$(REBAR) skip_deps=true compile
 
eunit: compile clean-common-test-data
	$(REBAR) skip_deps=true eunit
 
ct: compile clean-common-test-data
	$(REBAR) skip_deps=true ct
 
test: compile eunit ct
 
$(PROJECT_PLT):
	@echo Building local plt at $(PROJECT_PLT)
	@echo
	dialyzer --output_plt $(PROJECT_PLT) --build_plt \
	   --apps erts kernel stdlib -r deps
 
dialyzer: $(PROJECT_PLT)
	dialyzer --plt $(PROJECT_PLT) --fullpath -Wrace_conditions \
	-I $(CURDIR)/deps -pa $(CURDIR)/ebin --src src
 
typer:
	typer --plt $(PROJECT_PLT) -r ./src
 
clean-common-test-data:
# We have to do this because of the unique way we generate test
# data. Without this rebar eunit gets very confused
	- rm -rf $(CURDIR)/test/*_SUITE_data
 
clean: clean-common-test-data
	- rm -rf $(CURDIR)/test/*.beam
	- rm -rf $(CURDIR)/logs
	$(REBAR) skip_deps=true clean
 
distclean: clean
	- rm -rf $(PROJECT_PLT)
	- rm -rvf $(CURDIR)/deps/*
