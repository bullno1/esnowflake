REBAR=`which rebar`
all: deps compile
deps:
	@$(REBAR) get-deps
compile:
	@$(REBAR) compile
test:
	@$(REBAR) skip_deps=true eunit
release: compile
	@$(REBAR) generate
clean:
	@$(REBAR) skip_deps=true clean
