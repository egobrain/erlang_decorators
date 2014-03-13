REBAR= `which rebar || ./rebar`

build:
	@$(REBAR) compile skip_deps=true

deps:
	@$(REBAR) get-deps
	@$(REBAR) compile

eunit:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit
	
test: eunit	

clean:
	@$(REBAR) clean skip_deps=true

clean_all:
	@$(REBAR) clean

PHONY: build deps eunit clean clean_all test
