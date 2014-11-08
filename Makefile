.PHONY: all compile clean test deps

REBAR_CONFIG := rebar.config
REBAR := ./rebar -C $(REBAR_CONFIG)

all: compile

compile: deps/init rebar.config
	$(REBAR) compile

deps: deps/init

deps/init:
	$(REBAR) get-deps
	touch $@

clean:
	$(REBAR) clean

clean-deps:
	rm -rf deps

test: compile
	$(REBAR) eunit skip_deps=true

dialyze:
	dialyzer -I include --src -r src
