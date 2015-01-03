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
	$(REBAR) delete-deps
	rm deps/init

test: compile
	ERL_AFLAGS="-kernel error_logger silent -lurch autostart_devices false" $(REBAR) eunit skip_deps=true

dialyze:
	dialyzer -I include --src -r src
