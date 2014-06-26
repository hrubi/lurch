.PHONY: all compile clean test

REBAR_CONFIG := rebar.config
REBAR := ./rebar -C $(REBAR_CONFIG)

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) eunit
