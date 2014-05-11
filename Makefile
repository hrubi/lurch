.PHONY: all compile clean

REBAR_CONFIG := rebar.config
REBAR := ./rebar -C $(REBAR_CONFIG)

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

tests:
	$(REBAR) eunit
