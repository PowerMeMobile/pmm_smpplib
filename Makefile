.PHONY: all compile deps clean distclean xref test

all: deps compile

compile:
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

xref:
	@./rebar xref

test:
	@./rebar eunit
