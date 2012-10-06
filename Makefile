
REBAR=./rebar
.PHONY: all deps compile clean test xref

all: deps compile test
deps:
	@$(REBAR) get-deps
compile:
	@$(REBAR) compile
test:
	@$(REBAR) skip_deps=true eunit
clean:
	@$(REBAR) clean

xref:
	@$(REBAR) xref skip_deps=true

# USE_GDB=true
# export DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib
# cd ebin && erl -noshell -pa ../.eunit/ -eval "eunit:test(nodelua, [verbose])" -s init stop
