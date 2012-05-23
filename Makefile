all:
	./rebar compile && ./rebar skip_deps=true eunit
clean:
	./rebar clean

# USE_GDB=true
# export DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib
# cd ebin && erl -noshell -pa ../.eunit/ -eval "eunit:test(nodelua, [verbose])" -s init stop
