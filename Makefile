all:
	./rebar compile && ./rebar skip_deps=true eunit
clean:
	./rebar clean
