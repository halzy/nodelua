. /usr/local/erlang/r15b01_64/activate
erl -sname nodelua -cookie nodelua -pa ebin deps/*/ebin -boot start_sasl -s nodelua -config `pwd`/start.config -args_file `pwd`/start.args
