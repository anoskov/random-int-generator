# Random Integer generator and natural number filter

## Compile

$ rebar get-deps

$ rebar compile

## Create a release

$ mkdir rel

$ cd rel

$ rebar create-node nodeid=random_int_generator

add {lib_dirs, ["../../", "../deps/"]} to rel/reltool.config

$ cd ..

$ rebar compile generate

## Prepare application
$ . scripts/export_env.sh

## Start

### start application

$ ./rel/random_int_generator/bin/random_int_generator start

### stop application

$ ./rel/random_int_generator/bin/random_int_generator stop

### start application from command line

$ erl -pa ebin/ deps/*/ebin -eval "application:start(random_int_generator)"


