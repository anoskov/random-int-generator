-module(random_int_generator_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, _StartArgs) ->
    random_int_generator_sup:start_link().

stop(_State) ->
    ok.
