%%%-------------------------------------------------------------------
%%% @author anoskov
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Апр. 2016 15:21
%%%-------------------------------------------------------------------
-module(application_test).
-author("anoskov").

-include_lib("eunit/include/eunit.hrl").

supervisor_start_test() ->
  application:start(random_int_generator),
  ?assertNot(undefined == whereis(random_int_generator_sup)).

supervisor_stop_test() ->
  application:start(random_int_generator),
  application:stop(random_int_generator),
  ?assert(undefined == whereis(random_int_generator_sup)).