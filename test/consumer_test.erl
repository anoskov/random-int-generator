%%%-------------------------------------------------------------------
%%% @author anoskov
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Апр. 2016 15:36
%%%-------------------------------------------------------------------
-module(consumer_test).
-author("anoskov").

-include_lib("eunit/include/eunit.hrl").

-record(redis_conf, {host, port, db}).

-define(REDIS_HOST, "127.0.0.1").
-define(REDIS_PORT, 6379).
-define(REDIS_DB, 0).
-define(REDIS_QUEUE_KEY, "test1").
-define(REDIS_RESULT_SET_KEY, "result_test1").

eredis_start_test() ->
  RC = #redis_conf{host = ?REDIS_HOST, port = ?REDIS_PORT, db = ?REDIS_DB},
  Res = eredis:start_link(RC#redis_conf.host, RC#redis_conf.port, RC#redis_conf.db),
  ?assertMatch({ok, _}, Res),
  {ok, C} = Res,
  C.

eredis_flush_test() ->
  C = eredis_start_test(),
  ?assertMatch({ok, _}, eredis:q(C, ["FLUSHALL"])),
  C.

eredis_lpop_test() ->
  C = eredis_start_test(),
  ?assertMatch({ok, _}, eredis:q(C, ["LPOP", ?REDIS_QUEUE_KEY])).

eredis_sadd_test() ->
  C = eredis_start_test(),
  ?assertMatch({ok, _}, eredis:q(C, ["SADD", ?REDIS_RESULT_SET_KEY, 1])).

is_prime_test() ->
  ?assert(random_int_consumer:is_prime(10) == false),
  ?assert(random_int_consumer:is_prime(5) == true).

pusher_test() ->
  application:stop(random_int_generator),
  Number = 10000,
  C = eredis_flush_test(),
  PusherPid    = spawn_link(random_int_consumer, pusher, [C]),
  PusherPid ! {push, Number},
  timer:sleep(1000),
  {ok, Num} = eredis:q(C, ["SPOP", ?REDIS_RESULT_SET_KEY]),
  ?assertEqual(binary_to_integer(Num), Number).

filter_test() ->
  application:stop(random_int_generator),
  PrimeNumber = 5,
  Number = 10,
  C = eredis_flush_test(),
  PusherPid    = spawn_link(random_int_consumer, pusher, [C]),
  FilterPid    = spawn_link(random_int_consumer, filter, [PusherPid]),
  FilterPid ! {filter, PrimeNumber},
  FilterPid ! {filter, Number},
  timer:sleep(1000),
  {ok, Num} = eredis:q(C, ["SPOP", ?REDIS_RESULT_SET_KEY]),
  ?assertEqual(binary_to_integer(Num), PrimeNumber).