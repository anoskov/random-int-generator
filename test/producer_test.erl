%%%-------------------------------------------------------------------
%%% @author anoskov
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Апр. 2016 15:36
%%%-------------------------------------------------------------------
-module(producer_test).
-author("anoskov").

-include_lib("eunit/include/eunit.hrl").

-record(redis_conf, {host, port, db}).

-define(NUMBER_LIMIT, 3000).
-define(NUM_UPPER_BOUND, 50).

-define(REDIS_HOST, "127.0.0.1").
-define(REDIS_PORT, 6379).
-define(REDIS_DB, 0).
-define(REDIS_QUEUE_KEY, "test1").

generate_rand_int_test() ->
  RandInt = random_int_producer:generate_rand_int(2),
  ?assert(is_integer(RandInt) == true),
  ?assert((RandInt > 2) and (RandInt < ?NUM_UPPER_BOUND) == true).

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

eredis_push_test() ->
  C = eredis_start_test(),
  ?assertMatch({ok, _}, eredis:q(C, ["LPUSH", ?REDIS_QUEUE_KEY , 1])).

pusher_test() ->
  application:stop(random_int_generator),
  Number = 10000,
  C = eredis_flush_test(),
  PusherPid    = spawn_link(random_int_producer, pusher, [C]),
  PusherPid ! {push, Number},
  timer:sleep(1000),
  {ok, Num} = eredis:q(C, ["RPOP", ?REDIS_QUEUE_KEY]),
  ?assertEqual(binary_to_integer(Num), Number).