%%%-------------------------------------------------------------------
%%% @author anoskov
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Апр. 2016 13:39
%%%-------------------------------------------------------------------
-module(random_int_consumer).

-behavior(gen_server).

-author("anoskov").

-export([start_link/0, init/1, terminate/2]).

-export([consumer/2, filter/1, pusher/1]).

-record(redis_conf, {host, port, db}).

-define(REDIS_HOST, os:getenv("REDIS_HOST")).
-define(REDIS_PORT, list_to_integer(os:getenv("REDIS_PORT"))).
-define(REDIS_DB, list_to_integer(os:getenv("REDIS_DB"))).
-define(REDIS_QUEUE_KEY, os:getenv("REDIS_QUEUE_KEY")).
-define(REDIS_RESULT_SET_KEY, os:getenv("REDIS_RESULT_SET_KEY")).

%% ===================================================================
%% Callbacks
%% ===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  io:format("Initializing consumer server...~n"),
  process_flag(trap_exit, true),

  RC = #redis_conf{host = ?REDIS_HOST, port = ?REDIS_PORT, db = ?REDIS_DB},
  {ok, RedisClient} = eredis:start_link(RC#redis_conf.host, RC#redis_conf.port, RC#redis_conf.db),

  PusherPid      = spawn_link(?MODULE, pusher, [RedisClient]),
  FilterPid      = spawn_link(?MODULE, filter, [PusherPid]),
  ConsumerPid    = spawn_link(?MODULE, consumer, [RedisClient, FilterPid]),

  {ok, dict:new()}.

terminate(shutdown, _State) -> ok.

%% ===================================================================
%% Functions
%% ===================================================================

consumer(RedisClient, FilterPid) ->
  case eredis:q(RedisClient, ["LPOP", ?REDIS_QUEUE_KEY]) of
    {ok, undefined} ->
      ok;
    {ok, Number} ->
      FilterPid ! {filter, binary_to_integer(Number)}
  end,
  consumer(RedisClient, FilterPid).


filter(PusherPid) ->
  receive
    {filter, Number} ->
      case isPrime(Number) of
        true ->
          PusherPid ! {push, Number};
        false ->
          ok
      end,
      filter(PusherPid)
  end.

pusher(RedisClient) ->
  receive
    {push, Number} ->
      case eredis:q(RedisClient, ["SADD", ?REDIS_RESULT_SET_KEY, Number]) of
        {ok, _} ->
          ok
      end,
      pusher(RedisClient)
  end.

isPrime(N,M) when N == M -> true;
isPrime(N,M) when N rem M == 0 -> false;
isPrime(N,M) -> isPrime(N,M+1).

isPrime(N) when N < 2 -> false;
isPrime(N) when N rem 1 == 0 -> isPrime(N,2).