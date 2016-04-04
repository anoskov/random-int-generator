%%%-------------------------------------------------------------------
%%% @author anoskov
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Апр. 2016 17:39
%%%-------------------------------------------------------------------
-module(random_int_producer).

-behavior(gen_server).

-author("anoskov").

-export([start_link/0, init/1, terminate/2]).

-export([generator/1, controller/1, pusher/1]).

-record(redis_conf, {host, port, db}).

-define(NUMBER_LIMIT, list_to_integer(os:getenv("NUMBER_LIMIT"))).
-define(NUM_UPPER_BOUND, list_to_integer(os:getenv("NUM_UPPER_BOUND"))).

-define(REDIS_HOST, os:getenv("REDIS_HOST")).
-define(REDIS_PORT, list_to_integer(os:getenv("REDIS_PORT"))).
-define(REDIS_DB, os:getenv("REDIS_DB")).
-define(REDIS_QUEUE_KEY, os:getenv("REDIS_QUEUE_KEY")).

%% ===================================================================
%% Callbacks
%% ===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  io:format("Initializing producer server...~n"),
  process_flag(trap_exit, true),

  RC = #redis_conf{host = ?REDIS_HOST, port = ?REDIS_PORT, db = ?REDIS_DB},
  {ok, RedisClient} = eredis:start_link(),

  PusherPid    = spawn_link(?MODULE, pusher, [RedisClient]),
  CtrlPid      = spawn_link(?MODULE, controller, [PusherPid]),
  GeneratorPid = spawn_link(?MODULE, generator, [CtrlPid]),

  CtrlPid ! {run, GeneratorPid},

  {ok, dict:new()}.

handle_call(_Args, _From, State) ->
  {reply, State}.

handle_cast(stop, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(shutdown, _State) -> ok.

%% ===================================================================
%% Functions
%% ===================================================================

generator(CtrlPid) ->
  receive
    {CtrlPid, {ok, Count}} ->
      CtrlPid ! {self(), {generate_rand_int(2), Count+1}},
      generator(CtrlPid);
    {CtrlPid, {sleep, Time}} ->
      timer:sleep(Time),
      CtrlPid ! {self(), {generate_rand_int(2), 1}},
      generator(CtrlPid);
    _ ->
      ok
  end.

controller(PusherPid) ->
  receive
    {run, GeneratorPid} ->
      put(startIterationTimestamp, get_timestamp()),
      GeneratorPid ! {self(), {ok, 0}},
      controller(PusherPid);
    {From, {Number, Count}} ->
      case Count >= ?NUMBER_LIMIT of
        false ->
          PusherPid ! {self(), { push, Number }},
          From ! {self(), {ok, Count}};
        true ->
          PusherPid ! {self(), { push, Number }},
          SleepTime = 1000 - (get_timestamp() - erase(startIterationTimestamp)),
          From ! {self(), {sleep, SleepTime}},
          put(startIterationTimestamp, get_timestamp() + SleepTime)
      end,
      controller(PusherPid);
    {PusherPid, {ok, stored}} ->
      ok
  end.

pusher(RedisClient) ->
  receive
    {From, {push, Number}} ->
      case eredis:q(RedisClient, ["LPUSH", ?REDIS_QUEUE_KEY , Number]) of
        {ok, _} ->
          From ! {ok, stored};
        _ ->
          From ! {error}
      end,
      pusher(RedisClient)
  end.


generate_rand_int(LB) -> LB + random:uniform(?NUM_UPPER_BOUND - LB).

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).