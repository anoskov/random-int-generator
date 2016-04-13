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

-export([push/1, handle_number/2, start_service/0]).
-export([start_link/0, init/1, terminate/2, handle_call/3, handle_cast/2, code_change/3]).
-export([generator/1, generate_rand_int/1]).

-define(NUMBER_LIMIT, list_to_integer(os:getenv("NUMBER_LIMIT"))).
-define(NUM_UPPER_BOUND, list_to_integer(os:getenv("NUM_UPPER_BOUND"))).

-define(REDIS_QUEUE_KEY, os:getenv("REDIS_QUEUE_KEY")).

%% ===================================================================
%% API
%% ===================================================================

start_service() ->
  gen_server:call(?MODULE, { start, generator }).


push(Number) ->
  poolboy:transaction(storage_pool, fun(Worker) ->
    gen_server:call(Worker, {lpush, ?REDIS_QUEUE_KEY, Number})
  end).

handle_number(Number, Count) ->
  case Count >= ?NUMBER_LIMIT of
    false ->
      push(Number),
      whereis(generator) ! {ok, Count};
    true ->
      push(Number),
      case ets:lookup(?MODULE, startIterTs) of
        [{_Key, StartIterTs}] ->
          SleepTime = 1000 - (get_timestamp() - StartIterTs),
          io:format("sleep time is ~p ", [SleepTime]),
          ets:insert(?MODULE, {startIterTs, get_timestamp() + SleepTime}),
          whereis(generator) ! {sleep, SleepTime};
        [] ->
          ok
      end
  end.

%% ===================================================================
%% Callbacks
%% ===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  process_flag(trap_exit, true),
  ets:new(?MODULE, [named_table, public]),

  {ok, dict:new()}.

handle_call({start, generator}, _From, State) ->
  case whereis(generator) of
    undefined ->
      ets:insert(?MODULE, {startIterTs, get_timestamp()}),
      Pid = spawn_link(?MODULE, generator, [0]),
      register(generator, Pid),
      { reply, ok, State };
    _Pid ->
      { reply, { error, io:format("service ~p already started!", [generator]) }, State }
  end.

handle_cast(_Msg, State) ->
  {noreply, State}.

terminate(shutdown, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ===================================================================
%% Functions
%% ===================================================================

generator(Count) ->
  handle_number(generate_rand_int(2), Count+1),
  receive
    {ok, NewCount} ->
      generator(NewCount);
    {sleep, Time} ->
      timer:sleep(Time),
      generator(0);
    _ ->
      ok
  end.

generate_rand_int(LB) -> LB + random:uniform(?NUM_UPPER_BOUND - LB).

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).