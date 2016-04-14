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
-behaviour(poolboy_worker).

-author("anoskov").

-export([push/1, handle_number/2, start_service/0]).
-export([start_link/1, init/1, terminate/2, handle_call/3, handle_cast/2, code_change/3]).
-export([generator/1, generate_rand_int/1]).

%% ===================================================================
%% API
%% ===================================================================

start_service() ->
  poolboy:transaction(producer_pool, fun(Worker) ->
    gen_server:call(Worker, { start, generator })
  end).

push(Number) ->
  poolboy:transaction(storage_pool, fun(Worker) ->
    gen_server:call(Worker, {lpush, fetch_ets_key(redis_queue_key), Number})
  end).

handle_number(Number, Count) ->
  case Count >= fetch_ets_key(number_limit) of
    false ->
      push(Number),
      whereis(generator) ! {ok, Count};
    true ->
      push(Number),
      case ets:lookup(?MODULE, startIterTs) of
        [{_Key, StartIterTs}] ->
          SleepTime = 1000 - (get_timestamp() - StartIterTs),
          io:format("sleep time is ~p ~n ", [SleepTime]),
          ets:insert(?MODULE, {startIterTs, get_timestamp() + SleepTime}),
          whereis(generator) ! {sleep, SleepTime};
        [] ->
          ok
      end
  end.

%% ===================================================================
%% Callbacks
%% ===================================================================

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

init(Args) ->
  process_flag(trap_exit, true),
  ets:new(?MODULE, [named_table, public]),
  ets:insert(?MODULE, {number_limit, proplists:get_value(number_limit, Args)}),
  ets:insert(?MODULE, {num_upper_bound, proplists:get_value(num_upper_bound, Args)}),
  ets:insert(?MODULE, {redis_queue_key, proplists:get_value(redis_queue_key, Args)}),

 % start_service(),
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

generate_rand_int(LB) -> LB + random:uniform(fetch_ets_key(num_upper_bound) - LB).

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

fetch_ets_key(Key) ->
  [{_Key, Value}] = ets:lookup(?MODULE, Key),
  Value.