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

-export([push/1, start_service/1, start_service/0]).
-export([start_link/0, init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([generator/0, generate_rand_int/1]).

-define(NUMBER_LIMIT, list_to_integer(os:getenv("NUMBER_LIMIT"))).
-define(NUM_UPPER_BOUND, list_to_integer(os:getenv("NUM_UPPER_BOUND"))).

-define(REDIS_QUEUE_KEY, os:getenv("REDIS_QUEUE_KEY")).

%% ===================================================================
%% API
%% ===================================================================

start_service() ->
  start_service(generator).

start_service(Service) ->
  gen_server:call(?MODULE, { start, Service }).


push(Number) ->
  poolboy:transaction(storage_pool, fun(Worker) ->
    gen_server:call(Worker, {lpush, ?REDIS_QUEUE_KEY, Number})
  end).

%% ===================================================================
%% Callbacks
%% ===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  process_flag(trap_exit, true),
  log_record(info, "producer server started~n"),
  {ok, dict:new()}.

handle_call({start, Service}, _From, State) ->
  case whereis(Service) of
    undefined ->
      Pid = spawn_link(?MODULE, generator, []),
      register(Service, Pid),
      log_record(info, io:format("service ~p is started!~n", [Service])),
      { reply, ok, State };
    _Pid ->
      log_record(error, io:format("service ~p already started!~n", [Service])),
      { reply, { error, io:format("service ~p already started!", [Service]) }, State }
  end.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({info, Msg}, State) ->
  io:format("Message from module \"~p\" : ~p~n", [?MODULE, Msg]),
  {noreply, State};

handle_info({error, Msg}, State) ->
  error_logger:error_msg("An error occurred in module \"~p\" : ~p~n", [?MODULE, Msg]),
  {noreply, State}.

terminate(shutdown, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ===================================================================
%% Functions
%% ===================================================================

generator() ->
  receive
%%    {CtrlPid, {ok, Count}} ->
%%      CtrlPid ! {self(), {generate_rand_int(2), Count+1}},
%%      generator(CtrlPid);
%%    {CtrlPid, {sleep, Time}} ->
%%      timer:sleep(Time),
%%      CtrlPid ! {self(), {generate_rand_int(2), 1}},
%%      generator(CtrlPid);
    _ ->
      ok,
      generator()
  end.

%%controller(PusherPid) ->
%%  receive
%%    {run, GeneratorPid} ->
%%      put(startIterationTimestamp, get_timestamp()),
%%      GeneratorPid ! {self(), {ok, 0}},
%%      controller(PusherPid);
%%    {From, {Number, Count}} ->
%%      case Count >= ?NUMBER_LIMIT of
%%        false ->
%%          PusherPid ! {push, Number},
%%          From ! {self(), {ok, Count}};
%%        true ->
%%          PusherPid ! {push, Number},
%%          SleepTime = 1000 - (get_timestamp() - erase(startIterationTimestamp)),
%%          put(startIterationTimestamp, get_timestamp() + SleepTime),
%%          From ! {self(), {sleep, SleepTime}}
%%      end,
%%      controller(PusherPid)
%%  end.


generate_rand_int(LB) -> LB + random:uniform(?NUM_UPPER_BOUND - LB).

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

log_record(Level, Msg) ->
  self() ! {Level, Msg}.