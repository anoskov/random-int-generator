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

-export([push/1, push_async/1, start_service/1]).
-export([start_link/0, init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([generator/0, generate_rand_int/1]).

-record(redis_conf, {host, port, db}).

-define(NUMBER_LIMIT, list_to_integer(os:getenv("NUMBER_LIMIT"))).
-define(NUM_UPPER_BOUND, list_to_integer(os:getenv("NUM_UPPER_BOUND"))).

-define(REDIS_HOST, os:getenv("REDIS_HOST")).
-define(REDIS_PORT, list_to_integer(os:getenv("REDIS_PORT"))).
-define(REDIS_DB, list_to_integer(os:getenv("REDIS_DB"))).
-define(REDIS_QUEUE_KEY, os:getenv("REDIS_QUEUE_KEY")).

%% ===================================================================
%% API
%% ===================================================================

start_service(Service) ->
  gen_server:call(?MODULE, { start, Service }).

push(Number) ->
  gen_server:call(?MODULE, { push, Number }).

push_async(Number) ->
  gen_server:cast(?MODULE, { push, Number }).


%% ===================================================================
%% Callbacks
%% ===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  io:format("Initializing producer server...~n"),
  process_flag(trap_exit, true),

  RC = #redis_conf{host = ?REDIS_HOST, port = ?REDIS_PORT, db = ?REDIS_DB},
  {ok, RedisPid} = eredis:start_link(RC#redis_conf.host, RC#redis_conf.port, RC#redis_conf.db),
  register(redis, RedisPid),

  {ok, dict:new()}.

handle_call({start, Service}, _From, State) ->
  case whereis(Service) of
    undefined ->
      Pid = spawn_link(?MODULE, Service, []),
      register(Service, Pid),
      { reply, ok, State };
    _Pid ->
      { reply, {error, "service already started!"}, State }
  end;

handle_call({push, Number}, _From, State) ->
  case whereis(redis) of
    undefined ->
      { reply, {error, "redis process not found"}, State };
    Pid ->
      eredis:q(Pid, ["LPUSH", ?REDIS_QUEUE_KEY , Number]),
      { reply, ok, State }
  end.

handle_cast({push, Number}, State) ->
  case whereis(redis) of
    undefined ->
      io:format("Redis client not initialized or died");
    Pid ->
      eredis:q(Pid, ["LPUSH", ?REDIS_QUEUE_KEY , Number])
  end,
  { noreply, State }.

terminate(shutdown, _State) -> ok.

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