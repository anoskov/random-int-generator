-module(random_int_generator_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, Pools} = application:get_env(random_int_generator, pools),
  PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
    PoolArgs = [{name, {local, Name}},
      {worker_module, random_int_storage}] ++ SizeArgs,
    poolboy:child_spec(Name, PoolArgs, WorkerArgs)
  end, Pools),

%%  Producer =  {random_int_producer,
%%    {random_int_producer, start_link, []},
%%    permanent, 10000, worker,
%%    [random_int_producer]},
%%  Consumer =  {random_int_consumer,
%%    {random_int_consumer, start_link, []},
%%    permanent, 10000, worker,
%%    [random_int_consumer]},
%%  Childrens = [Producer, Consumer],

  RestartStrategy = {one_for_one, 5, 10},
  {ok, {RestartStrategy, PoolSpecs}}.

