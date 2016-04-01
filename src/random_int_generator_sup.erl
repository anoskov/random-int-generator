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
  Producer =  {random_int_producer,
    {random_int_producer, start_link, []},
    permanent, 10000, worker,
    [random_int_producer]},
  RestartStrategy = {one_for_one, 5, 10},
  Childrens = [Producer],
  {ok, {RestartStrategy, Childrens}}.

