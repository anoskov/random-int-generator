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

%% ===================================================================
%% Callbacks
%% ===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  io:format("Initializing producer server...~n"),
  process_flag(trap_exit, true),
  spawn_link(fun generator/0),
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

generate_rand_int(LB, UB) -> LB + random:uniform(UB - LB).

generator() ->
  RandInt = generate_rand_int(10, 20),
  io:format("Random integer: ~p~n", [RandInt]),
  timer:sleep(5000),
  generator().