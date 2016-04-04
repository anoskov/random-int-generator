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

-export([start_link/0, init/1, terminate/2, generator/1, controller/0]).

%% ===================================================================
%% Callbacks
%% ===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  io:format("Initializing producer server...~n"),
  process_flag(trap_exit, true),
  CtrlPid = spawn_link(?MODULE, controller, []),
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
      CtrlPid ! {self(), {generate_rand_int(10, 20), Count+1}},
      generator(CtrlPid);
    {CtrlPid, {sleep, Time}} ->
      timer:sleep(Time),
      CtrlPid ! {self(), {generate_rand_int(10, 20), 1}},
      generator(CtrlPid);
    _ ->
      ok
  end.

controller() ->
  receive
    {run, GeneratorPid} ->
      GeneratorPid ! {self(), {ok, 0}},
      controller();
    {From, {Number, Count}} ->
      io:format("Random integer: ~p~n", [Number]),
      io:format("Number count:   ~p~n", [Count]),
      io:format("Numbers Length:   ~p~n", [application:get_env(random_int_generator_app, numbersRange)]),
      case Count >= 5 of
        false ->
          From ! {self(), {ok, Count}};
        true ->
          From ! {self(), {sleep, 10000}}
      end,
      controller()
  end.

generate_rand_int(LB, UB) -> LB + random:uniform(UB - LB).