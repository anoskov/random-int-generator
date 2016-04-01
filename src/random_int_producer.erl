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

%% API

-export([start_link/0, init/1]).

start_link() ->
  gen_server:start_link({global, server}, ?MODULE, [], []).

init(_Args) ->
  io:format("Initializing producer server...~n"),
  {ok, dict:new()}.

handle_call(_Args, _From, State) ->
  {reply, State}.

handle_cast(_Args, _From, State) ->
  {noreply, State}.

handle_info(Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) -> ok.