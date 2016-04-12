%%%-------------------------------------------------------------------
%%% @author anoskov
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Апр. 2016 22:33
%%%-------------------------------------------------------------------
-module(random_int_storage).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-author("anoskov").

%% API

%% Callbacks

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-record(state, {conn}).

%% ===================================================================
%% API
%% ===================================================================

%% ===================================================================
%% Callbacks
%% ===================================================================

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

init(Args) ->
  process_flag(trap_exit, true),
  Host = proplists:get_value(host, Args),
  Port = proplists:get_value(port, Args),
  DB = proplists:get_value(db, Args),
  {ok, Conn} = eredis:start_link(Host, Port, DB),
  {ok, #state{conn=Conn}}.

handle_call({lpush, Key, Value}, _From, #state{conn=Conn}=State) ->
  {reply, eredis:q(Conn, ["LPUSH", Key , Value]), State};

handle_call({sadd, Set, Value}, _From, #state{conn=Conn}=State) ->
  {reply, eredis:q(Conn, ["SADD", Set , Value]), State};

handle_call({lpop, Key}, _From, #state{conn=Conn}=State) ->
  {reply, eredis:q(Conn, ["LPOP", Key]), State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.