%% --------------------------
%% @copyright 2010 Kenneth Barber
%% @doc This processes RFC3164 BSD syslog messages
%%
%% @end
%% --------------------------
% 

-module(.organic.logger.syslog_3164.decode_fsm).

-behaviour(gen_fsm).

-include_lib("include/data.hrl").

-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
    'RECEIVE'/2
]).

-record(state, {
                socket,    % client socket
                addr,       % client address
	        router      % route pid
               }).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @spec (Socket) -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%      If init/1 fails with Reason, the function returns {error,Reason}.
%%      If init/1 returns {stop,Reason} or ignore, the process is
%%      terminated and the function returns {error,Reason} or ignore,
%%      respectively.
%% @end
%%-------------------------------------------------------------------------
start_link(Socket) ->
    .gen_fsm:start_link(?MODULE, [Socket], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%% --------------------------
%% @doc 
%%
%% @end
%% --------------------------
init([Socket]) ->
    .process_flag(trap_exit, true),
    {ok, RoutePid} = .organic.logger.route.sup:start_client(self()), % borked
    link(RoutePid),
    {ok, 'RECEIVE', #state{socket=Socket, router=RoutePid}}.

%% --------------------------
%% @doc 
%%
%% @end
%% --------------------------
'RECEIVE'({msg, Data}, #state{router=Router} = State)->
    HeaderRe = "^<(\\d{1,3})>(.{32})\\s(.+)\\s(.+):\\s(.*?)",
    ReOpts = [unicode,{capture,all,binary},dotall, ungreedy],
    case .re:run(Data,HeaderRe,ReOpts) of
	{match, [_All,Priority,Timestamp,Hostname,Tag,Content]} -> 
	    SR = #syslog{
	      facility = .organic.util:bin_to_int(Priority) div 8,
	      severity = .organic.util:bin_to_int(Priority) rem 8,
	      timestamp = binary_to_list(Timestamp),
	      hostname = binary_to_list(Hostname),
	      tag = binary_to_list(Tag),
	      content = Content},

	    .gen_fsm:send_event(Router, {log, SR}),
	    ok;
	{match, _Capture} -> ok; %TODO:proper loggin
	nomatch -> ok;
        _Other -> ok
    end,
    %.io:format("DATA: ~p~n", [Data]),
    {next_state, 'RECEIVE', State};
'RECEIVE'(_Msg,State) ->
    {next_state, 'RECEIVE', State}.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({'EXIT',From,_}, StateName, #state{router=Router} = StateData) -> 
    % This is how we receive signals from the route process when it stops
    % In this case, we just stop as well.
    case From of
	Router ->
	    {ok, RouterPid} = .organic.logger.route.sup:start_client(self()),
	    link(RouterPid),
	    {next_state, StateName, #state{router=RouterPid}};
	_Other -> 
	    {stop, normal, StateData}
    end;
handle_info({'EXIT',_,_}, _StateName, StateData) -> 
    % This is how we receive signals from the route process when it stops
    % In this case, we just stop as well.
    {stop, normal, StateData};
handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, _StateData) ->
    ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.