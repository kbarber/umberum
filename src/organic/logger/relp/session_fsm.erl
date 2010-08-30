%% --------------------------
%% @copyright 2010 Kenneth Barber
%% @doc This is a state based session handler for the RELP protocol
%%
%% @end
%% --------------------------
% 

-module(.organic.logger.relp.session_fsm).

-behaviour(gen_fsm).

-include_lib("include/common.hrl").

-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
    'SESSION_STARTUP'/2,
    'SESSION_TRANSMISSION'/2
]).

-record(state, {
                socket,    % client socket
                addr,      % client address
                syslog     % syslog process
                }).

-define(RELP_CAP, "~p rsp 92 200 OK~nrelp_version=0~nrelp_software=librelp,1.0.0,http://librelp.adiscon.com~ncommands=syslog~n").
-define(RELP_OK, "~p rsp 6 200 OK~n").

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
    {ok, 'SESSION_STARTUP', #state{socket=Socket}, ?CONF(relp_login_timeout)}.

%% --------------------------
%% @doc 
%%
%% @end
%% --------------------------
'SESSION_STARTUP'({open, #relp{txnr=Txnr}}, #state{socket=S})->
    Response = .lists:flatten(.io_lib:format(?RELP_CAP,[Txnr])),
    ?DEBUGF("SEND: ~p~n", [Response]),
    .gen_tcp:send(S, Response),
    {ok, SyslogPid} = .organic.logger.syslog_3164.decode_sup:start_client(),
    link(SyslogPid),
    {next_state, 'SESSION_TRANSMISSION', #state{socket=S,syslog=SyslogPid}};
'SESSION_STARTUP'({close, #relp{txnr=Txnr}}, #state{socket=S} = State)->
    Response = .lists:flatten(.io_lib:format(?RELP_OK, [Txnr])),
    ?DEBUGF("SEND: ~p~n",[Response]),
    .gen_tcp:send(S, Response),
    {stop, normal, State};
'SESSION_STARTUP'({Command, _PR}, State)->
    ?WARNF("Unknown RELP command received on socket: ", [Command]),
    {stop, normal, State};
'SESSION_STARTUP'(timeout, #state{socket=S} = State) ->
    Response = "0 serverclose 0~n",
    ?DEBUGF("SEND: ~p ~n", [Response]),
    .gen_tcp:send(S, Response),
    {stop, normal, State};
'SESSION_STARTUP'(_Other,State) ->
    {stop, normal, State}.

%% --------------------------
%% @doc 
%%
%% @end
%% --------------------------
'SESSION_TRANSMISSION'({syslog, #relp{data=Data, txnr=Txnr}}, #state{socket=S,syslog=Syslog} = State)->
    ?DEBUGF("RECV: ~p~n",[Data]),
    .gen_fsm:send_event(Syslog, {msg, Data}),
    Response = .lists:flatten(.io_lib:format(?RELP_OK, [Txnr])),
    ?DEBUGF("SEND: ~p~n",[Response]),
    .gen_tcp:send(S, Response),
    {next_state, 'SESSION_TRANSMISSION', State};
'SESSION_TRANSMISSION'({close, #relp{txnr=Txnr}}, #state{socket=S} = State) ->
    Response = .lists:flatten(.io_lib:format(?RELP_OK, [Txnr])),
    ?DEBUGF("SEND: ~p~n",[Response]),
    .gen_tcp:send(S, Response),
    {stop, normal, State};
'SESSION_TRANSMISSION'(_Other, State) ->
    {next_state, 'SESSION_TRANSMISSION', State}.


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
handle_info({'EXIT',From,_}, StateName, #state{socket=Socket,syslog=Syslog} = StateData) -> 
    % This is how we receive signals from the connection process when it stops
    % In this case, we just stop as well.
    case From of
	Syslog ->
	    {ok, SyslogPid} = .organic.logger.relp.syslog_sup:start_client(Socket),
	    link(SyslogPid),
	    {next_state, StateName, #state{socket=Socket,syslog=SyslogPid}};
	_Other -> 
	    {stop, normal, StateData}
    end;
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
