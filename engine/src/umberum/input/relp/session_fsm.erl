%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc This is a state based session handler for the RELP protocol
%%
%% @end
%% --------------------------
% 

-module(.umberum.input.relp.session_fsm).

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

%% State record
-record(state, {
                socket,    % client socket
                addr,      % client address
                syslog,     % syslog process
                session_id  % unique ID for the session
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
%% @doc Session startup state.
%%
%% This receives decoded RELP commands and data from con_fsm processes prior
%% to session startup.
%%
%% open - new session
%% close - close session
%%
%% @end
%% --------------------------
'SESSION_STARTUP'({open, #relp{txnr=Txnr}}, #state{socket=S})->
    %% This message type indicates the client wishes to open a new session.

    % Assign uuid for session and log it
    Id = .uuid:v4(),
    ?INFOF("New RELP session started. [sessionid=~p]~n",[.uuid:to_string(Id)]),

    % Send capabilities announcement back to peer
    Response = .lists:flatten(.io_lib:format(?RELP_CAP,[Txnr])),
    ?DEBUGF("SEND: ~p~n", [Response]),
    .gen_tcp:send(S, Response),

    % Start a syslog_3164 decoder and link to it
    {ok, SyslogPid} = .umberum.logger.syslog_3164.decode_sup:start_client(),
    link(SyslogPid),

    {next_state, 'SESSION_TRANSMISSION', #state{socket=S,syslog=SyslogPid,session_id=Id}};
'SESSION_STARTUP'({close, #relp{txnr=Txnr}}, #state{socket=S} = State)->
    %% This message type indicates the client wishes to close the connection.

    Response = .lists:flatten(.io_lib:format(?RELP_OK, [Txnr])),
    ?DEBUGF("SEND: ~p~n",[Response]),
    .gen_tcp:send(S, Response),
    {stop, normal, State};
'SESSION_STARTUP'({Command, _PR}, State)->
    %% Unknown and unsupported command. The standard states to close in this 
    %% case.
    ?WARNF("Unknown RELP command received on socket: ", [Command]),
    {stop, normal, State};
'SESSION_STARTUP'(timeout, #state{socket=S} = State) ->
    %% If the connected user does not connect before a timeout triggers
    %% this executes.
    Response = "0 serverclose 0~n",
    ?DEBUGF("SEND: ~p ~n", [Response]),
    .gen_tcp:send(S, Response),
    {stop, normal, State};
'SESSION_STARTUP'(Other,State) ->
    %% Fall through for all other unsupported situations. 
    ?ERRF("Unknown internal message, closing connection out of paranoia.~nMsg: ~p~n",[Other]),
    {stop, normal, State}.

%% --------------------------
%% @doc Session transmission state.
%%
%% This receives decoded RELP commands and data from con_fsm processes from an
%% established session.
%%
%% close - close session
%% syslog - syslog message
%%
%% @end
%% --------------------------
'SESSION_TRANSMISSION'({syslog, #relp{data=Data, txnr=Txnr}}, #state{socket=S,syslog=Syslog,session_id=Id} = State)->
    %% This command indicates a syslog message is provided in the payload
    ?DEBUGF("RECV: ~p~n",[Data]),

    % Get socket details about peer
    {ok,{Peerip,Peerport}} = .inet:peername(S),

    % Get socket details locally
    {ok,{Localip,Localport}} = .inet:sockname(S),
    
    % Generate tuple list to pass to syslog_3164 decoder
    % TODO: generate this in con_fsm so we get more information at that level
    Event = [
        {'relp.txnr', Txnr},
        {'relp.command', "syslog"}, % TODO: remove this hard-coding
        {'relp.datalen',size(Data)}, % TODO: catch in con_fsm
        {'relp.data',Data},
        {'relp.session_id',Id},
        {'relp.tx_ip',Peerip},
        {'relp.tx_port',Peerport},
        {'relp.rx_ip',Localip},
        {'relp.rx_port',Localport}
    ],

    % Send to event processor
    .umberum.event:process(Event),

    % Send event to syslog_3164 decoder
    .gen_fsm:send_event(Syslog, {msg, Event}),

    % Return OK response on socket
    Response = .lists:flatten(.io_lib:format(?RELP_OK, [Txnr])),
    ?DEBUGF("SEND: ~p~n",[Response]),
    .gen_tcp:send(S, Response),

    {next_state, 'SESSION_TRANSMISSION', State};

'SESSION_TRANSMISSION'({close, #relp{txnr=Txnr}}, #state{socket=S} = State) ->
    %% Indicates an established client session wishes to close

    % Return OK
    Response = .lists:flatten(.io_lib:format(?RELP_OK, [Txnr])),
    ?DEBUGF("SEND: ~p~n",[Response]),
    .gen_tcp:send(S, Response),

    % Then stop - signalling its parent.
    {stop, normal, State};    

'SESSION_TRANSMISSION'(Other, State) ->
    %% Unknown command/message
    ?ERRF("Unknown message and/or RELP command during session. Will ignore.~n~p", [Other]),
    {next_state, 'SESSION_TRANSMISSION', State}.


%%-------------------------------------------------------------------------
%% @doc 
%%
%% @end
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->    
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% @doc
%%
%% @end
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% @doc Deal with 'EXIT' signals here from the connection process.
%%
%% @end
%%-------------------------------------------------------------------------
handle_info({'EXIT',From,_}, StateName, #state{socket=Socket,syslog=Syslog} = StateData) -> 
    % This is how we receive signals from the connection process when it stops
    % In this case, we just stop as well.
    case From of
	    Syslog ->
            % If it comes from the syslog process, restart the syslog process
	        {ok, SyslogPid} = .umberum.logger.syslog_3164.decode_sup:start_client(),
	        link(SyslogPid),
	        {next_state, StateName, #state{socket=Socket,syslog=SyslogPid}};
	    _Other -> 
            % This implies the stop is coming from the con_fsm, so lets just
            % stop.
	        {stop, normal, StateData}
    end;
handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.

%%-------------------------------------------------------------------------
%% @doc
%%
%% @end
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, _StateData) ->
    ok.

%%-------------------------------------------------------------------------
%% @doc
%%
%% @end
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
