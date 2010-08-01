%% --------------------------
%% @copyright 2010 Kenneth Barber
%% @doc This processes RFC3164 BSD syslog messages
%%
%% @end
%% TODO: this belongs in its own directory not under relp. Something like bsd_syslog.
%% --------------------------
% 

-module(.organic.logger.relp.syslog_fsm).

-behaviour(gen_fsm).

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
                addr       % client address
               }).

-record(syslog_packet, {
	  facility,
	  severity,
	  timestamp,
	  hostname,
	  tag,
	  content
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
    {ok, 'RECEIVE', #state{socket=Socket}}.

%% --------------------------
%% @doc 
%%
%% @end
%% --------------------------
'RECEIVE'({msg, Data}, #state{socket=S} = State)->
    HeaderRe = "^<(\\d{1,3})>(.{32})\\s(.+)\\s(.+):\\s(.*?)",
    ReOpts = [unicode,{capture,all,binary},dotall, ungreedy],
    case .re:run(Data,HeaderRe,ReOpts) of
	{match, [_All,Priority,Timestamp,Hostname,Tag,Content]} -> 
	    SR = #syslog_packet{
	      facility = binary_to_integer(Priority) div 8,
	      severity = binary_to_integer(Priority) rem 8,
	      timestamp = binary_to_list(Timestamp),
	      hostname = binary_to_list(Hostname),
	      tag = binary_to_list(Tag),
	      content = Content},

	    .io:format("<data>~n"++
		       .io_lib:format("  facility = ~p~n", [SR#syslog_packet.facility])++
		       .io_lib:format("  severity = ~p~n", [SR#syslog_packet.severity])++
		       .io_lib:format("  timestamp = ~p~n", [SR#syslog_packet.timestamp])++
		       .io_lib:format("  hostname = ~p~n", [SR#syslog_packet.hostname])++
		       .io_lib:format("  tag = ~p~n", [SR#syslog_packet.tag])++
		       .io_lib:format("  content = ~p~n", [SR#syslog_packet.content])++
		       "</data>~n"),

	    ok;
	{match, Capture} -> ok;
	    %.io:format("Fell through match~p~n", [Capture]); %TODO:proper loggin
	nomatch -> ok;
        Other -> ok
    end,
    %.io:format("DATA: ~p~n", [Data]),
    {next_state, 'RECEIVE', State};
'RECEIVE'(Msg,State) ->
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
handle_info({EXIT,_,_}, StateName, StateData) -> 
    % This is how we receive signals from the connection process when it stops
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
terminate(_Reason, _StateName, #state{socket=Socket}) ->
    ok.

%%-------------------------------------------------------------------------
%% Func: binary_to_integer/1
%% Purpose: Translate binary to integer
%% Returns: integer()
%% @private
%%-------------------------------------------------------------------------
binary_to_integer(Binary) ->
    %TODO: find a built-in way of doing this
    %TODO: consider putting this in a shared place
    list_to_integer(binary_to_list(Binary)).

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
