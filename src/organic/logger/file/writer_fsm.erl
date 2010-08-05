%% --------------------------
%% @copyright 2010 Kenneth Barber
%% @doc This process is for outputting data to a file.
%%
%% @end
%% --------------------------
% 

-module(.organic.logger.file.writer_fsm).

-include_lib("include/data.hrl").

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
                source_proc
               }).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @spec (Socket) -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%% @end
%%-------------------------------------------------------------------------
start_link(SourceProc) ->
    .gen_fsm:start_link(?MODULE, [SourceProc], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%% --------------------------
%% @doc 
%%
%% @end
%% --------------------------
init([SourceProc]) ->
    .process_flag(trap_exit, true),
    {ok, 'RECEIVE', #state{source_proc=SourceProc}}.

%% --------------------------
%% @doc 
%%
%% @end
%% --------------------------
'RECEIVE'({write, SR}, State)->
    % TODO: logging to one file isn't that useful
    {ok,LogFile} = .file:open("/tmp/log", [ write, append]),
    .file:write(LogFile, list_to_binary(.io_lib:format("<data>~n"++
	       .io_lib:format("  facility = ~p~n", [SR#syslog.facility])++
	       .io_lib:format("  severity = ~p~n", [SR#syslog.severity])++
	       .io_lib:format("  timestamp = ~p~n", [SR#syslog.timestamp])++
	       .io_lib:format("  hostname = ~p~n", [SR#syslog.hostname])++
	       .io_lib:format("  tag = ~p~n", [SR#syslog.tag])++
	       .io_lib:format("  content = ~p~n", [SR#syslog.content])++
	       "</data>~n",[]))),
    .file:close(LogFile),
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
handle_info({'EXIT',_,_}, _StateName, StateData) -> 
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
