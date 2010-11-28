%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc This process is for outputting data to a file.
%%
%% @end
%% TODO: Name this something other then writer_fsm as it is confusing in the editor
%% due to conflicts with umberum.mongodb.writer_fsm
%% --------------------------
% 

-module(.umberum.logger.file.writer_fsm).

-include_lib("include/common.hrl").

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
                source_proc,
                log_file
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
    .pg2:join('umberum.logger.file.writer_fsm', self()),
    .process_flag(trap_exit, true),
    {ok,LogFile} = .file:open(?CONF(file_outputpath), [ write, append]),
    {ok, 'RECEIVE', #state{source_proc=SourceProc, log_file=LogFile}}.

%% --------------------------
%% @doc Here we receive incoming records and using bit syntax store them in the
%% log file.
%%
%% @end
%% --------------------------
'RECEIVE'({write, SR}, #state{log_file=LogFile} = State)->
  .file:write(LogFile, <<
    "<data>\n",
    "  facility = ", (SR#syslog.facility)/binary, "\n",
    "  severity = ", (SR#syslog.severity)/binary, "\n",
    "  timestamp = ", (SR#syslog.timestamp)/binary, "\n",
    "  hostname = ", (SR#syslog.hostname)/binary, "\n",
    "  tag = ", (SR#syslog.tag)/binary, "\n",
    "  procid = ", (SR#syslog.procid)/binary, "\n",
    "  content = ", (SR#syslog.content)/binary, "\n",
    "</data>\n"
  >>),
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
