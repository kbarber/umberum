%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc This processes RFC3164 BSD syslog messages
%%
%% @end
%% --------------------------
% 

-module(.umberum.logger.syslog_3164.decode_fsm).

-behaviour(gen_fsm).

-include_lib("include/common.hrl").

-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
     handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
  'RECEIVE'/2
]).

-record(state, {
        router,    % route pid
        decoder
        }).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @spec () -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%    If init/1 fails with Reason, the function returns {error,Reason}.
%%    If init/1 returns {stop,Reason} or ignore, the process is
%%    terminated and the function returns {error,Reason} or ignore,
%%    respectively.
%% @end
%%-------------------------------------------------------------------------
start_link() ->
  .gen_fsm:start_link(?MODULE, [], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%% --------------------------
%% @doc 
%%
%% @end
%% --------------------------
init([]) ->
  .process_flag(trap_exit, true),
  {ok, RoutePid} = .umberum.logger.route.route_sup:start_client(self()),
  link(RoutePid),
  HeaderRe = "^<(\\d{1,3})>(.{32})\\s(.+)\\s(.+)(\\[(\\d+)\\]){0,1}:\\s(.*?)",
  ReOpts = [unicode,dotall,ungreedy],
  case .re:compile(HeaderRe,ReOpts) of
    {ok, Decoder} ->
      {ok, 'RECEIVE', #state{router=RoutePid,decoder=Decoder}};
    Other ->
      Other
  end.  

%% --------------------------
%% @doc Here we receive messages with RELP payloads, so we can extract out
%% embedded syslog_3164 data and forward it to other processes.
%%
%% @end
%% --------------------------
'RECEIVE'({msg, Event}, #state{router=Router,decoder=Decoder} = State)->
  % Extract needed details from Event data
  {value,{_,Data}} = .lists:keysearch('relp.data',1,Event),

  % The decoder runs a regular expression across the data extracting out
  % the relevant parts.
  case .re:run(Data,Decoder,[{capture,all,binary}]) of
	  {match, [_All,Priority,Timestamp,Hostname,Tag,_,Procid,Content]} -> 
      Facility = case .umberum.util:bin_to_int(Priority) div 8 of
      0 -> <<"kernel">>;
      1 -> <<"user">>;
      2 -> <<"mail">>;
      3 -> <<"daemon">>;
      4 -> <<"auth">>;
      5 -> <<"syslog">>;
      6 -> <<"lpr">>;
      7 -> <<"news">>;
      8 -> <<"uucp">>;
      9 -> <<"cron">>;
      10 -> <<"authpriv">>;
      11 -> <<"ftp">>;
      16 -> <<"local0">>;
      17 -> <<"local1">>;
      18 -> <<"local2">>;
      19 -> <<"local3">>;
      20 -> <<"local4">>;
      21 -> <<"local5">>;
      22 -> <<"local6">>;
      24 -> <<"local7">>
      end,
      Severity = case .umberum.util:bin_to_int(Priority) rem 8 of
      0 -> <<"kernel">>;
      1 -> <<"user">>;
      2 -> <<"mail">>;
      3 -> <<"daemon">>;
      4 -> <<"auth">>;
      5 -> <<"syslog">>;
      6 -> <<"lpr">>;
      7 -> <<"news">>
      end,
	    SR = #syslog{
	      facility = Facility,
	      severity = Severity,
	      timestamp = Timestamp,
	      hostname = Hostname,
	      tag = Tag,
        procid = Procid,
	      content = Content},

      .gen_fsm:send_event(Router, {log, SR}),
      ok;
    {match, _Capture} -> 
      ?ERR("Unable to decode packet. Ignoring.");
    nomatch -> ok;
    _Other -> ok
  end,
  ?DEBUGF("DATA: ~p~n", [Data]),
  {next_state, 'RECEIVE', State};
'RECEIVE'(_Msg,State) ->
  {next_state, 'RECEIVE', State}.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}      |
%%      {next_state, NextStateName, NextStateData, Timeout} |
%%      {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
  {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}      |
%%      {next_state, NextStateName, NextStateData, Timeout}   |
%%      {reply, Reply, NextStateName, NextStateData}      |
%%      {reply, Reply, NextStateName, NextStateData, Timeout} |
%%      {stop, Reason, NewStateData}              |
%%      {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
  {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}      |
%%      {next_state, NextStateName, NextStateData, Timeout} |
%%      {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({'EXIT',From,_}, StateName, #state{router=Router} = StateData) -> 
  % This is how we receive signals from the route process when it stops
  % In this case, we just stop as well.
  case From of
	Router ->
	  {ok, RouterPid} = .umberum.logger.route.route_sup:start_client(self()),
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
