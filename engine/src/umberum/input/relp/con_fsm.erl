%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc This is a state based connection handler for receiving and parsing
%% RELP based protocol logging traffic. See here for the protocol reference:
%%
%% [http://www.rsyslog.com/doc/relp.html]
%%
%% [http://www.librelp.com/relp.html]
%% 
%% @end
%% --------------------------
% 

-module(.umberum.input.relp.con_fsm).

-behaviour(gen_fsm).

-include_lib("include/common.hrl").

-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
    'WAIT_FOR_SOCKET'/2,
    'WAIT_FOR_DATA'/2
]).

-record(state, {
                socket,    % client socket
                addr,      % client address
                session,   % session pid
                last_data  % last_data
                }).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @spec () -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%      If init/1 fails with Reason, the function returns {error,Reason}.
%%      If init/1 returns {stop,Reason} or ignore, the process is
%%      terminated and the function returns {error,Reason} or ignore,
%%      respectively.
%% @end
%%-------------------------------------------------------------------------
start_link() ->
    .gen_fsm:start_link(?MODULE, [], []).

%% --------------------------
%% @doc 
%%
%% @end
%% --------------------------
set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    .gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([]) ->
    .process_flag(trap_exit, true),
    {ok, 'WAIT_FOR_SOCKET', #state{}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
    % Now we own the socket
    .inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
    {ok, {IP, _Port}} = .inet:peername(Socket),
    {ok, SessionPid} = .umberum.input.relp.session_sup:start_client(Socket),
    link(SessionPid),
    {next_state, 'WAIT_FOR_DATA', State#state{socket=Socket, addr=IP, session=SessionPid, last_data = <<>>}};
'WAIT_FOR_SOCKET'(Other, State) ->
    ?ERRF("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, #state{session=Session,last_data=LastData} = State) ->
    ?DEBUGF("RCV: ~p~n", [binary_to_list(Data)]),

    CombinedData = <<LastData/binary, Data/binary>>,

    case process_packet(CombinedData, Session) of
        ok ->
            {next_state, 'WAIT_FOR_DATA', State#state{last_data = <<>>}};
        {more, _Length} ->
            {next_state, 'WAIT_FOR_DATA', State#state{last_data=CombinedData}}
    end;

'WAIT_FOR_DATA'(Data, State) ->
    ?WARNF("~p Ignoring data: ~p\n", [self(), Data]),
    {next_state, 'WAIT_FOR_DATA', State}.
    

process_packet(Data, Session) ->
    process_packet_r(.umberum.input.relp.relp_protocol:decode(Data), Session).

process_packet_r({ok,RelpFrame,Rest}, Session) ->
    {relp,Txnr,Command,DataLen,Data} = RelpFrame,
    PR = #relp{
        txnr = Txnr,
        command = Command,
        datalen = DataLen,
        data = Data},
    .gen_fsm:send_event(Session, {Command, PR}),

    case size(Rest) > 0 of
        true -> process_packet(Rest,Session);
        false -> ok
    end;

process_packet_r({more, Length}, _Session) ->
    {more, Length};

process_packet_r({error, Msg, _Data}, _Session) ->
    ?ERRF("Error in RELP decoder: ~p\n", Msg),
    ok.

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
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
    % Flow control: enable forwarding of next TCP message
    .inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName,
            #state{socket=Socket, addr=Addr} = StateData) ->
    ?INFOF("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info({'EXIT',_,_}, _StateName, StateData) ->
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
    (catch .gen_tcp:close(Socket)),
    ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
