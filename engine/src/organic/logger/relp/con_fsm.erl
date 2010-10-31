%% --------------------------
%% @copyright 2010 Kenneth Barber
%% @doc This is a state based connection handler for receiving and parsing
%% RELP based protocol logging traffic. See here for reference:
%%
%% http://www.rsyslog.com/doc/relp.html
%% http://www.librelp.com/relp.html
%% 
%% @end
%% --------------------------
% 

-module(.organic.logger.relp.con_fsm).

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
                session    % session pid
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
    {ok, SessionPid} = .organic.logger.relp.session_sup:start_client(Socket),
    link(SessionPid),
    {next_state, 'WAIT_FOR_DATA', State#state{socket=Socket, addr=IP, session=SessionPid}};
'WAIT_FOR_SOCKET'(Other, State) ->
    ?ERRF("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, #state{session=Session} = State) ->
    ?DEBUGF("RCV: ~p~n", [binary_to_list(Data)]),
    process_packet(Data, Session),
    {next_state, 'WAIT_FOR_DATA', State};

'WAIT_FOR_DATA'(Data, State) ->
    ?WARNF("~p Ignoring data: ~p\n", [self(), Data]),
    {next_state, 'WAIT_FOR_DATA', State}.

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

process_packet(RawData, Session) ->
    ?DEBUGF("PROCESSING(~p): [~p]~n", [size(RawData),RawData]),

    HeaderRe = "^(\\d{1,9}) (open|close|syslog|rsp|abort) (\\d{1,9}?)[\\s|\\n](.*?)",
    ReOpts = [unicode,{capture,all,binary},dotall,ungreedy],
    case .re:run(RawData,HeaderRe,ReOpts) of
        {match, [_, Txnr, Command, DataLenBin, Data]} -> 
	        DataLen = .organic.util:bin_to_int(DataLenBin),
	        % Lets populate a relp_packet record with the unpacked data
	        PR = #relp{
	            txnr = .organic.util:bin_to_int(Txnr),
	            command = binary_to_atom(Command, latin1),
                datalen = .organic.util:bin_to_int(DataLenBin)},

            ?DEBUGF("Txnr: ~p Command: ~p Datalen: ~p Data: ~p size(Data): ~p~n",
                [PR#relp.txnr,PR#relp.command,PR#relp.datalen,Data,size(Data)]),                

	        % Check to see if the length matches the data size, indicating a single
	        % RELP packet in this transmission.
	        case DataLen+1 == size(Data) of
                true -> 
                    case .binary:last(Data) of
                        10 ->
                            .gen_fsm:send_event(Session, 
                                {PR#relp.command, 
                                 PR#relp{data=binary_part(Data,0,DataLen)}
                                });
                        Other ->
                            ?ERRF("No trailer found. Instead we found: ~p~n", [Other])
                    end;
		        false -> 
            	    case DataLen+1 > size(Data) of
                		true ->
                            case DataLen of
                                0 ->
                                    .gen_fsm:send_event(Session,
                                        {PR#relp.command,
                                        PR});
                                _Other ->
                                    % TODO: We're missing data, switch states so we can receive the rest
                                    ?ERR("TODO: size is greater then data section. We cannot handle this case yet.~n")
                            end;
                        false ->
                            % This packet contains multiple parts. Process the first, and then feed the
                            % remainder back to this function for more processing.
                            CurData = binary_part(Data, 0, DataLen+1),
                            case .binary:last(CurData) of
                                10 -> 
                                    .gen_fsm:send_event(Session,
                                        {PR#relp.command,
                                        PR#relp{data=binary_part(CurData, 0, DataLen)}
                                        }),
                                    Remainder = binary_part(Data, DataLen+2, size(Data)-(DataLen+2)),
                                    process_packet(Remainder, Session);
                                Other ->
                                    ?ERRF("No trailer found. Instead we found: ~p~n", [Other])
                            end
                    end,
                    ok
            end;
        _Other ->
            ?ERRF("Received packet does not look it is a RELP packet: [~p]. Ignoring.", [RawData])
    end.

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
