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

-include_lib("include/data.hrl").

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
    %TODO: proper logging
    .error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, #state{session=Session} = State) ->
    .io:format("RCV: ~p~n", [binary_to_list(Data)]),
    process_packet(Data, Session),
    {next_state, 'WAIT_FOR_DATA', State};

'WAIT_FOR_DATA'(Data, State) ->
    .io:format("~p Ignoring data: ~p\n", [self(), Data]),
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
    .error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info({'EXIT',_,_}, _StateName, StateData) ->
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.

process_packet(RawData, Session) ->
    HeaderRe = "^(\\d{1,9}) (open|close|syslog|rsp|abort) (\\d{1,9})\\s?(.*)?",
    ReOpts = [unicode,{capture,all,binary},dotall],
    case .re:run(RawData,HeaderRe,ReOpts) of
	{match, [_AllData, Txnr, Command, DataLenBin, Data]} -> 
	    DataLen = .organic.util:bin_to_int(DataLenBin),
	    % Lets populate a relp_packet record with the unpacked data
	    PR = #relp{
	      txnr = .organic.util:bin_to_int(Txnr),
	      command = binary_to_atom(Command, latin1)},

	    % Check to see if the length matches the data size, indicating a single
	    % RELP packet in this transmission.
	    case DataLen+1 == size(Data) of
		true -> 
		    case .lists:last(binary_to_list(Data)) of
			10 ->
			    .gen_fsm:send_event(Session, 
						{PR#relp.command, 
						 PR#relp{data=.organic.util:bin_part(Data,DataLen)}
						});
			Other ->
			    .io:format("ERROR: No trailer found. Instead we found: ~p~n", [Other])
		    end;
		false -> 
		    case DataLen+1 > size(Data) of
			true ->
			    % TODO: We're missing data, switch states so we can receive the rest
			    .io:format("TODO: size is greater then data section. We cannot handle this case yet."),
			    ok;
			false ->
			    % This packet contains multiple parts. Process the first, and then feed the
			    % remainder back to this function for more processing.
			    CurData = .organic.util:bin_part(Data, DataLen+1),
			    case .lists:last(binary_to_list(CurData)) of
				10 -> 
				    .gen_fsm:send_event(Session,
							{PR#relp.command,
							 PR#relp{data=.organic.util:bin_part(CurData, DataLen)}
							}),
				    Remainder = .organic.util:bin_part(Data, DataLen+2, 999999999),
				    process_packet(Remainder, Session);
				Other ->
				    .io:format("ERROR: No trailer found. Instead we found: ~p~n", [Other])
			    end
		    end,
		    ok
	    end;
        _Other -> ok
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
