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

-record(relp_packet, {
	  all_data,
	  txnr,
	  command,
	  datalen,
	  datalencur,
	  data,
	  trailer
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
'WAIT_FOR_DATA'({data, Data}, #state{socket=S,session=Session} = State) ->
    % .io:format("RCV: ~p~n", [binary_to_list(Data)]),
    %% TODO: regular expressions are probably slow here - need a non -regexp
    %%       way of doing this.
    %% TODO: This regexp does not handle partial packets due to the inability of me 
    %%       to be able to make the last line feed optional, and always be matched if available
    HeaderRe = "^(\\d{1,9}) (open|close|syslog|rsp|abort) (\\d{1,9})\\s?(.*)?(\\n)",
    ReOpts = [unicode,{capture,all,binary},dotall],
    case .re:run(Data,HeaderRe,ReOpts) of
	{match, [AllData, Txnr, Command, DataLen, DataCaptured, Trailer]} -> 
	    % Lets populate a relp_packet record with the unpacked packet data
	    PR = #relp_packet{
	      all_data = AllData,
	      txnr = binary_to_integer(Txnr),
	      command = binary_to_atom(Command, latin1),
	      datalen = binary_to_integer(DataLen),
	      data = DataCaptured,
	      datalencur = size(DataCaptured),
	      trailer = Trailer},

	    % TODO: Ensure datalen matches the size of data. A datalen size that is larger
	    % then the size of data indicates generally there is more data to come.
	    % In the opposite case, it indicates the packet is mangled somehow.
	    case PR#relp_packet.datalen == PR#relp_packet.datalencur of
		true -> 
		    .gen_fsm:send_event(Session, {PR#relp_packet.command, PR});
		false -> 
		    case PR#relp_packet.datalen > PR#relp_packet.datalencur of
			true ->
			    % TODO: We're missing data, switch states so we can receive the rest
			    ok;
			false ->
			    % TODO: We have too much data. There is probably many packets here.
			    .io:format("TODO: Multiple RELP packets received in one TCP packet. I cannot handle this yet: ~p~n",[Data]),
			    ok
		    end,
		    ok
	    end;	    
	{match, Capture} -> ok;
	    %.io:format("Fell through match~p~n", [Capture]); %TODO:proper loggin
	nomatch -> ok;
        Other -> ok
    end,
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

handle_info({EXIT,_,_}, _StateName, StateData) ->
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
