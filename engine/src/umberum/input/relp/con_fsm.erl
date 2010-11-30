%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc This is a state based connection handler for receiving and parsing
%% RELP based protocol logging traffic.
%%
%% This process manages the connection while session_fsm manages the session.
%% Both work together to provide RELP decoding facilities. The actual decoding
%% is provided by relp_protocol.
%% @end
%% --------------------------

-module(.umberum.input.relp.con_fsm).

-behaviour(gen_fsm).

-include_lib("include/common.hrl").

-export([start_link/0, set_socket/2]).
-export([init/1, handle_event/3,
     handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([
  'WAIT_FOR_SOCKET'/2,
  'WAIT_FOR_DATA'/2
]).

-record(state, {
        socket,     % client socket
        addr,       % client address
        session,    % session pid
        last_data,  % last_data
        decoder     % decoder
        }).

%%-------------------------------------------------------------------------
%% @spec () -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%% @end
%%-------------------------------------------------------------------------
start_link() ->
  .gen_fsm:start_link(?MODULE, [], []).

%% --------------------------
%% @doc 
%%
%% @end
%% @private
%% --------------------------
set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
  .gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%-------------------------------------------------------------------------
%% @doc Initialise.
%% @private
%%-------------------------------------------------------------------------
init([]) ->
  .process_flag(trap_exit, true),
  {ok, 'WAIT_FOR_SOCKET', #state{decoder = .umberum.input.relp.relp_protocol:init()}}.

%%-------------------------------------------------------------------------
%% @doc The WAIT_FOR_SOCKET state awaits a new established socket to be passed
%% to this process.
%% @end
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
  % Now we own the socket
  .inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
  {ok, {IP, _Port}} = .inet:peername(Socket),
  {ok, SessionPid} = .umberum.input.relp.session_sup:start_client(Socket),
  link(SessionPid),
  {next_state, 'WAIT_FOR_DATA', State#state{socket=Socket, addr=IP, 
    session=SessionPid, last_data = <<>>}};

'WAIT_FOR_SOCKET'(Other, State) ->
  ?ERRF("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
  %% Allow to receive async messages
  {next_state, 'WAIT_FOR_SOCKET', State}.


%%-------------------------------------------------------------------------
%% @doc The WAIT_FOR_DATA state awaits a new packet from the socket, decodes it
%% and sends each frame to the session handling part of the RELP decoder.
%%
%% Primarly we are expecting this kind of RELP data inside a packet:
%%
%% * n frames
%% * n frames, 1 leading partial frame
%% * 1 trailing partial frame, n frames
%% * 1 trailing partial frame, n frames, 1 leading partial frame
%% * 1 partial frame (trailing and leading)
%%
%% And we will deal with each scenario differently. For partials, we have to 
%% keep data around to attempt to prefix it onto the next packet.
%%
%% @end
%%-------------------------------------------------------------------------
'WAIT_FOR_DATA'({data, Data}, 
  #state{session=Session,last_data=LastData,decoder=Decoder} = State) ->
  
  ?DEBUGF("RCV: ~p~n", [binary_to_list(Data)]),

  % Here we hopefully combine the last lead partial frame with the next 
  % trialing one to make a complete frame.
  CombinedData = <<LastData/binary, Data/binary>>,

  case process_packet(CombinedData, Session, Decoder) of
    ok ->
      % If the packet is okay, get ready for another.
      {next_state, 'WAIT_FOR_DATA', State#state{last_data = <<>>}};
    {more, _Length, Remainder} ->
      % This indicates we have a leading partial frame that was cut off
      % in the data section. In this case we know there is definately 
      % more data so we stow away that data to be later prefixed to the
      % next set of incoming data.
      {next_state, 'WAIT_FOR_DATA', 
        State#state{last_data = Remainder}};
    {error, nomatch, Remainder} ->
      % So this is either a legit error or a leading partial frame that
      % was cut-off before the header could be established for a 'more'
      % return. For now we'll assume its a leading frame and add it to be
      % combined with the next packet for now.
      %
      % TODO: deal with states where the packet really is invalid. At the
      % moment this behaviour will break the next packet as well.
      {next_state, 'WAIT_FOR_DATA', 
        State#state{last_data = Remainder}};
    {error, Msg, Remainder} ->
      % These states are genuine mismatches so fail out and don't try to
      % combined anything.
      ?ERRF("Error in RELP decoder: ~p~nLast Data: ~p\n", 
        [Msg, Remainder]),
      {next_state, 'WAIT_FOR_DATA', State#state{last_data = <<>>}}
  end;

'WAIT_FOR_DATA'(Data, State) ->
  ?WARNF("~p Ignoring data: ~p\n", [self(), Data]),
  {next_state, 'WAIT_FOR_DATA', State}.
  
%%-------------------------------------------------------------------------
%% @doc Here we provide a recursive routine to iterate through a packet, using
%% a RELP decoder to pull each frame out and forward it to the session process.
%% @end
%% @private
%%-------------------------------------------------------------------------
process_packet(Packet, Session, Decoder) ->
  case .umberum.input.relp.relp_protocol:decode(Packet, Decoder) of
    {ok,{relp,Txnr,Command,DataLen,Data},Rest} ->
      PR = #relp{
        txnr = Txnr,
        command = Command,
        datalen = DataLen,
        data = Data},
      .gen_fsm:send_event(Session, {Command, PR}),

      case size(Rest) > 0 of
        true -> process_packet(Rest,Session, Decoder);
        false -> ok
      end;
    {error, Msg} ->
      {error, Msg, Packet};
    {more, Length} ->
      {more, Length, Packet}
  end.

%%-------------------------------------------------------------------------
%% @doc Gets triggered on events.
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
  {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% @doc Gets triggered on sync events.
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
  {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% @doc Gets triggered on info events.
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
  % Flow control: enable forwarding of next TCP message
  .inet:setopts(Socket, [{active, once}]),
  ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName,
      #state{socket=Socket, addr=Addr} = StateData) ->
  ?INFOF("~p Client ~p disconnected.\n", [self(), .inet_parse:ntoa(Addr)]),
  {stop, normal, StateData};

handle_info({'EXIT',_,_}, _StateName, StateData) ->
  {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
  {noreply, StateName, StateData}.  

%%-------------------------------------------------------------------------
%% @doc Shuts down the fsm.
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket=Socket}) ->
  (catch .gen_tcp:close(Socket)),
  ok.

%%-------------------------------------------------------------------------
%% @doc Convert the process state when the code is changed.
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.
