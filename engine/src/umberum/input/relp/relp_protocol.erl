%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc This is a standalone RELP protocol decoder module that is intended to
%% take care of the protocol side of translation.
%%
%% The protocol specifications can be accessed here:
%%
%% [http://www.rsyslog.com/doc/relp.html]
%%
%% [http://www.librelp.com/relp.html]
%% 
%% @end
%% --------------------------

-module(.umberum.input.relp.relp_protocol).

-include_lib("include/common.hrl").

-export([decode/1]).

%%-------------------------------------------------------------------------
%% @spec (Bin) -> {ok,Frame,Rest} | {more, DataLen} | {error, Error, ErrData}
%%    Frame = {relp,Txnr,Command,DataLen,Data}
%%    Txnr = integer()
%%    Command = relp_command()
%%    DataLen = integer()
%%    Data = binary()
%%    Rest = binary()
%%    Error = string()
%%    ErrData = binary()
%% @type relp_command() = open|close|syslog|rsp|abort
%% @doc Decodes the first RELP frame decoded in Bin. This is very similar to 
%% the BIF erlang:decode_packet.
%%
%% If an entire packet is contained in Bin it is returned together with the
%% remainder of the binary as `{ok, Packet, Rest}'.
%%
%% If Bin does not contain the entire packet, `{more, Length}' is returned. 
%% Length is the value of the data length section of the available RELP frame.
%%
%% If the packet does not conform to the protocol format `{error,Reason}' is 
%% returned.
%%
%% @end
%%-------------------------------------------------------------------------

decode(Packet) ->
    Re = "^(\\d{1,9}) (open|close|syslog|rsp|abort) (\\d{1,9}?)[\\s|\\n](.*?)",
    ReOpts = [unicode,{capture,all,binary},dotall,ungreedy],
    case .re:run(Packet,Re,ReOpts) of
        {match, [_, RawTxnr, RawCommand, RawDataLen, Data]} -> 
            Txnr = .umberum.util:bin_to_int(RawTxnr),
            Command = binary_to_atom(RawCommand, latin1),
	        DataLen = .umberum.util:bin_to_int(RawDataLen),

	        % Check to see if the length matches the data size, indicating a 
            % single RELP packet in this transmission.
	        case DataLen+1 == size(Data) of
                true -> 
                    % Single RELP packet
                    % Now check the trailer
                    case .binary:last(Data) of
                        10 ->
                            {ok,{relp,Txnr,Command,DataLen,
                                binary_part(Data,0,DataLen)},<<>>};
                        Other ->
                            {error, "Mismatched trailer", Other}
                    end;
		        false -> 
            	    case DataLen+1 > size(Data) of
                		true ->
                            case DataLen of
                                0 ->
                                    {ok,{relp,Txnr,Command,DataLen,Data},<<>>};
                                _Other ->
                                    {more, DataLen}
                            end;
                        false ->
                            % This packet contains multiple parts. Process the 
                            % first one and return the remainder.
                            CurData = binary_part(Data, 0, DataLen+1),
                            case .binary:last(CurData) of
                                10 -> 
                                    {ok,{
                                        relp,Txnr,Command,DataLen,
                                        binary_part(CurData, 0, DataLen)},
                                        binary_part(Data, DataLen+1, 
                                            size(Data)-(DataLen+1))
                                    };
                                Other ->
                                    {error, "Mismatched trailer", Other}
                            end
                    end
            end;
        {match, Other} ->
            {error, "Regex matched but parameters did not", Other};
        _Other ->
            {error, "Invalid RELP packet"}
    end.
