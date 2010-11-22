%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc This is a standalone RELP protocol decoder module that is intended to
%% take care of the protocol side of translation.
%%
%% http://www.rsyslog.com/doc/relp.html
%% http://www.librelp.com/relp.html
%% 
%% @end
%% --------------------------

-module(.umberum.input.relp.relp_protocol).

-include_lib("include/common.hrl").

-export([decode/2,decode/1]).

%%-------------------------------------------------------------------------
%% @spec (Data) -> [RelpEvent]
%% @doc When passed raw data it will attempt to decode any valid RELP frames
%% from it. It is meant to return a collection of decoded RELP events.
%%
%% It should return:
%%
%% [{relp, Txnr, Command, DataLen, Data}, ...]
%%
%%
%% @end
%%-------------------------------------------------------------------------

decode(Packet) ->
    HeaderRe = "^(\\d{1,9}) (open|close|syslog|rsp|abort) (\\d{1,9}?)[\\s|\\n](.*?)",
    ReOpts = [unicode,{capture,all,binary},dotall,ungreedy],
    case .re:run(Packet,HeaderRe,ReOpts) of
        {match, [_, RawTxnr, RawCommand, RawDataLen, Data]} -> 
            Txnr = .umberum.util:bin_to_int(RawTxnr),
            Command = binary_to_atom(RawCommand, latin1),
	        DataLen = .umberum.util:bin_to_int(RawDataLen),

	        % Check to see if the length matches the data size, indicating a single
	        % RELP packet in this transmission.
	        case DataLen+1 == size(Data) of
                true -> 
                    case .binary:last(Data) of
                        10 ->
                            [{relp,Txnr,Command,DataLen,binary_part(Data,0,DataLen)}];
                        Other ->
                            {error, "Mismatched trailer", Other}
                    end;
		        false -> 
            	    case DataLen+1 > size(Data) of
                		true ->
                            case DataLen of
                                0 ->
                                    [{relp,Txnr,Command,DataLen,Data}];
                                _Other ->
                                    [{remainder, Packet}]
                            end;
                        false ->
                            % This packet contains multiple parts. Process the first, and then feed the
                            % remainder back to this function for more processing.
                            CurData = binary_part(Data, 0, DataLen+1),
                            case .binary:last(CurData) of
                                10 -> 
                                    % TODO: deal with instances where there is multiple data per packet
                                    Previous = [{relp,Txnr,Command,DataLen,binary_part(CurData, 0, DataLen)}],
                                    Remainder = binary_part(Data, DataLen+1, size(Data)-(DataLen+1)),
                                    decode(Previous, Remainder);
                                Other ->
                                    {error, "Mismatched trailer.", Other}
                            end
                    end
            end;
        {match, Other} ->
            {error, "Regex matched but parameters did not", Other};
        _Other ->
            {error, "Invalid RELP packet"}
    end.

decode(_Previous, _Remainder) ->
    .lists:append(_Previous, decode(_Remainder)).

