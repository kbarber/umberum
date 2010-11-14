%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc Tests for the module .umberum.input.relp.relp_protocol
%%
%% @end
%% @hidden
%% --------------------------

-module(.umberum.input.relp.relp_protocol_tests).
-include_lib("eunit/include/eunit.hrl").

%
% Lets begin with a series of 1 packet 1 relp frame tests only
%

%% @doc
%% @end
basic_1_test() ->
    Test = "basic_1",
    Input = <<"5253 syslog 222 <6>2010-11-14T20:53:46.473766+00:00 obelisk kernel: [965250.511753] iptables(INPUT-DROP): IN=br0 OUT= MAC= SRC=192.168.1.69 DST=192.168.1.255 LEN=156 TOS=0x00 PREC=0x00 TTL=64 ID=0 DF PROTO=UDP SPT=17500 DPT=17500 LEN=136 \n">>,
    Expected = [{relp, 5253, syslog, 222, <<"<6>2010-11-14T20:53:46.473766+00:00 obelisk kernel: [965250.511753] iptables(INPUT-DROP): IN=br0 OUT= MAC= SRC=192.168.1.69 DST=192.168.1.255 LEN=156 TOS=0x00 PREC=0x00 TTL=64 ID=0 DF PROTO=UDP SPT=17500 DPT=17500 LEN=136 ">>}],
    Returned = relp_protocol:decode(Input),
    
    ?debugFmt("~nTest: ~p~nInput: ~p~nExpected: ~p~nReturned: ~p~n",
        [ Test, Input, Expected, Returned ]
    ),

    ?assertEqual(Expected, Returned).

basic_2_test() ->
    Test = "basic_2",
    Input = <<"5580 syslog 91 <83>2010-11-14T21:40:01.993837+00:00 obelisk sudo: pam_unix(sudo:auth): conversation failed\n">>,
    Expected = [{relp, 5580, syslog, 91, <<"<83>2010-11-14T21:40:01.993837+00:00 obelisk sudo: pam_unix(sudo:auth): conversation failed">>}],
    Returned = relp_protocol:decode(Input),

    ?debugFmt("~nTest: ~p~nInput: ~p~nExpected: ~p~nReturned: ~p~n",
        [ Test, Input, Expected, Returned ]
    ),

    ?assertEqual(Expected, Returned).
