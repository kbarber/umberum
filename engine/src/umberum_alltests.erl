%% --------------------------
%% @copyright 2010 Bob.sh
%% @doc Primary entry point for tests.
%%
%% @end
%% --------------------------

-module(umberum_alltests).
-export([start/0]).

start() ->
    error_logger:tty(false),
    eunit:test(
        .umberum.input.relp.relp_protocol,
        [
            verbose,
            {report,{eunit_surefire,[{dir,"test_reports"}]}}
        ]
    ),
    halt().

