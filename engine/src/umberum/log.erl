%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc This module provides functions for logging internal umberum error 
%% messages.
%% 
%% @end
%% --------------------------

-module(umberum.log).
-export([setup/0]).
-export([err/1, err/2, warn/1, warn/2, info/1, info/2, debug/1, debug/2]).

-include_lib("include/common.hrl").

%%-------------------------------------------------------------------------
%% @doc Setup logging.
%% @end
%%-------------------------------------------------------------------------
setup() ->
    .error_logger:logfile({open,?CONF(log_file)}).

%%-------------------------------------------------------------------------
%% @doc Log an error message with formatting.
%% @end
%%-------------------------------------------------------------------------
err(Msg,Fmt) ->
    .error_logger:error_msg(Msg,Fmt).

%%-------------------------------------------------------------------------
%% @doc Log an error message.
%% @end
%%-------------------------------------------------------------------------
err(Msg) ->
    .error_logger:error_msg(Msg).


%%-------------------------------------------------------------------------
%% @doc Log a warning message with formatting.
%% @end
%%-------------------------------------------------------------------------
warn(Msg,Fmt) ->
    .error_logger:warning_msg(Msg,Fmt).

%%-------------------------------------------------------------------------
%% @doc Log a warning message.
%% @end
%%-------------------------------------------------------------------------
warn(Msg) ->
    .error_logger:warning_msg(Msg).


%%-------------------------------------------------------------------------
%% @doc Log an info message with formatting.
%% @end
%%-------------------------------------------------------------------------
info(Msg,Fmt) ->
    .error_logger:info_msg(Msg,Fmt).

%%-------------------------------------------------------------------------
%% @doc Log an info message.
%% @end
%%-------------------------------------------------------------------------
info(Msg) ->
    .error_logger:info_msg("~p " ++ Msg, [self()]).

%%-------------------------------------------------------------------------
%% @doc Log a debug message with formatting.
%% @end
%%-------------------------------------------------------------------------
debug(Msg,Fmt) ->
    case ?CONF(log_debug) of
        true -> .error_logger:info_msg("DEBUG: " ++ Msg,Fmt);
        _Other -> ok
    end.

%%-------------------------------------------------------------------------
%% @doc Log a debug message.
%% @end
%%-------------------------------------------------------------------------
debug(Msg) ->
    case ?CONF(log_debug) of
        true -> .error_logger:info_msg("DEBUG: " ++ Msg);
        _Other -> ok
    end.
