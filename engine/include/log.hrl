%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc Header file for logging related macros
%%
%% @end
%% --------------------------

%% --------------------------
%% @doc A macro for logging errors
%%
%% Usage:
%%
%%   ?ERR("Error has occured").
%%
%% @end
%% --------------------------
-define(LOG_FMT, "Pid: ~p Code: ~p:~p~n~n").

-define(ERR(MSG), .umberum.log:err(?LOG_FMT ++ MSG,[self(),?FILE,?LINE])).
-define(ERRF(MSG,FMT), .umberum.log:err(?LOG_FMT ++ MSG,[self(),?FILE, ?LINE] ++ FMT)).

-define(WARN(MSG), .umberum.log:warn(?LOG_FMT ++ MSG,[self(),?FILE,?LINE])).
-define(WARNF(MSG,FMT), .umberum.log:warn(?LOG_FMT ++ MSG,[self(),?FILE, ?LINE] ++ FMT)).

-define(INFO(MSG), .umberum.log:info(?LOG_FMT ++ MSG,[self(),?FILE,?LINE])).
-define(INFOF(MSG,FMT), .umberum.log:info(?LOG_FMT ++ MSG,[self(),?FILE, ?LINE] ++ FMT)).

-define(DEBUG(MSG), .umberum.log:debug(?LOG_FMT ++ MSG,[self(),?FILE,?LINE])).
-define(DEBUGF(MSG,FMT), .umberum.log:debug(?LOG_FMT ++ MSG,[self(),?FILE, ?LINE] ++ FMT)).
