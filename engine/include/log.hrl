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

-define(ERR(MSG), .organic.log:err(?LOG_FMT ++ MSG,[self(),?FILE,?LINE])).
-define(ERRF(MSG,FMT), .organic.log:err(?LOG_FMT ++ MSG,[self(),?FILE, ?LINE] ++ FMT)).

-define(WARN(MSG), .organic.log:warn(?LOG_FMT ++ MSG,[self(),?FILE,?LINE])).
-define(WARNF(MSG,FMT), .organic.log:warn(?LOG_FMT ++ MSG,[self(),?FILE, ?LINE] ++ FMT)).

-define(INFO(MSG), .organic.log:info(?LOG_FMT ++ MSG,[self(),?FILE,?LINE])).
-define(INFOF(MSG,FMT), .organic.log:info(?LOG_FMT ++ MSG,[self(),?FILE, ?LINE] ++ FMT)).

-define(DEBUG(MSG), .organic.log:debug(?LOG_FMT ++ MSG,[self(),?FILE,?LINE])).
-define(DEBUGF(MSG,FMT), .organic.log:debug(?LOG_FMT ++ MSG,[self(),?FILE, ?LINE] ++ FMT)).
