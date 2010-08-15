%% --------------------------
%% @copyright 2010 Kenneth Barber
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
-define(ERR(MSG), .organic.log:err(MSG)).
-define(ERRF(MSG,FMT), .organic.log:err(MSG,FMT)).

-define(WARN(MSG), .organic.log:warn(MSG)).
-define(WARNF(MSG,FMT), .organic.log:warn(MSG,FMT)).

-define(INFO(MSG), .organic.log:info(MSG)).
-define(INFOF(MSG,FMT), .organic.log:info(MSG,FMT)).

-define(DEBUG(MSG), .organic.log:debug(MSG)).
-define(DEBUGF(MSG,FMT), .organic.log:debug(MSG,FMT)).
