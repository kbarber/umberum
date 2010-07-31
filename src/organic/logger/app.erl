%% --------------------------
%% @copyright 2010 Kenneth Barber
%% @doc Application module for Organic Logger
%% 
%% @end
%% --------------------------

-module(organic.logger.app).
-behaviour(application).
-export([start/2, stop/1]).

%% @doc Callback for starting the application
start(_Type, _Args) ->
    .organic.logger.sup:start_link().

%% @doc Callback for when the application stops
stop(_State) ->
    ok.
