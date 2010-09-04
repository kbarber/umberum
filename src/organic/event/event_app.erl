%% --------------------------
%% @copyright 2010 Kenneth Barber
%% @doc Application module for Organic Event Engine
%% 
%% @end
%% --------------------------

-module(organic.event.event_app).
-behaviour(application).
-export([start/2, stop/1]).

-include_lib("include/common.hrl").

%% @doc Callback for starting the application
start(_Type, _Args) ->
    ?INFO("organic.event application has started"),
    .organic.log:setup(),
    .organic.event.event_sup:start_link().

%% @doc Callback for when the application stops
stop(_State) ->
    ok.
