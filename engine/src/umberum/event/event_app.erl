%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc Application module for Umberum Event Engine
%% 
%% @end
%% --------------------------

-module(umberum.event.event_app).
-behaviour(application).
-export([start/2, stop/1]).

-include_lib("include/common.hrl").

%% @doc Callback for starting the application
start(_Type, _Args) ->
    ?INFO("umberum.event application has started"),
    .umberum.log:setup(),
    .umberum.event.event_sup:start_link().

%% @doc Callback for when the application stops
stop(_State) ->
    ok.
