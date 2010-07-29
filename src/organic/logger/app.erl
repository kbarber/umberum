-module(organic.logger.app).
-behaviour(application).
-export([start/2, stop/1]).
start(_Type, _Args) ->
    %Pid = .supervisor:start_link(.organic.logger.sup, []),
    .organic.logger.sup:start_link().
stop(_State) ->
    ok.
