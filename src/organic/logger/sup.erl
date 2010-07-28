-module(organic.logger.sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
start_link() ->
    supervisor:start_link(.organic.logger.sup, []).

init(_Args) ->
    {ok, {{one_for_one, 1, 60}, []}}.
