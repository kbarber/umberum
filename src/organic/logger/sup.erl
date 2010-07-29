-module(organic.logger.sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
start_link() ->
    .supervisor:start_link(.organic.logger.sup, []).

init(_Args) ->
    {ok,
        {_SupFlags = {one_for_one, 10, 1},
            [
              % TCP Listener
              {   .organic.logger.relp.sup,           % Id       = internal id
                  {.organic.logger.relp.sup,start_link,[]}, % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                                  % Type     = worker | supervisor
                  [.organic.logger.relp.sup]          % Modules  = [Module] | dynamic
              }
            ]
         }
    }.
