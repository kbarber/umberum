%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc Supervisor module for the Organic Event engine
%% 
%% @end
%% --------------------------

-module(organic.event.event_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%% @doc Start and link to this module
start_link() ->
    .supervisor:start_link(.organic.event.event_sup, []).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

init(_Args) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % Processing supervisor
              {   .organic.event.proc.proc_sup,                  % Id       = internal id
                  {.organic.event.proc.proc_sup,start_link,[]},  % StartFun = {M, F, A}
                  permanent,                                            % Restart  = permanent | transient | temporary
                  infinity,                                             % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                                           % Type     = worker | supervisor
                  [.organic.event.proc.proc_sup]                 % Modules  = [Module] | dynamic
              }
            ]
         }
    }.
