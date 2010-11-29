%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc Supervisor module for the Umberum Event engine
%% 
%% @end
%% --------------------------

-module(umberum.event.event_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%% @doc Start and link to this module
start_link() ->
  .supervisor:start_link(.umberum.event.event_sup, []).

-define(MAX_RESTART,  5).
-define(MAX_TIME,    60).

init(_Args) ->
  {ok,
    {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
        % Processing supervisor
        {   .umberum.event.proc.proc_sup,          % Id     = internal id
          {.umberum.event.proc.proc_sup,start_link,[]},  % StartFun = {M, F, A}
          permanent,                      % Restart  = permanent | transient | temporary
          infinity,                       % Shutdown = brutal_kill | int() >= 0 | infinity
          supervisor,                       % Type   = worker | supervisor
          [.umberum.event.proc.proc_sup]         % Modules  = [Module] | dynamic
        }
      ]
     }
  }.
