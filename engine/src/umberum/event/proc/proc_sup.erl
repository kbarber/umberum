%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc Event processing supervisor
%% 
%% @end
%% --------------------------

-module(.umberum.event.proc.proc_sup).

-behaviour(supervisor).

%% Supervisor callbacks
-export([start_link/0, stop/1, init/1]).

%% Others
-export([start_child/0]).

-include_lib("include/common.hrl").

-define(MAX_RESTART,  5).
-define(MAX_TIME,    60).

%% --------------------------
%% @doc 
%%
%% @end
%% --------------------------
start_child() ->
  .supervisor:start_child(umberum.event.proc.proc_sup, []).

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------

%% --------------------------
%% @doc 
%%
%% @end
%% --------------------------
start_link() ->
  {ok, Pid} = .supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  ?INFO("Started processing supervisor."),
  {ok, Pid}.

%% --------------------------
%% @doc 
%%
%% @end
%% --------------------------
stop(_S) ->
  ok.

%% --------------------------
%% @doc 
%%
%% @end
%% --------------------------
init([]) ->
  {ok,
    {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
        {   undefined,                 % Id     = internal id
          {.umberum.event.proc.proc_fsm,start_link,[]},          % StartFun = {M, F, A}
          temporary,                 % Restart  = permanent | transient | temporary
          2000,                  % Shutdown = brutal_kill | int() >= 0 | infinity
          worker,                  % Type   = worker | supervisor
          []                     % Modules  = [Module] | dynamic
        }
	    ]
	  }
  }.
