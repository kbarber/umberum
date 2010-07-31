%% --------------------------
%% @copyright 2010 Kenneth Barber
%% @doc RELP syslog process supervisor
%% 
%% @end
%% --------------------------

-module(.organic.logger.relp.syslog_sup).

-behaviour(supervisor).

%% Internal API
-export([start_client/1]).

%% Supervisor callbacks
-export([start_link/0, stop/1, init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

%% --------------------------
%% @doc A startup function for spawning new syslog handling FSM.
%% To be called by its session_fsm process.
%% @end
%% --------------------------
start_client(Socket) ->
    .supervisor:start_child(organic.logger.relp.syslog_sup, [Socket]).

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------

%% --------------------------
%% @doc 
%%
%% @end
%% --------------------------
start_link() ->
    .supervisor:start_link({local, ?MODULE}, ?MODULE, [.organic.logger.relp.syslog_fsm]).

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
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {.organic.logger.relp.syslog_fsm,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.
