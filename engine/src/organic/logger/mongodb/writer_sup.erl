%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc File writing supervisor
%% 
%% @end
%% --------------------------

-module(.organic.logger.mongodb.writer_sup).

-behaviour(supervisor).

-export([start_client/1]).

%% Supervisor callbacks
-export([start_link/0, stop/1, init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

%% --------------------------
%% @doc A startup function for spawning new file writing FSM.
%% To be called by its session_fsm process.
%% @end
%% --------------------------
start_client(Router) ->
    .supervisor:start_child(organic.logger.mongodb.writer_sup, [Router]).

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------

%% --------------------------
%% @doc 
%%
%% @end
%% --------------------------
start_link() ->
    .supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
                  {.organic.logger.mongodb.writer_fsm,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.
