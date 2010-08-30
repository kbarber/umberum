%% --------------------------
%% @copyright 2010 Kenneth Barber
%% @doc Primary supervisor for the RELP protocol
%% 
%% @end
%% --------------------------

-module(.organic.logger.relp.relp_sup).

-behaviour(supervisor).

%% Supervisor callbacks
-export([start_link/0, stop/1, init/1]).

-include_lib("include/common.hrl").

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).


%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------

%% --------------------------
%% @doc 
%%
%% @end
%% --------------------------
start_link() ->
    .supervisor:start_link(
        {local, ?MODULE}, 
        ?MODULE, 
        [?CONF(relp_port), .organic.logger.relp.con_fsm]
        ).

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
init([Port, Module]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Listener
              {   organic.logger.relp.listener,           % Id       = internal id
                  {.organic.logger.relp.listener,start_link,[Port,Module]}, % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []        % Modules  = [Module] | dynamic
              },
              % Client instance supervisor
              {   organic.logger.relp.con_sup,
                  {supervisor,start_link,[{local, organic.logger.relp.con_sup}, .organic.logger.relp.con_sup, [Module]]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              },
              % Session supervisor
              {   organic.logger.relp.session_sup,
                  {supervisor,start_link,[{local, organic.logger.relp.session_sup}, .organic.logger.relp.session_sup, []]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.