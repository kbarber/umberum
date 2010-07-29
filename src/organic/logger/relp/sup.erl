-module(.organic.logger.relp.sup).
-author('saleyn@gmail.com').

-behaviour(supervisor).

%% Internal API
-export([start_client/0]).

%% Supervisor callbacks
-export([start_link/0, stop/1, init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,    2222).

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client() ->
    .supervisor:start_child('.organic.logger.relp.client_sup', []).

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
start_link() ->
    ListenPort = get_app_env(listen_port, ?DEF_PORT),
    .supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, .organic.logger.relp.con_fsm]).

stop(_S) ->
    ok.

init([Port, Module]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Listener
              {   .organic.logger.relp.listener,           % Id       = internal id
                  {.organic.logger.relp.listener,start_link,[Port,Module]}, % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []        % Modules  = [Module] | dynamic
              },
              % Client instance supervisor
              {   .organic.logger.relp.client_sup,
                  {supervisor,start_link,[{local, '.organic.logger.relp.client_sup'}, ?MODULE, [Module]]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    };

init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {Module,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
get_app_env(Opt, Default) ->
    case .application:get_env(.application:get_application(), Opt) of
    {ok, Val} -> Val;
    _ ->
        case .init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error       -> Default
        end
    end.
