%% --------------------------
%% @copyright 2010 Kenneth Barber
%% @doc Application module for Organic Logger
%% 
%% @end
%% --------------------------

-module(organic.logger.sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%% @doc Start and link to this module
start_link() ->
    .supervisor:start_link(.organic.logger.sup, []).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

init(_Args) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
                % RELP Listener
                {   .organic.logger.relp.relp_sup,           % Id       = internal id
                  {.organic.logger.relp.relp_sup,start_link,[]}, % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                                  % Type     = worker | supervisor
                  [.organic.logger.relp.relp_sup]          % Modules  = [Module] | dynamic
                },
                % Syslog supervisor
                {   organic.logger.syslog_3164.decode_sup,
                  {.organic.logger.syslog_3164.decode_sup,start_link,[]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
                },
                % Tokenizer supervisor
                {   organic.logger.tokenizer.tokenizer_sup,
                  {.organic.logger.tokenizer.tokenizer_sup,start_link,[]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
                },
                % Route supervisor
                {   organic.logger.route.route_sup,
                  {.organic.logger.route.route_sup,start_link,[]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
                },
	            % File writer
                {   organic.logger.file.writer_sup,
                  {.organic.logger.file.writer_sup,start_link,[]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
                },
	            % Mongodb writer
                {   organic.logger.mongodb.writer_sup,
                  {.organic.logger.mongodb.writer_sup,start_link,[]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
                }
            ]
         }
    }.
