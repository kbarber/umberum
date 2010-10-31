%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc Tokenizer supervisor
%% 
%% @end
%% --------------------------

-module(.organic.logger.tokenizer.tokenizer_sup).

-behaviour(supervisor).

%% Supervisor callbacks
-export([start_link/0, stop/1, init/1]).

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
        {_SupFlags = {one_for_one, 10, 1},
            [
              % Match supervisor
              {   .organic.logger.tokenizer.match_sup,                  % Id       = internal id
                  {.organic.logger.tokenizer.match_sup,start_link,[]},  % StartFun = {M, F, A}
                  permanent,                                            % Restart  = permanent | transient | temporary
                  infinity,                                             % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                                           % Type     = worker | supervisor
                  [.organic.logger.tokenizer.match_sup]                 % Modules  = [Module] | dynamic
              },
              % Pattern supervisor
              {   .organic.logger.tokenizer.pattern_sup,                  % Id       = internal id
                  {.organic.logger.tokenizer.pattern_sup,start_link,[]},  % StartFun = {M, F, A}
                  permanent,                                            % Restart  = permanent | transient | temporary
                  infinity,                                             % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                                           % Type     = worker | supervisor
                  [.organic.logger.tokenizer.pattern_sup]                 % Modules  = [Module] | dynamic
              }
	        ]
	    }
    }.
