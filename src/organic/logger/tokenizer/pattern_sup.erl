%% --------------------------
%% @copyright 2010 Kenneth Barber
%% @doc Pattern supervisor
%% 
%% @end
%% --------------------------

-module(.organic.logger.tokenizer.pattern_sup).

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
        {_SupFlags = {simple_one_for_one, 10, 1},
            [
              % Pattern worker
              {   undefined,                                            % Id       = internal id
                  {.organic.logger.tokenizer.pattern_srv,start_link,[]},  % StartFun = {M, F, A}
                  permanent,                                            % Restart  = permanent | transient | temporary
                  2000,                                                 % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                               % Type     = worker | supervisor
                  [.organic.logger.tokenizer.pattern_srv]                 % Modules  = [Module] | dynamic
              }
	    ]
	}
    }.
