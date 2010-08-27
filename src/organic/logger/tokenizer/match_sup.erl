%% --------------------------
%% @copyright 2010 Kenneth Barber
%% @doc Match supervisor
%% 
%% @end
%% --------------------------

-module(.organic.logger.tokenizer.match_sup).

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
    {ok, Pid} = .supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    ?INFO("Started match supervisor, now starting workers."),
    start_child(Pid, ?CONF(tokenizer_match_procs)),
    {ok, Pid}.

start_child(_Pid, 0) ->
    ok;
start_child(Pid, Num) ->
    {ok,_Child} = .supervisor:start_child(Pid,[]),
    start_child(Pid, Num-1).

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
              % Match worker
              {   undefined,                                            % Id       = internal id
                  {.organic.logger.tokenizer.match_srv,start_link,[]},  % StartFun = {M, F, A}
                  permanent,                                            % Restart  = permanent | transient | temporary
                  2000,                                                 % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                               % Type     = worker | supervisor
                  [.organic.logger.tokenizer.match_srv]                 % Modules  = [Module] | dynamic
              }
	        ]
	    }
    }.
