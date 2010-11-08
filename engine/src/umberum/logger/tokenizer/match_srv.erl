%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc Initial matcher to determine which pattern to apply for
%% tokenization.
%% @end
%% --------------------------

-module(.umberum.logger.tokenizer.match_srv).
-behaviour(gen_server).

-export([start_link/0]).

%% gen_server call backs
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3, format_status/2]).

-include_lib("include/common.hrl").

%% --------------------------
%% @doc
%% @end
%% --------------------------
start_link() ->
    ?INFO("Starting match worker"),
    .gen_server:start_link(?MODULE, [], []).

%% --------------------------
%% @doc
%% @end
%% --------------------------
init([])->
    % Join group
    .pg2:create(?MODULE),
    ok = .pg2:join(?MODULE,self()),
    {ok,self()}.

%% --------------------------
%% @doc
%% @end
%% --------------------------
handle_call(_Request, {_Pid,_Tag}, _State) ->
    ok.

%% --------------------------
%% @doc
%% @end
%% --------------------------
handle_cast(_Request, _State)->
    ok.

%% --------------------------
%% @doc
%% @end
%% --------------------------
handle_info(_Info, _State)->
    ok.

%% --------------------------
%% @doc
%% @end
%% --------------------------
terminate(_Reason, _State)->
    ok.

%% --------------------------
%% @doc
%% @end
%% --------------------------
code_change(_OldVsn, State, _Extra)->
    {ok, State}.

%% --------------------------
%% @doc
%% @end
%% --------------------------
format_status(_Opt, [_PDict, _State]) ->
    [{data, [{"State", ok}]}].
