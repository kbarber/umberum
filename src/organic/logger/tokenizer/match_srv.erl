%% --------------------------
%% @copyright 2010 Kenneth Barber
%% @doc Initial matcher to determine which pattern to apply for
%% tokenization.
%% @end
%% --------------------------

-module(organic.logger.tokenizer.match_srv).
-behaviour(gen_server).
-export([]).

%% --------------------------
%% @doc
%% @end
%% --------------------------
init([])->
     ok.

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
