%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc A place to store misc functions that are used everywhere in Umberum
%% development projects in Erlang.
%%
%% Anything that has more complexity then a single function can be found 
%% elsewhere.
%% 
%% @end
%% --------------------------

-module(umberum.util).
-export([bin_to_int/1]).

%%-------------------------------------------------------------------------
%% @doc Translate a number stored in binary to an integer.
%% @end
%%-------------------------------------------------------------------------
bin_to_int(Binary) ->
    %TODO: find a built-in way of doing this
    %TODO: consider putting this in a shared place
    list_to_integer(binary_to_list(Binary)).

