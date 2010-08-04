%% --------------------------
%% @copyright 2010 Kenneth Barber
%% @doc A place to store misc functions that are used everywhere in Organic
%% development projects in Erlang.
%%
%% Anything that has more complexity then a single function can be found 
%% elsewhere.
%% 
%% @end
%% --------------------------

-module(organic.util).
-export([bin_to_int/1, bin_part/2, bin_part/3]).

%%-------------------------------------------------------------------------
%% @doc Translate a number stored in binary to an integer.
%% @end
%%-------------------------------------------------------------------------
bin_to_int(Binary) ->
    %TODO: find a built-in way of doing this
    %TODO: consider putting this in a shared place
    list_to_integer(binary_to_list(Binary)).

%%-------------------------------------------------------------------------
%% @doc Extract a piece of Binary from the beginning of the variable to Len.
%%
%% My own version of binary:part/2 since Ubuntu 10.04 doesn't support it
%% @end
%%-------------------------------------------------------------------------
bin_part(Binary, Len) ->
    bin_part(Binary, 1, Len).

%%-------------------------------------------------------------------------
%% @doc Extract a piece of Binary from Start index of the binary to Len.
%%
%% My own version of binary:part/3 since Ubuntu 10.04 doesn't support it
%% @end
%%-------------------------------------------------------------------------
bin_part(Binary, Start, Len) ->
     list_to_binary(.lists:sublist(binary_to_list(Binary), Start, Len)).
