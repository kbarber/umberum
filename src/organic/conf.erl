%% --------------------------
%% @copyright 2010 Kenneth Barber
%% @doc This module provides functions for obtaining global Organic 
%% configuration.
%% 
%% @end
%% --------------------------

-module(organic.conf).
-export([get_item/1]).

%%-------------------------------------------------------------------------
%% @doc Get a single configuration item. This fails with badmatch if the item
%% does not exist.
%% @end
%%-------------------------------------------------------------------------
get_item(Item) ->
    {ok,Value} = .application:get_env(Item),
    Value.
