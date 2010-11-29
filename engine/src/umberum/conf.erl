%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc This module provides functions for obtaining global Umberum 
%% configuration.
%% 
%% @end
%% --------------------------

-module(umberum.conf).
-export([get_item/1]).

%%-------------------------------------------------------------------------
%% @doc Get a single configuration item. This fails with badmatch if the item
%% does not exist.
%% @end
%%-------------------------------------------------------------------------
get_item(Item) ->
  {ok,Value} = .application:get_env(Item),
  Value.
