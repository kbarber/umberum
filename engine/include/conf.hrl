%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc Header file for configuration related macros
%%
%% @end
%% --------------------------

%% --------------------------
%% @doc A macro for grabbing global configuration.
%%
%% Usage:
%%
%%   Var = ?CONF(config_item_name).
%%
%% @end
%% --------------------------
-define(CONF(ITEM), .umberum.conf:get_item(ITEM)).
