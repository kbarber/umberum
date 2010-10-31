%% --------------------------
%% @copyright 2010 Kenneth Barber
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
-define(CONF(ITEM), .organic.conf:get_item(ITEM)).
