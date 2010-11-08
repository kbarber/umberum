%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc Application definition for umberum.logger
%% 
%% @end
%% --------------------------

{application, 'umberum.logger',
 [{description, "Umberum Log Receiver"},
  {vsn, "1"},
  {modules, []},
  {registered, []},
  {applications, [kernel,stdlib,sasl]},
  {mod, {umberum.logger.app,[]}}
 ]}.
