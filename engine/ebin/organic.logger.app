%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc Application definition for organic.logger
%% 
%% @end
%% --------------------------

{application, 'organic.logger',
 [{description, "Organic Log Receiver"},
  {vsn, "1"},
  {modules, []},
  {registered, []},
  {applications, [kernel,stdlib,sasl]},
  {mod, {organic.logger.app,[]}}
 ]}.
