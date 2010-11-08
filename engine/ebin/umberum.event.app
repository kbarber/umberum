%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc Application definition for umberum.event
%% 
%% @end
%% --------------------------

{application, 'umberum.event',
 [{description, "Umberum Event Engine"},
  {vsn, "1"},
  {modules, []},
  {registered, []},
  {applications, [kernel,stdlib,sasl]},
  {mod, {umberum.event.event_app,[]}}
 ]}.
