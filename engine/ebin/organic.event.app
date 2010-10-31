%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc Application definition for organic.event
%% 
%% @end
%% --------------------------

{application, 'organic.event',
 [{description, "Organic Event Engine"},
  {vsn, "1"},
  {modules, []},
  {registered, []},
  {applications, [kernel,stdlib,sasl]},
  {mod, {organic.event.event_app,[]}}
 ]}.
