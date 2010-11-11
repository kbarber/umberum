%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc Application definition for umberum.event.relp.app
%% 
%% @end
%% --------------------------

{application, 'umberum.input.relp',
 [{description, "Umberum RELP Input Plugin"},
  {vsn, "1"},
  {modules, []},
  {registered, []},
  {applications, [kernel,stdlib,sasl]},
  {mod, {umberum.input.relp.relp_app,[]}}
 ]}.
