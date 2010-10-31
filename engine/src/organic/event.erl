%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc Event API
%%
%% This module provides a client API for interfacing with the event processing
%% engine.
%% 
%% @end
%% --------------------------

-module(organic.event).
-export([process/1]).

-include_lib("include/common.hrl").

%% @doc Send event to the processing engine.
%%
%% This function receives an Event object and passes it to the processing 
%% engine in an asynchronous manner.
%%
%% @end
process(Event) ->
    ?DEBUGF("Event: ~p~n", [Event]),

    % Start new process
    {ok, Proc} = .organic.event.proc.proc_sup:start_child(),

    % Send new process our event
    .gen_fsm:send_event(Proc, {process,Event,callback,self()}),

    % Wait for response
    receive 
        {ok, process_id, Id} ->
            {ok, Id}
    end.
