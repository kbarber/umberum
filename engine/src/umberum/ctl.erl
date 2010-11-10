%% --------------------------
%% @copyright 2010 Bob.sh Limited
%% @doc Umberum control module.
%%
%% This module controls the umberum erlang processes. It uses rpc techniques
%% to run erlang commands on a running Umberum node.
%% 
%% @end
%% --------------------------
-module(umberum.ctl).

-export([start/0,
	 init/0,
	 process/1
	]).

%%-----------------------------
%% Module
%%-----------------------------

start() ->
    case .init:get_plain_arguments() of
	[SNode | Args] ->
	    SNode1 = case .string:tokens(SNode, "@") of
			 [_Node, _Server] ->
			     SNode;
			 _ ->
			     case .net_kernel:longnames() of
				 true ->
				     SNode ++ "@" ++ .inet_db:gethostname() ++
					 "." ++ .inet_db:res_option(domain);
				 false ->
				     SNode ++ "@" ++ .inet_db:gethostname();
				 _ ->
				     SNode
			     end
		     end,
	    Node = list_to_atom(SNode1),
        case .rpc:call(Node, ?MODULE, process, [Args]) of
            {badrpc, Reason} ->
                .io:format("Failed RPC connection to the node ~p: ~p~n",
                    [Node, Reason]),
                %% TODO: show minimal start help
                ok;
            ok ->
                halt();
            S ->
                S
        end,
	    halt();
	_ ->
	    %% TODO: print_usage() function
	    halt()
    end.

init() ->
    .ets:new(kod_ctl_cmds, [named_table, set, public]),
    .ets:new(kod_ctl_host_cmds, [named_table, set, public]).


%%-----------------------------
%% Process
%%-----------------------------

process(["stop"]) ->
    .init:stop();

process(["restart"]) ->
    .init:restart(),
    ok;

process(["mnesia"]) ->
    .io:format("~p~n", [.mnesia:system_info(all)]),
    ok;

process(["mnesia", "info"]) ->
    .mnesia:info(),
    ok;

process(["toolbar"]) ->
    .toolbar:start(),
    ok;

process(["webtool"]) ->
    .webtool:start(),
    ok;

process(["etop"]) ->
    .etop:start(),
    ok;

process(["mnesia", Arg]) when is_list(Arg) ->
    case catch .mnesia:system_info(list_to_atom(Arg)) of
	    {'EXIT', Error} -> .io:format("Error: ~p~n", [Error]);
	    Return -> .io:format("~p~n", [Return])
    end,
    ok.

