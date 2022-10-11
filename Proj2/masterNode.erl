-module(masterNode).
-export([add_blacklist/2, get_blacklist/1, init/1, handle_call/3, handle_cast/2, whiteRandom/4]).
% -export([add_blacklist/2, get_blacklist/1, get_whitelist/1, whiteRandom/4, init/1, handle_call/3, handle_cast/2]).
add_blacklist(Pid, Message)->
    gen_server:cast(Pid, {add_blacklist, Message}).

get_blacklist({Pid, NodeId, Topo, NumNodes})->
    io:fwrite("blacklistwhitelistPid ~p", [Pid]),
    gen_server:call(Pid, {get_blacklist, [Pid, NodeId, Topo, NumNodes]}).

% get_whitelist(Pid, NodeId, Topo, NumNodes)->
%     gen_server:call(Pid, {get_whitelist, NodeId, Topo, NumNodes}).

% get_whitelist({Pid, NodeId, Topo, NumNodes})->
%     io:fwrite("Whitelist Pid ~p", [Pid]),
%     gen_server:call(Pid, {get_whitelist, Pid, NodeId, Topo, NumNodes}).

whiteRandom(Topo, NumNodes, NodeId, Messages)->
    NodeList = topologies:checkRandom(Topo, NumNodes, NodeId),
    NodeList2 = lists:filter(NodeList, fun(El) -> not lists:member(El, Messages) end),
    NodeLen = length(NodeList2),
    % TopoCheck = false,
    if 
        (Topo == "line") or Topo == ("2D") ->
            TopoCheck2 = true
    end,
    if 
        (NodeLen == 0) and (TopoCheck2 == true) ->
            timer:sleep(1000), %not sure about this
            exit(global:whereis_name("tanvidelora"),kill)
    end,
    if 
        NodeLen == 0 ->
            whiteRandom(Topo, NumNodes, NodeId, Messages);
        true ->
            RandomNeighbor = rand:uniform(NodeLen),
            lists:nth(RandomNeighbor - 1, NodeList2)
    end.
 

%Server
init(Messages)->
    io:fwrite("Print two init ~p", [Messages]),
    {ok, Messages}.

handle_call({func, [Pid, NodeId, Topo, NumNodes]}, _from, Messages)->
    % {Message, Messages2} = get_blacklist(Messages),
    % Out = list_to_atom("Blacklist"), 
    if 
        NodeId == 999999 ->
        io:fwrite("HandleCall for get_blacklist ~p ~p", [NodeId, Pid]),
        {reply, Messages, Messages};
    true -> 
        io:fwrite("HandleCall for get_whitelist ~p ~p", [NodeId, Pid]),
        Nodernd = masterNode:whiteRandom(Topo, NumNodes, NodeId, Messages),
        {reply, Nodernd, Messages}
    end.
    

handle_cast({add_blacklist, New_message}, Messages) ->
    {noreply, [New_message | Messages]}.

% handle_call({get_whitelist, Pid, NodeId, Topo, NumNodes}, _From, Messages) ->
%     io:fwrite("HandleCall for get_whitelist ~p ~p", [Messages, Pid]),
%     Nodernd = masterNode:whiteRandom(Topo, NumNodes, NodeId, Messages),
%     {reply, Nodernd, Messages}.