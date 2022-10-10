-module(masterNode).
-behaviour(gen_server).
-export([add_blacklist/2, get_blacklist/1, get_whitelist/1, whiteRandom/4, init/1, handle_call/3, handle_cast/2]).

add_blacklist(Pid, Message)->
    gen_server:cast(Pid, {add_blacklist, Message}).

get_blacklist(Pid)->
    io:fwrite("Pid ~p", [Pid]),
    gen_server:call(Pid, get_blacklist).

% get_whitelist(Pid, NodeId, Topo, NumNodes)->
%     gen_server:call(Pid, {get_whitelist, NodeId, Topo, NumNodes}).

get_whitelist({Pid, NodeId, Topo, NumNodes})->
    gen_server:call(Pid, {get_whitelist, {Pid, NodeId, Topo, NumNodes}}, infinity).

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

% handle_call(get_blacklist, _From, Messages)->
%     % {Message, Messages2} = get_blacklist(Messages),
%     {reply, Messages, Messages}.

handle_cast({add_blacklist, New_message}, Messages) ->
    {noreply, [New_message | Messages]}.

handle_call({get_whitelist, Pid, NodeId, Topo, NumNodes}, _From, Messages) ->
    io:fwrite("HandleCall for get_whitelist ~p", [Messages]),
    Nodernd = masterNode:whiteRandom(Topo, NumNodes, NodeId, Messages),
    {reply, Nodernd, Messages}.