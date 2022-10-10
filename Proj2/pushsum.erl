-module(pushsum).
-export([start_link/0, add_message/7, s/3, init/1, handle_cast/2,createNodes/1 ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_message(Pid, Message, Number, Topo, NumNodes, HalfS, HalfW) ->
    gen_server:cast(Pid, {add_message, Message, Number, Topo, NumNodes, HalfS, HalfW}).

s(N, B, Topo) ->
    Blacklist = masterNode:get_blacklist(global:whereis_name("nodeMaster")),
    Bllen = kernel:length(Blacklist),
    if 
        (Topo == "line") or (Topo == "2D")->
            Threshold = 0.1;
        true ->
            Threshold = 0.5
    end,

    Temp = Bllen / N,
    if 
        Temp >= Threshold ->
            io:put_chars("Time = ~p", [statistics(wall_clock) - b]),
            % Process.exit(self(),:kill)
            exit(self(),kill)
    end,
    s(N, B, Topo).

% SERVER


init(Messages) ->
    {ok, Messages}.

handle_cast({add_message, New_message, Number, Topo, NumNodes, HalfS, HalfW}, Messages) ->
    NewS=lists:nth(1,Messages)+HalfS,      %starts with 1
    NewW=lists:nth(2,Messages)+HalfW,  

    OldRatio = lists:nth(1,Messages)/lists:nth(2,Messages),
    NewRatio = NewS/NewW,
    OldCount=0,
    if
        OldRatio - NewRatio < 0.0000000001 ->
        Check1=lists:nth(3,Messages) == 2,
            if Check1->
                masterNode:add_blacklist(global:whereis_name("nodeMaster"), Number)
            end,
        OldCount1=lists:nth(3,Messages)+1            %not sure what to do about this
    end,
    HalfS1=newS/2,
    HalfW1=newW/2,

    NewS1=NewS-HalfS1,
    NewW1 = NewW - HalfW1,

    if 
        OldCount1=/=0->
            NewState=[NewS1,NewW1,OldCount1];
        true->
            NewState=[NewS1,NewW1,OldCount]
    end,   

    R = masterNode:get_whitelist(global:whereis_name("nodeMaster"), Number, Topo, NumNodes),
    NodeName = string:to_atom("node ~p",[r]),
    % :timer.sleep 1, 
    timer:sleep(1),
    pushsum:add_message(global:whereis_name(NodeName), New_message, R, Topo, NumNodes, HalfS, HalfW),
    {noreply, NewState}.
    
createNodes(Times) ->
    if 
        Times > 0 ->
        NodeName = string:to_atom("node ~p",[times]),
        {ok, Pid} = gen_server:start_link(gossip, 1, NodeName),
        global:register_name(NodeName,Pid),
        createNodes(Times-1)
    end.