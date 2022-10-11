-module(gossip).
% -behaviour(gen_server).
-export([start_link/1, add_message/1, s/3, init/1, handle_cast/2,createNodes/1]).
% -import(whiteList,[get_whitelist/1]).

% -import(string, [to_atom/2]).
% -export([start/3]).

% start(NumNodes, Topology, Algorithm) ->
%     B = erlang:system_time().
%     if
%         Topology == "2D" 

% API

% Need to check this line
start_link(NodeName) ->
    % gen_server:start_link({local, gossip}, gossip, [], []).
    gen_server:start_link(gossip, [], name=NodeName).

add_message({Pid, Message, Number, Topo, NumNodes}) ->
    % io:fwrite([Pid, Message, Number, Topo, NumNodes]),
    gen_server:cast(Pid, {add_message, {Pid, Message, Number, Topo, NumNodes}}).

s(N, B, Topo) ->
    Messages = [],
    Blacklist = masterNode:get_blacklist({global:whereis_name("nodeMaster"), 999999, Topo, Messages}),
    % Blacklist = masterNode:get_blacklist("nodeMaster"),
    io:fwrite("Blcklist: ~w" , [Blacklist]),
    Bllen = length(Blacklist),           %not sure about this as kernel:length ???
    if 
        % Topo == "line" or Topo == "2D"->
        (Topo == "line") or (Topo == "2D")->
            Threshold = 0.1;
        true ->
            Threshold = 0.5
    end,

    Temp = Bllen / N,
    if 
        Temp >= Threshold ->
            io:put_chars("Time = ~p", [statistics(wall_clock) - B]),
            io:fwrite("write"),
            % Process.exit(self(),:kill)
            exit(self(),kill);
        true ->
            io:fwrite("wrong")
    end,
    s(N, B, Topo).

% SERVER

init(Messages) ->
    % io:fwrite("Print one init ~p", [Messages]),
    {ok, Messages}.

handle_cast({add_message, {Pid, New_message, Number, Topo, NumNodes}}, Messages) ->
    io:fwrite("Print handle_cast_gossip ~w", [Messages]),
    if 
        Messages == 9 ->
            io:fwrite("Added blacklist -- Handle cast"),
            masterNode:add_blacklist(global:whereis_name("nodeMaster"), Number);
        true ->
            continue
    end,
    io:fwrite("Added whitelist -- Handle cast ~p ~p" ,[global:whereis_name("nodeMaster"), Pid] ),

    % R = masterNode:get_whitelist({global:whereis_name("nodeMaster"), Number, Topo, NumNodes}),
    % WhitelistNodeName = list_to_atom("nodeMaster"),
    % R = masterNode:get_whitelist({WhitelistNodeName, Number, Topo, NumNodes}),

    % R = whiteList:get_whitelist([global:whereis_name("nodeMaster"), Number, Topo, NumNodes]),
    R = masterNode:get_blacklist({global:whereis_name("nodeMaster"), Number, Topo, NumNodes}),
    io:fwrite("R ~p", [R]),
    NodeName = list_to_atom(string:concat("node",[R])),
    timer:sleep(1000),
    gossip:add_message({global:whereis_name(NodeName), New_message, R, Topo, NumNodes}),
    {noreply, Messages+1}.


% handle_call(gossip, _from, Messages)->
%         {reply, Messages, Messages}.


createNodes(Times) ->
    if Times > 0 ->
        % Messages = [],
        NodeName = list_to_atom(string:concat("node", integer_to_list(Times))) ,
        io:fwrite("GOSSIP ~p", [NodeName]),
        {ok, Pid} = gen_server:start_link({global,NodeName}, gossip, [], []),
        io:fwrite("Pid ~p", [Pid]),
        % io:fwrite("output"),
        % {ok, Pid} = start_link(NodeName),
        % {ok, Pid}= start_link(),
        global:register_name(NodeName,Pid),
        createNodes(Times-1);
        true ->
            continue
    end.