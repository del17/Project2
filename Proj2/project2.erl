-module(project2).
-export([main/0]).
-import(gossip,[createNodes/1]).
main() ->
    % numNodes topology algorithm
    Values = string:strip(io:get_line("Project2 "), right, $\n),
    [NumOfNodesFromInput, Topology, Algo] = string:tokens(Values, " "),
    NumOfNodes=list_to_integer(NumOfNodesFromInput),
    io:format("nn: ~p~n", [NumOfNodes]),

    % ?????   b = System.system_time(:millisecond)
    B=statistics(wall_clock),
    global:register_name("tanvidelora", self()),
    case Topology of
        
        "2D"->
            Sqrt=round(math:floor(math:sqrt(NumOfNodes))),
            RoundNumNodes=round(math:pow(Sqrt, 2)),
            StartingNode=rand:uniform(RoundNumNodes),
            case Algo of
                "Gossip"->
                    gossip:createNodes(RoundNumNodes),
                    %    {:ok, pid1} = GenServer.start_link(MasterNode, [], name: :"nodeMaster")
                    {ok, Pid1}=gen_server:start_link(masterNode, [], "nodeMaster"), %start_link(Module, Args, Options)
                    global:register_name("nodeMaster", Pid1),
                    global:sync(),
                    NodeName=string:to_atom("node ~p",[StartingNode]),                
                    gossip:add_message(global:whereis_name(NodeName), "Gossip", StartingNode, Topology, RoundNumNodes),
                    gossip:s(RoundNumNodes, B, Topology);
                "PushSum"->
                    pushsum:createNodes(RoundNumNodes),
                    {ok, Pid2}=gen_server:start_link(masterNode, [], "nodeMaster"),
                    global:register_name("nodeMaster", Pid2),
                    global:sync(),
                    NodeNamePS=string:to_atom("node ~p",[StartingNode]),
                    pushsum:add_message(global:whereis_name(NodeNamePS), "Push-sum",  StartingNode, Topology, RoundNumNodes, 0, 0),
                    pushsum:s(RoundNumNodes, B, Topology)
            end;
        _->
            StartingNodeNN = rand:uniform(NumOfNodes),
            io:fwrite("StartingNodeNN = ~p", [StartingNodeNN]), 
            % StartingNodeNN = string:strip(StartingNodeMN,left,$\00),
            case Algo of
                "Gossip"->
                    % Messages = [],
                    gossip:createNodes(NumOfNodes),
                    %    {:ok, pid1} = GenServer.start_link(MasterNode, [], name: :"nodeMaster")
                    % {ok, Pid1}=gen_server:start_link(masterNode, [], "nodeMaster"), %start_link(Module, Args, Options)
                    {ok, Pid1} = gen_server:start_link({global,"nodeMaster"}, masterNode, [], []),
                    global:register_name("nodeMaster", Pid1),
                    global:sync(),
                    io:fwrite("Node PID1= ~p", [Pid1]),  
                    NodeName=list_to_atom(string:concat("node",integer_to_list(StartingNodeNN))),  
                    io:fwrite("Node = ~p", [NodeName]),    
                    io:fwrite("Node = ~p", [global:whereis_name(NodeName)]),             
                    gossip:add_message({global:whereis_name(NodeName), "Gossip", StartingNodeNN, Topology, NumOfNodes}),
                    io:fwrite("B: ~p" , [B]),
                    gossip:s(NumOfNodes, B, Topology);
                "PushSum"->
                    pushsum:createNodes(NumOfNodes),
                    {ok, Pid2}=gen_server:start_link(masterNode, [], "nodeMaster"),
                    global:register_name("nodeMaster", Pid2),
                    global:sync(),
                    NodeNamePS=string:to_atom("node ~p",[StartingNodeNN]),
                    pushsum:add_message(global:whereis_name(NodeNamePS), "Push-sum",  StartingNodeNN, Topology, NumOfNodes, 0, 0),
                    pushsum:s(NumOfNodes, B, Topology)
            end
    end.

    % for algorithms
    % if 
    %     Algo=="Gossip"->
    %         gossip:createNodes(RoundNumNodes),
    %         %    {:ok, pid1} = GenServer.start_link(MasterNode, [], name: :"nodeMaster")
    %         {ok, Pid1}=gen_server:start_link(masterNode, [], "nodeMaster"), %start_link(Module, Args, Options)
    %         global:register_name("nodeMAster", Pid1),
    %         global:sync(),
    %         NodeName=string:to_atom("node ~p",[StartingNode]),
    %         if 
    %             Topology=="2D"->
    %                 gossip:add_message(global:whereis_name(NodeName), "Gossip", StartingNode, Topology, RoundNumNodes),
    %                 gossip:s(RoundNumNodes, B, Topology);
    %             true->
    %                 gossip:add_message(global:whereis_name(NodeName), "Gossip", StartingNode, Topology, NumOfNodes),
    %                 gossip:s(NumOfNodes, B, Topology)
    %         end
    % end,

    % if   
    %     Algo=="PushSum"->
    %         pushsum:createNodes(RoundNumNodes),
    %         {ok, Pid2}=gen_server:start_link(masterNode, [], "nodeMaster"),
    %         global:register_name("nodeMAster", Pid2),
    %         global:sync(),
    %         NodeNamePS=string:to_atom("node ~p",[StartingNode]),
    %         if 
    %             Topology=="2D"->
    %                 pushsum:add_message(global:whereis_name(NodeNamePS), "Push-sum",  StartingNode, Topology, RoundNumNodes, 0, 0),
    %                 pushsum:s(RoundNumNodes, B, Topology);
    %             true-> 
    %                 pushsum:add_message(global:whereis_name(NodeNamePS), "Push-sum",  StartingNode, Topology, NumOfNodes, 0, 0),
    %                 pushsum:s(NumOfNodes, B, Topology)
    %     end
    % end.



    % for topologys
% if 
%     Topology=="Full"->
%         io:fwrite("Full~n");
%     Topology=="2D"->
%         io:fwrite("2D~n");
%     Topology=="Line"->
%         io:fwrite("Line~n");
%     Topology=="Imperfect3D"->
%         io:fwrite("Imperfect3D~n")
% end.