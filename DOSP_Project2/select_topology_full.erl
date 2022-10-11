-module(select_topology_full).
-export([main/2, createNodesGossip/2, createNodesPushSum/1, nodesOfGossipAlgo/3, nodesOfPushSumAlgo/3]).

main(NumOfNodes, Algorithm) ->
  case Algorithm of
    "gossip" ->
      createNodesGossip(NumOfNodes, NumOfNodes),
      ConTime = erlang:monotonic_time()/10000,
      register(node_termination, spawn(node_termination, terminate_node, [ConTime])),
      NodeWithRumor = rand:uniform(NumOfNodes),
      NodeName = list_to_atom("nodesOfGossipAlgo" ++ integer_to_list(NodeWithRumor)),
      Pid = whereis(NodeName),
      Pid ! {listRumors, NumOfNodes, []};

    "push-sum" ->
      createNodesPushSum(NumOfNodes),
      ConTime = erlang:monotonic_time()/10000,
      register(node_termination, spawn(node_termination, terminate_node, [ConTime])),
    
      NodeWithRumor = rand:uniform(NumOfNodes),
      NodeName = list_to_atom("nodesOfPushSumAlgo" ++ integer_to_list(NodeWithRumor)),
      Pid = whereis(NodeName),
      Pid ! {listRumors, 0, 1, NumOfNodes};

    _->
      io:fwrite("Invalid Algorithm!!")
  end.

%Gossip Node Creation
createNodesGossip(0, NumOfNodes) ->
  io:format("Number of nodes is ~p~n", [NumOfNodes]),
  done;

createNodesGossip(NumOfNodes, FixedNumOfNodes) when NumOfNodes > 0 ->
  Pid = spawn(select_topology_full, nodesOfGossipAlgo, [0, [], FixedNumOfNodes]),
  NodeName = list_to_atom("nodesOfGossipAlgo" ++ integer_to_list(NumOfNodes)),
  register(NodeName, Pid),
  createNodesGossip(NumOfNodes - 1, FixedNumOfNodes).

nodesOfGossipAlgo(Count, List, NumOfNodes) ->
  receive
    {listRumors, Num, ListRumor} ->
      io:format("Received rumor at node ~p, count ~p~n", [self(), Count]),
      if
        Count >= 10 ->
          % io:format("List ~w ~w ~n", [self(), pid_to_list(self())]),
          List1 = lists:append(List, [self()]),
          ListR1 = lists:append(ListRumor, List1),
          % io:format("List ~w ~n", [List1]),
          % io:format("ListR1 ~w ~n", [ListR1]),
          List2 = lists:uniq(ListR1),
          % io:format("List2 ~w ~n", [List2]),
          Length = length(List2),
          % io:format("Length ~w ~n", [Length]),
          if 
            Length == NumOfNodes ->
              % io:format("Number of node ~p ~p ~n", [Length, NumOfNodes]),
              whereis(node_termination) ! over;
            true ->
              % io:format("Number of node ~p ~p ~n", [Length, NumOfNodes])
              done
              % erlang:halt()
          end;
        true ->
          List2 = List,
          ListR1 = ListRumor,
          done
      end,
      NameOfNei = list_to_atom("nodesOfGossipAlgo" ++ integer_to_list(rand:uniform(Num))),
      io:format("Sending the rumor to ~p~n", [NameOfNei]),
      PidOfNei = whereis(NameOfNei),
      % io:format("Sending the rumor to ~p~n", [PidOfNei]),
      PidOfNei ! {listRumors, Num, ListR1},
      % io:format("After Sending the rumor to ~p~n", [PidOfNei]),
      % io:format("Print List before rounds ~w ~p ~n", [List2, NumOfNodes]),
      nodesOfGossipAlgo(Count + 1, List2, NumOfNodes)
      
  end.

%Push-Sum Node Creation
createNodesPushSum(0) ->
  done;

createNodesPushSum(NumOfNodes) when NumOfNodes > 0 ->
  Pid = spawn(select_topology_full, nodesOfPushSumAlgo, [NumOfNodes, 1, 0]),
  NodeName = list_to_atom("nodesOfPushSumAlgo" ++ integer_to_list(NumOfNodes)),
  register(NodeName, Pid),
  createNodesPushSum(NumOfNodes - 1).

nodesOfPushSumAlgo(S, W, CountRound) ->
  if
    CountRound == 3 ->
      whereis(node_termination) ! over;
    true ->
      done
  end,
  receive
    {listRumors, NewS, NewW, Num} ->
      io:format("Received rumor at node ~p~n", [self()]),
      Ratio = (S + NewS)/(W + NewW) - S / W,
      io:format("Ratio= ~p~n", [abs(Ratio)]),
      NameOfNei = list_to_atom("nodesOfPushSumAlgo" ++ integer_to_list(rand:uniform(Num))),
      PidOfNei = whereis(NameOfNei),
      PidOfNei ! {listRumors, S / 2, W / 2, Num},
      if
        abs(Ratio) < 0.0000000001 ->
          nodesOfPushSumAlgo((S + NewS) / 2, (W + NewW) / 2, CountRound + 1);
        true ->
          nodesOfPushSumAlgo((S + NewS) / 2, (W + NewW) / 2, 0)
      end
  end.