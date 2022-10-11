-module(select_topology_line).

-export([main/2, createNodesGossip/2, createNodesPushSum/2, nodesOfGossipAlgo/4, nodesOfPushSumAlgo/5]).

main(NumOfNodes, Algorithm) ->
  case Algorithm of 
    "gossip" ->
      createNodesGossip(NumOfNodes, NumOfNodes),
      ConTime = erlang:monotonic_time()/10000,
      register(node_termination, spawn(node_termination, terminate_node, [ConTime])),
      NodeWithRumor = rand:uniform(NumOfNodes),
      NodeName = list_to_atom("nodesOfGossipAlgo" ++ integer_to_list(NodeWithRumor)),
      Pid = whereis(NodeName),
      Pid ! {listRumors, NodeWithRumor, NumOfNodes, []};

    "push-sum" ->
      createNodesPushSum(NumOfNodes, NumOfNodes),
      ConTime = erlang:monotonic_time()/10000,
      register(node_termination, spawn(node_termination, terminate_node, [ConTime])),
      NodeWithRumor = rand:uniform(NumOfNodes),
      NodeName = list_to_atom("nodesOfPushSumAlgo" ++ integer_to_list(NodeWithRumor)),
      Pid = whereis(NodeName),
      Pid ! {listRumors, 0, 1, NodeWithRumor, NumOfNodes};

    _->
      io:fwrite("Invalid Algorithm!!")
  end.

%Gossip Node Creation
createNodesGossip(0, FixedNumOfNodes) ->
  io:format("Number of nodes is ~p~n", [FixedNumOfNodes]),
  done;

createNodesGossip(NumOfNodes,FixedNumOfNodes ) when NumOfNodes > 0 ->
  Pid = spawn(select_topology_line, nodesOfGossipAlgo, [0, NumOfNodes, FixedNumOfNodes, []]),
  NodeName = list_to_atom("nodesOfGossipAlgo" ++ integer_to_list(NumOfNodes)),
  register(NodeName, Pid),
  createNodesGossip(NumOfNodes - 1, FixedNumOfNodes).

nodesOfGossipAlgo(Count, NumOfNodes, FixedNumOfNodes, List) ->
  receive
    {listRumors, Pos, FixedNumOfNodes, ListRumor} ->
      io:format("Received rumor at node ~p ~p ~n", [Pos, Count]),
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
      Randval = rand:uniform(2),
      io:format("Random value ~p ~p ~n", [Randval, FixedNumOfNodes]),
      if
        (Randval == 2) ->
          if (Pos + 1 > FixedNumOfNodes) ->
            SenTo = Pos - 1;
          true ->
            SenTo = Pos + 1
          end;
        true -> 
          if (Pos - 1 =< 0) ->
            SenTo = Pos + 1;
          true ->
            SenTo = Pos - 1
          end            
      end,
      NameOfNei = list_to_atom("nodesOfGossipAlgo" ++ integer_to_list(SenTo)),
      PidOfNei = whereis(NameOfNei),
      io:format("Sending the rumor to ~p~n", [SenTo]),
      PidOfNei ! {listRumors, SenTo, FixedNumOfNodes, ListR1},
      nodesOfGossipAlgo(Count + 1, NumOfNodes, FixedNumOfNodes, List2 )
  end.

%Push-Sum Node Creation
createNodesPushSum(0, FixedNumOfNodes) ->
  io:format("Number of nodes is ~p~n", [FixedNumOfNodes]),
  done;

createNodesPushSum(NumOfNodes, FixedNumOfNodes) when NumOfNodes > 0 ->
  Pid = spawn(select_topology_line, nodesOfPushSumAlgo, [NumOfNodes, 1, 0, NumOfNodes, FixedNumOfNodes]),
  NodeName = list_to_atom("nodesOfPushSumAlgo" ++ integer_to_list(NumOfNodes)),
  register(NodeName, Pid),
  createNodesPushSum(NumOfNodes - 1, FixedNumOfNodes).

nodesOfPushSumAlgo(S, W, CountRound, NumOfNodes, FixedNumOfNodes) ->
  if
    CountRound == 3 ->
      whereis(node_termination) ! over;
    true ->
      done
  end,
  receive
    {listRumors, NewS, NewW, Pos, FixedNumOfNodes} ->
      io:format("Received rumor at node ~p~n", [Pos]),
      Ratio = (S + NewS)/(W + NewW) - S / W,
      Randval = rand:uniform(2),
      if
        (Randval == 2) ->
          if (Pos + 1 > FixedNumOfNodes) ->
            SenTo = Pos - 1;
          true ->
            SenTo = Pos + 1
          end;
        true -> 
          if (Pos - 1 =< 0) ->
            SenTo = Pos + 1;
          true ->
            SenTo = Pos - 1
          end            
      end,
      NameOfNei = list_to_atom("nodesOfPushSumAlgo" ++ integer_to_list(SenTo)),
      PidOfNei = whereis(NameOfNei),
      io:format("Sending the rumor to ~p~n", [SenTo]),
      PidOfNei ! {listRumors, S / 2, W / 2, SenTo, FixedNumOfNodes},
      if
        abs(Ratio) < 0.0000000001 ->
          nodesOfPushSumAlgo((S + NewS) / 2, (W + NewW) / 2, CountRound + 1, NumOfNodes, FixedNumOfNodes);
        true ->
          nodesOfPushSumAlgo((S + NewS) / 2, (W + NewW) / 2, 0, NumOfNodes, FixedNumOfNodes)
      end
  end.