-module(select_topology_imp3D).
-export([main/2, randNei2D/1, createNodesGossip/1, createNodesGossipLoop/4, currNeiGossip/4, nodesOfGossipAlgo/3, createNodesPushSum/1, createNodesPushSumLoop/4, currNeiPushSum/5, nodesOfPushSumAlgo/4]).

main(NumOfNodes, Algorithm) ->
  case Algorithm of 
    "gossip" ->
      createNodesGossip(NumOfNodes),
      Start = erlang:monotonic_time()/10000,
      register(node_termination, spawn(node_termination, terminate_node, [Start])),
      Sqrt = trunc(math:sqrt(NumOfNodes)),
      RumorA1 = rand:uniform(Sqrt),
      RumorA2 = rand:uniform(Sqrt),
      NodeName = list_to_atom("nodesOfGossipAlgo" ++ integer_to_list(RumorA1) ++ "_" ++ integer_to_list(RumorA2)),
      Pid = whereis(NodeName),
      Pid ! {listRumors, [RumorA1, RumorA2], NumOfNodes,[]};

    "push-sum" ->
      createNodesPushSum(NumOfNodes),
      Start = erlang:monotonic_time()/10000,
      register(node_termination, spawn(node_termination, terminate_node, [Start])),
      Sqrt = trunc(math:sqrt(NumOfNodes)),
      RumorA1 = rand:uniform(Sqrt),
      RumorA2 = rand:uniform(Sqrt),
      NodeName = list_to_atom("nodesOfPushSumAlgo" ++ integer_to_list(RumorA1) ++ "_" ++ integer_to_list(RumorA2)),
      Pid = whereis(NodeName),
      Pid ! {listRumors, 0, 1, [RumorA1, RumorA2], NumOfNodes};

      _->
        io:fwrite("Invalid Algorithm!!")
  end.

randNei2D([A1, A2]) ->
  List = [[0, -1], [-1, 0], [0, 1], [1, 0], [-1, 1], [1, -1], [-1 ,-1], [1, 1]],
  PosNei = lists:nth(rand:uniform(8), List),
  NeiA1 = lists:nth(1, PosNei) + A1,
  NeiA2 = lists:nth(2, PosNei) + A2,
  [NeiA1, NeiA2].

%Gossip Node Creation
createNodesGossip(NumOfNodes) ->
  Sqrt = trunc(math:sqrt(NumOfNodes)),
  createNodesGossipLoop(Sqrt, Sqrt, Sqrt, Sqrt).

createNodesGossipLoop(0, 0, _, FixedNumOfNodes) ->
  io:format("Number of nodes is ~p~n", [FixedNumOfNodes]),
  done;

createNodesGossipLoop(A1, A2, Sqrt, FixedNumOfNodes) ->
  Pid = spawn(select_topology_imp3D, nodesOfGossipAlgo, [0,[], FixedNumOfNodes]),
  NodeName = list_to_atom("nodesOfGossipAlgo" ++ integer_to_list(A1) ++ "_" ++ integer_to_list(A2)),
  register(NodeName, Pid),
  if
    A1 > 0 ->
      createNodesGossipLoop(A1 - 1, A2, Sqrt, FixedNumOfNodes);
    A2 > 0 ->
      createNodesGossipLoop(Sqrt, A2 - 1, Sqrt, FixedNumOfNodes);
    true ->
      done
  end.

currNeiGossip(A1, A2, Num, ListR1) ->
  PosNei = randNei2D([A1, A2]),
  NeiA1 = lists:nth(1, PosNei),
  NeiA2 = lists:nth(2, PosNei),
  NameOfNei = list_to_atom("nodesOfGossipAlgo" ++ integer_to_list(NeiA1) ++ "_" ++ integer_to_list(NeiA2)),
  PidOfNei = whereis(NameOfNei),
  if
    is_pid(PidOfNei) ->
      io:format("Sending the rumor to ~p~n", [NameOfNei]),
      PidOfNei ! {listRumors, [NeiA1, NeiA2], Num, ListR1},
      done;
    true ->
      currNeiGossip(A1, A2, Num, ListR1)
  end.

nodesOfGossipAlgo(Count, List, NumOfNodes) ->
  receive
    {listRumors, [A1, A2], Num, ListRumor} ->
      io:format("Received rumor at node ~p_~p~n", [A1, A2]),
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
      RandVal = rand:uniform(2),
      if
        RandVal == 1 ->
          currNeiGossip(A1, A2, Num, ListR1);
        true ->
          Sqrt = trunc(math:sqrt(Num)),
          SendToA1 = rand:uniform(Sqrt),
          SendToA2 = rand:uniform(Sqrt),
          NodeName = list_to_atom("nodesOfGossipAlgo" ++ integer_to_list(SendToA1) ++ "_" ++ integer_to_list(SendToA2)),
          Pid = whereis(NodeName),
          Pid ! {listRumors, [SendToA1, SendToA2], Num, ListR1},
          io:format("sending to ~p_~p~n", [SendToA1, SendToA2])
      end,

      nodesOfGossipAlgo(Count + 1, List2, NumOfNodes )
  end.


%Push-Sum Node Creation
createNodesPushSum(NumOfNodes) ->
  Sqrt = trunc(math:sqrt(NumOfNodes)),
  createNodesPushSumLoop(Sqrt, Sqrt, Sqrt, NumOfNodes).

createNodesPushSumLoop(0, 0, _, _) ->
  done;

createNodesPushSumLoop(A1, A2, Sqrt, NumOfNodes) ->
  Pid = spawn(select_topology_imp3D, nodesOfPushSumAlgo, [NumOfNodes, 1, [A1, A2], 0]),
  NodeName = list_to_atom("nodesOfPushSumAlgo" ++ integer_to_list(A1) ++ "_" ++ integer_to_list(A2)),
  register(NodeName, Pid),
  if
    A1 > 0 ->
      createNodesPushSumLoop(A1 - 1, A2, Sqrt, NumOfNodes - 1);
    A2 > 0 ->
      createNodesPushSumLoop(Sqrt, A2 - 1, Sqrt, NumOfNodes - 1);
    true ->
      done
  end.

currNeiPushSum(A1, A2, S, W, Num) ->
  PosNei = randNei2D([A1, A2]),
  NeiA1 = lists:nth(1, PosNei),
  NeiA2 = lists:nth(2, PosNei),
  NameOfNei = list_to_atom("nodesOfPushSumAlgo" ++ integer_to_list(NeiA1) ++ "_" ++ integer_to_list(NeiA2)),
  PidOfNei = whereis(NameOfNei),
  if
    is_pid(PidOfNei) ->
      io:format("Sending the rumor to ~p~n", [NameOfNei]),
      PidOfNei ! {listRumors, S, W, [NeiA1, NeiA2], Num},
      done;
    true ->
      currNeiPushSum(A1, A2, S, W, Num)
  end.

nodesOfPushSumAlgo(S, W, [A1, A2], CountRound) ->
  if
    CountRound == 3 ->
      whereis(node_termination) ! over;
    true ->
      done
  end,

  receive
    {listRumors, NewS, NewW, [A1, A2], Num} ->
      io:format("Received rumor at node ~p~n", [self()]),
      Ratio = (S + NewS)/(W + NewW) - S / W,
      RandVal = rand:uniform(2),
      if
        RandVal == 1 ->
          currNeiPushSum(A1, A2, S, W, Num);
        true ->
          Sqrt = trunc(math:sqrt(Num)),
          SendToA1 = rand:uniform(Sqrt),
          SendToA2 = rand:uniform(Sqrt),
          NodeName = list_to_atom("nodesOfPushSumAlgo" ++ integer_to_list(SendToA1) ++ "_" ++ integer_to_list(SendToA2)),
          Pid = whereis(NodeName),
          Pid ! {listRumors, S, W, [SendToA1, SendToA2], Num},
          io:format("Sending the rumor to ~p~p~n", [SendToA1, SendToA2])
      end,
      if
        abs(Ratio) < 0.0000000001 ->
          nodesOfPushSumAlgo((S + NewS) / 2, (W + NewW) / 2, [A1, A2], CountRound + 1);
        true ->
          nodesOfPushSumAlgo((S + NewS) / 2, (W + NewW) / 2, [A1, A2], 0)
      end
  end.