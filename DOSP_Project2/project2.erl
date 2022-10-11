-module(project2).
-export([main/0]).

main() ->
  Values = string:strip(io:get_line("Project2 "), right, $\n),
  [NumOfNodesFromInput, Topology, Algorithm] = string:tokens(Values, " "),
  NumOfNodes=list_to_integer(NumOfNodesFromInput),

case Topology of
    "full" ->
      select_topology_full:main(NumOfNodes, Algorithm);
    "2D" ->
      select_topology_2D:main(NumOfNodes, Algorithm);
    "line" ->
      select_topology_line:main(NumOfNodes, Algorithm);
    "imp3D" ->
      select_topology_imp3D:main(NumOfNodes, Algorithm);
    _ ->
      io:fwrite("Invalid Topology!")
  end.