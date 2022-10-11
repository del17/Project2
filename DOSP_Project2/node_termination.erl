-module(node_termination).

-export([terminate_node/1]).

terminate_node(Start) ->
  receive
    over ->
      End = erlang:monotonic_time()/10000,
      ConTime = End - Start,
      io:format("Total Time = ~f ms~n", [ConTime]),
      erlang:halt()
  end,
  terminate_node(Start).