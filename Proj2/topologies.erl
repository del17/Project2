-module(topologies).
-export([selectTopo/3,nth_root/2, raise/2, for/4, imp3D/3, checkRandom/3]).
% -define(MapIndextoNei,dict()).

selectTopo(Topo, N, I)->
    % if 
    %     Topo=="Line"->
    %         if 
    %             I==1->
    %             Neighbors= [I+1];
    %         true->
    %             Neighbors=[I+1, I-1]
    %         end,
    %         if 
    %             I==N ->
    %                 Neighbors=[I-1];
    %             true->
    %                 Neighbors=[I+1, I-1]
    %         end
    % end,
    if 
        Topo=="Full" ->
            Neighbors=lists:seq(1, N)
    end,
    if 
        Topo=="2D"->
            io:fwrite("2D~n~p", [N]),
            Sqrt=round(math:sqrt(N)), %Sqrt=-j
            % Neighbors=[],
            if 
                I rem Sqrt == 0->
                    lists:append(Neighbors,I+1)
            end,

            if 
                I+1 rem Sqrt ->     %!!!!! equal to what?
                    lists:append(Neighbors,I-1)
            end,
            if 
                I-Sqrt< 0->
                    lists:append(Neighbors,I+Sqrt)
            end,
            if 
                (I - (N-Sqrt)) >= 0->
                    lists:append(Neighbors,I-Sqrt)
            end,
            if 
                N>4->
                    if 
                        (I rem Sqrt =/=0) and (I+1 rem Sqrt=/=0)->
                            lists:append(Neighbors,I-1),
                            lists:append(Neighbors,I+1)
                    end,
                    if 
                        (I-Sqrt>0) and (I-(N-Sqrt))<0 ->
                            lists:append(Neighbors,I+Sqrt),
                            lists:append(Neighbors,I-Sqrt)                   
                    end,    
                    if 
                        I==Sqrt->
                            lists:append(Neighbors,I+Sqrt),
                            lists:append(Neighbors,I-Sqrt)   
                    end
            end,
            Neighbors
        end,
        if
            %I is index
            %N is max value
            %D is increment value
            Topo=="Imp3D"->
                Cube=erlang:list_to_integer(erlang:float_to_list(math:ceil(nth_root(27,3)),[{decimals,0}])),
                for(0, N-1, 1, Cube)
        end.

%For Cube Root
%
% -spec(nth_root(number(), integer()) -> number()).

nth_root(X, N) ->
  A = X / 2.0,
  nth_root(X, N, A).
nth_root(X, N, A) ->
%   io:format("Current guess is ~p~n", [A]), %% see the guesses converge
  F = raise(A, N) - X,
  Fprime = N * raise(A, N - 1),
  Next = A - F / Fprime,
  Change = abs(Next - A),
  if
    Change < 1.0e-8 -> Next;
    true -> nth_root(X, N, Next)
  end.


% -spec(raise(number(), integer()) -> number()).

raise(_, 0) -> 1;

raise(X, N) when N > 0 ->
  raise(X, N, 1);

raise(X, N) when N < 0 -> 1 / raise(X, -N).

raise(_, 0, Accumulator) -> Accumulator;

raise(X, N, Accumulator) ->
  raise(X, N-1, X * Accumulator).

%
%For Cube Root

for(I, N, _,_) when I == N -> 1;

for(I, N, D, Cube) when I < N -> 
    MapIndextoNei={},
    % io:fwrite("~w~n", [I]),
    Neighbors=[],
    Lvl = I/(Cube*Cube),
    UpperL=(Lvl+1)*Cube*Cube,
    LowerL=Lvl*Cube*Cube,

    if 
        I-Cube>=LowerL ->
            lists:append(Neighbors,I-Cube)
    end,
    if 
        I+Cube<UpperL ->
            lists:append(Neighbors,I+Cube)
    end,

    if 
        ((((I-1) rem Cube) =/= (Cube-1)) and (I-1>=0)) ->
            lists:append(Neighbors,I-1)
    end,
    if 
        (((I+1) rem Cube) =/= 0) ->
            lists:append(Neighbors,I+1)
    end,
    if 
        (I+(Cube*Cube) < N+1) ->
            lists:append(Neighbors,I+(Cube*Cube))
    end,
    if 
        (I-(Cube*Cube)>=0) ->
            lists:append(Neighbors,I-(Cube*Cube))
    end,

    RandVal = imp3D(I, Neighbors, N),
    lists:append(Neighbors,RandVal),
    dict:append(I,Neighbors,MapIndextoNei),     % should be a global variable
    for(I+D, N, D,cube),
    Neighbors.                                  % not sure what it does

imp3D(I,Neighbors,N)->
    Ran=rand:uniform(N),
    Check=(Ran==1) or (lists:member(Ran, Neighbors)==true),
    if 
        Check->
            imp3D(I,Neighbors,N);
        true->
            Ran
    end.

checkRandom(Topo, N, I)->
    NodeList=selectTopo(Topo, N, I),
    %    NodeList2 = lists:filter(NodeList, fun(el) -> not lists:member(el, Messages) end),
    NodeList1 = lists:filter(NodeList, fun(X) -> ((X=/=1) == true) end),
    NodeList2 = lists:filter(NodeList1, fun(X) -> ((X=/=0) == true) end),
    NodeList3 = lists:filter(NodeList2, fun(X) -> ((X<N) == true) end),
    NodeList4=lists:uniq(NodeList3),
    NodeList4.