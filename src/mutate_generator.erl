%%%-------------------------------------------------------------------
%%% @author chass
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Oct 2020 14:32
%%%-------------------------------------------------------------------
-module('mutate_generator').
-author("chass").

%% API
-export([mutateAgent/1, getName/1, getVerticesList/1, getEdgesList/1]).



mutateAgent(G)->
  VerL=digraph:vertices(G),
  NumOfNeurons=length(VerL),
  NUM=round(math:pow(NumOfNeurons,1/2)),
 A=rand:uniform(NUM),
  mutate(G,A).


mutate(_,0)->ok;
mutate(G,A) ->
%X=6,
  X=rand:uniform(7),
case X of
  1->setBias(G,rand:uniform());
  2->removeBias(G);
  3->addEdge(G,nn_agent:randomWeight(),50);
  4->addNeuron(G,nn_agent:randomWeight(),nn_agent:randomWeight(),50);
  5->removeEdge(G);
  6->changeWeight(G,nn_agent:randomWeight());
  7->changeAF(G)


end,
mutate(G,A-1)
.



setBias(G,NewBias) ->

Edges_old=getEdgesList(G),
  VertexList2= getVerticesList(G),
 VertexList3=filterSensors(VertexList2,[]),
  {V,{A,B1,C1,D,E,AF}}=lists:nth(rand:uniform(length(VertexList3)),VertexList3),
  digraph:add_vertex(G,V,{A,B1,C1,D,NewBias,AF}),
  Edges_new=getEdgesList(G),
  VertexList2_new= getVerticesList(G),
 Rep= gen_statem:call(C1,{self(),newBias,NewBias}),

  Rep
.




addEdge(G,Weight,0)->ok;
addEdge(G,Weight,Counter) ->
  VertexList= getVerticesList(G),
  Edges_old=getEdgesList(G),
  {V,{A,B1,C1,D,E,AF}}=lists:nth(rand:uniform(length(VertexList)),VertexList),
  NEW_L=lists:delete({V,{A,B1,C1,D,E,AF}},VertexList),
  {V1,{A1,B2,C3,D4,E5,_}}=lists:nth(rand:uniform(length(NEW_L)),NEW_L),
  EdgeAlreadyExist= lists:member(V1,digraph:out_neighbours(G,V)) or (A =:=sensor andalso A1 =:= sensor)
  or (A =:=actuator),

  if
    EdgeAlreadyExist=:=true->addEdge(G,Weight,Counter-1) ;
    true ->
             Result=digraph:add_edge(G,V,V1,Weight),
      if
        is_tuple(Result) -> addEdge(G,Weight,Counter-1) ;
        true -> VertexList_new= getVerticesList(G),
                Edges_old_new=getEdgesList(G),
          Rep1= gen_statem:call(C1,{self(),new_neighbour_out,C3}),
          Rep2= gen_statem:call(C3,{self(),new_neighbour_in,{C1,Weight}}),
          1
      end

  end.


addNeuron(G,Weight1,Weight2,0)->ok;
addNeuron(G,Weight1,Weight2,Counter) ->
  VertexList= getVerticesList(G),
  Edges_old=getEdgesList(G),
  {V,{A,B1,C1,D,E,AF}}=lists:nth(rand:uniform(length(VertexList)),VertexList),
  NEW_L=lists:delete({V,{A,B1,C1,D,E,AF}},VertexList),
  {V1,{A1,B2,C3,D4,E5,E6}}=lists:nth(rand:uniform(length(NEW_L)),NEW_L),
  EdgeAlreadyExist= lists:member(V1,digraph:out_neighbours(G,V)) or (A =:=sensor andalso A1 =:= sensor)
    or (A =:=actuator ) ,

  if
    EdgeAlreadyExist=:=true->addNeuron(G,Weight1,Weight2,Counter-1) ;
    true ->
      Result=digraph:add_edge(G,V,V1,Weight1),
      if
        is_tuple(Result) -> addNeuron(G,Weight1,Weight2,Counter-1) ;
        true ->
          digraph:del_edge(G,Result),
          ID2=list_to_atom(lists:flatten(io_lib:format("neuron~p", [nn_agent:generate_id()]))),
          PID=neuron2:start_link(self(),ID2,neuron),
          Bias=rand:uniform(),
          AF1=newAF(),
          digraph:add_vertex(G,ID2,{neuron,PID,ID2,ok,Bias,AF1}),
          M_PID=getName(self()),
          Result2=digraph:add_edge(G,V,ID2,Weight1),
          Result3=digraph:add_edge(G,ID2,V1,Weight2),
          VertexList_new= getVerticesList(G),
          Edges_old_new=getEdgesList(G),
          X=gen_statem:call(ID2,{M_PID,{ID2,AF1,[{C1,Weight1}],[C3],Bias}}),
          Rep1= gen_statem:call(C1,{self(),new_neighbour_out,ID2}),
          Rep2= gen_statem:call(C3,{self(),new_neighbour_in,{ID2,Weight2}}),
          1
      end

  end.


getEdgesList(G)->
  B=digraph:edges(G),
  [digraph:edge(G,E) || E <- B].


getVerticesList(G)->
  VerL=digraph:vertices(G),
  [digraph:vertex(G,V) || V <- VerL].





filterSensors([],L)->L;
filterSensors([{A,{B,C,D,E,F,G}}|T],L) ->
 if
   B=:= sensor->filterSensors(T,L);
   true->filterSensors(T,L++[{A,{B,C,D,E,F,G}}])
 end .

removeBias(G) ->
  setBias(G,0).

getName(PID)->
   [{_,Name}|_T]=process_info(PID),
   Name.

removeEdge(G) ->
  changeWeight(G,0).

changeWeight(G,N) ->
  Edges_old=getEdgesList(G),
  {A,B,C,_D}=lists:nth(rand:uniform(length(Edges_old)),Edges_old),
  Result=digraph:add_edge(G,A,B,C,N),
  {_V1,{_,_Pid,ID1,_ID,_Bias,_AF} }= digraph:vertex(G,B),
    {_V2,{_,_Pid1,ID2,_ID1,_Bias1,_AF2}}= digraph:vertex(G,C),
  Edges_new=getEdgesList(G),
  VertexList2_new= getVerticesList(G),
  Rep= gen_statem:call(ID2,{self(),newWeight, {ID1,N}}),


1.

changeAF(G) ->

  NEWAF=newAF(),
  Edges_old=getEdgesList(G),
  VertexList2= getVerticesList(G),
  VertexList3=filterSensors(VertexList2,[]),
  {V,{A,B1,C1,D,Bias,_AF}}=lists:nth(rand:uniform(length(VertexList3)),VertexList3),
  digraph:add_vertex(G,V,{A,B1,C1,D,Bias,NEWAF}),
  Edges_new=getEdgesList(G),
  VertexList2_new= getVerticesList(G),
  Rep= gen_statem:call(C1,{self(),newAF,NEWAF}),

  Rep.

newAF() ->
 case rand:uniform(5) of
   1->relu;
   2->sin;
   3->cos;
   4->bin;
   5->tanh
 end.