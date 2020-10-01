%%%-------------------------------------------------------------------
%%% @author chass
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2020 14:52
%%%-------------------------------------------------------------------
-module('NN_Agent').
-author("chass").
-compile(export_all).
%% API
-export([]).
-import(digraph,[new/1,add_vertex/2,vertices/1,get_short_path/3,del_vertex/2,add_edge/3]).

createNN(SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer)->
  G=new(acyclic),
createSensors(G,SensorNum),
  createNeurons(G,NumOfNeuronsEachLayer*NumOfLayers),
  createActuators(G,ActuatorNum),
createSensorsEdges(G,SensorNum,NumOfNeuronsEachLayer,NumOfNeuronsEachLayer),
createNeuronsLayers(G,NumOfLayers,NumOfNeuronsEachLayer),
  createAcoautorLayers(G,NumOfLayers*NumOfNeuronsEachLayer,
    NumOfNeuronsEachLayer*(NumOfLayers-1) ,ActuatorNum,ActuatorNum)
.
createSensors(G,0)->ok;
createSensors(G,N)->
  PID=1,
digraph:add_vertex(G,list_to_atom(lists:flatten(io_lib:format("sensor~p", [N]))),PID),
createSensors(G,N-1).


createNeurons(G, 0) ->ok;
createNeurons(G, N) ->
  PID=1,
  digraph:add_vertex(G,list_to_atom(lists:flatten(io_lib:format("neuron~p", [N]))),PID),
  createNeurons(G, N-1).


createActuators(G, 0) ->ok;
createActuators(G, N) ->
  PID=1,
  digraph:add_vertex(G,list_to_atom(lists:flatten(io_lib:format("actuator~p", [N]))),PID),
  createNeurons(G, N-1).



createSensorsEdges(_,0,_,_)-> ok;
createSensorsEdges(G,S,0,T)-> createSensorsEdges(G,S-1,T,T);
createSensorsEdges(G,S,N,T) ->
  A=list_to_atom(lists:flatten(io_lib:format("neuron~p", [N]))),
  B=list_to_atom(lists:flatten(io_lib:format("sensor~p", [S]))),
  digraph:add_edge(G,B,A,5*(1.0 - rand:uniform())),
createSensorsEdges(G,S,N-1,T).

createNeuronsLayers(_,PrevLayer, _,_,_,Prev3) when PrevLayer =:= Prev3->ok;

createNeuronsLayers(G,PrevLayer, NextLayer,Prev2,Next2,Prev3) when NextLayer =:= Prev2 ->
createNeuronsLayers(G,PrevLayer-1, Next2,Prev2,Next2,Prev3);
createNeuronsLayers(G,PrevLayer, NextLayer,Prev2,Next2,Prev3) ->
  A=list_to_atom(lists:flatten(io_lib:format("neuron~p", [NextLayer]))),
  B=list_to_atom(lists:flatten(io_lib:format("neuron~p", [PrevLayer]))),
  digraph:add_edge(G,B, A,5*(1.0 - rand:uniform())),
  createNeuronsLayers(G,PrevLayer, NextLayer-1,Prev2,Next2,Prev3).


createNeuronsLayers(_, 1, _) ->ok;
createNeuronsLayers(G, NumOfLayers, NumOfNeuronsEachLayer) ->
  A=(NumOfLayers-1)*NumOfNeuronsEachLayer,
  B=NumOfLayers*NumOfNeuronsEachLayer,
  C=A-NumOfNeuronsEachLayer,
  createNeuronsLayers(G,A,B,A,B,C).


createAcoautorLayers(_, IDX1, IDX2, _,_) when IDX1 =:= IDX2->ok;
createAcoautorLayers(G, IDX1, IDX2, 0,ActuatorNum)->
  createAcoautorLayers(G, IDX1, IDX2-1, ActuatorNum,ActuatorNum);
createAcoautorLayers(G, IDX1, IDX2, ActuatorNumIDX,ActuatorNum) ->
  A=list_to_atom(lists:flatten(io_lib:format("actuator~p", [IDX1]))),
  B=list_to_atom(lists:flatten(io_lib:format("neuron~p", [IDX2]))),
  digraph:add_edge(G,B, A,5*(1.0 - rand:uniform())),
  createAcoautorLayers(G,IDX1,IDX2,ActuatorNumIDX-1,ActuatorNum).

