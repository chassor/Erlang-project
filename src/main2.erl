-module(main2).
-export([loop/1]).
-define(SERVER, ?MODULE).
-record(info, {result}).
%start_link(Main_PID,SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs)

%%run()->
%%  run2(Main_PID = self() ,SensorNum = 3 ,ActuatorNum = 5 ,NumOfLayers =4,  NumOfNeuronsEachLayer =3  ,AF = sin ,Num_Of_NN_AGENTS = 10,Inputs = [3,4,5],PopulationID=pop1 ).
%%run2( Main_PID  ,SensorNum  ,ActuatorNum  ,NumOfLayers ,  NumOfNeuronsEachLayer   ,AF  ,Num_Of_NN_AGENTS ,Inputs,PopulationID )->
%%  PID = population:start_link( self(),SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs,PopulationID),
%%  loop2(PID),
%%  PID.

loop2(PID,Gui_PID,R)->
  gen_statem:cast(PID,{start_insert}),
  receive
    {From,new_gen_hit_me,L} ->
      wx_object:cast(neuron_network_gui,{done,L}),
      loop2(From,Gui_PID,R#info{result = L});

    {stop} ->
      wx_object:cast(neuron_network_gui,{final_result,R#info.result}),
      loop3(PID,Gui_PID)
  end.

loop3(PID,Gui_PID)->
  receive
    {terminate} ->
      gen_statem:stop(PID),
      loop(Gui_PID)
  end.


loop(Gui_PID)->
  R = #info{},
  receive
    {start,Main_PID ,Sensors ,Actuators ,Layers,  Neurons ,Choice ,NN} ->
      Inputs = generate_inputs(Sensors,[]),
      Name = list_to_atom(lists:flatten(io_lib:format("pop~p", [generate_id()]))),
      PID = population:start_link( self(),Sensors,Actuators,Layers,Neurons,Choice,NN,Inputs,Name),
      loop2(PID,Gui_PID,R)
  end.

generate_id() ->
  {MegaSeconds,Seconds,MicroSeconds} = now(),
  1/(MegaSeconds + Seconds + MicroSeconds).

generate_inputs(0,L)-> L;
generate_inputs(N,L)->
  X = random_input(),
  L2 = L ++ [X] ,
  generate_inputs(N-1,L2).


random_input()->
X=5*(1.0 - rand:uniform()),
case rand:uniform(2) of
1-> Y=X;
2-> Y=-1*X
end,
Y.

