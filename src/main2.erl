-module(main2).
-export([run/1,loop/1,sendtome/1]).
-define(SERVER, ?MODULE).
-record(info, {result}).
%start_link(Main_PID,SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs)

run(List_of_nodes)->
 Main_PID = self(),SensorNum = 3,ActuatorNum = 5,NumOfLayers =4,  NumOfNeuronsEachLayer =3  ,AF = sin ,Num_Of_NN_AGENTS = 10,Inputs = [3,4,5],
  Map = startChat(List_of_nodes,maps:new(),Main_PID ,SensorNum ,ActuatorNum,NumOfLayers, NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs),
  l(Map).

l(Map)->
  io:format("im in l loop ~p ~n",[Map]),
  [ rpc:cast(X, population, cast_myself, [{start_insert}]) || X <-maps:values(Map) ],
  %[ gen_statem:cast({population,X},{start_insert}) || X <-maps:values(Map) ],
  N = maps:size(Map),
  RL= recive_loop(N,[]),
      if
          RL > 0.0000001 ->
          io:format("Fitness: ~p ~n", [RL]),
          l(Map);
          true ->
         io:format("Bye Bye ~n")

      end.


recive_loop(0,L) ->
  L2 = lists:sort(L),
  hd(L2),
  L2;
recive_loop(N,L) ->
  io:format("im in recive loop ~n"),
  receive
    {From,new_gen_hit_me,Tuple} ->
      {F,_,_} = Tuple,
      L2 = L ++ [F],
      recive_loop(N-1,L2)
  end.

sendtome(M)->
  io:format("send to myself ~n"),
  self() ! M.


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


%%----------------------------------------------------------------------------------------------------
%%                                        internal Functions
%%----------------------------------------------------------------------------------------------------

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

startChat([],Map,_,_,_,_,_,_,_,_) ->
  register(main_Pid,erlang:self()),%% Save the Pid of the local host process
  Map;
startChat([Address|T],Map,Main_PID ,SensorNum ,ActuatorNum,NumOfLayers, NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs ) ->
  N = maps:size(Map)+1,
  PopulationID = {list_to_atom(lists:flatten(io_lib:format("node~p", [N]))),node()},
  M2 = maps:put(PopulationID,Address,Map),
  case whereis(main_Pid) of
    undefined ->
      rpc:cast(Address, population, start_link, [Main_PID,SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs,PopulationID]),
      startChat(T,M2,Main_PID ,SensorNum ,ActuatorNum,NumOfLayers, NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs);
    _ -> io:format("You already started a chat~n")
  end.


