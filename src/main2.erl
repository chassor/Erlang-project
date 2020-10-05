-module(main2).
-export([run/0]).
%start_link(Main_PID,SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs)

run()->
  Momo=2,
run2(Main_PID = pid_111 ,SensorNum = 3 ,ActuatorNum = 2 ,NumOfLayers =4,  NumOfNeuronsEachLayer =2  ,AF = sin ,Num_Of_NN_AGENTS = 10,Inputs = [3,4,5] ).
run2(Main_PID  ,SensorNum  ,ActuatorNum  ,NumOfLayers ,  NumOfNeuronsEachLayer   ,AF  ,Num_Of_NN_AGENTS ,Inputs )->
  PID=gen_statem:start_link(population, {Main_PID,SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs}, []),
  PID.
