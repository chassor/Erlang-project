-module(actuator).
-export([]).
-compile(export_all).



generate_actuator(ExoSelf_PId,Node)->
  spawn(Node,?MODULE,init,[ExoSelf_PId]).

init(NN_manger_PID) ->
  receive
    {NN_manger_PID,{Id,AF,Input_PIdPs,_}} ->
      loop(Id,NN_manger_PID,AF,{Input_PIdPs,Input_PIdPs},0)
  end.



loop(Id,NN_manger_PID,AF,{[{Input_PId,Weight}|Input_PIdPs],MInput_PIdPs},Acc)->
  receive
  % update the inputs and outputs lists when created new net.
    {new_net,Input_PIdPs,NN_manger_PID} ->
      loop(Id,NN_manger_PID,AF,{Input_PIdPs,Input_PIdPs},0);

    {Input_PId,forward,Input}->
      New_ACC= Acc+(Input*Weight),
      loop(Id,NN_manger_PID,AF,{Input_PIdPs,MInput_PIdPs},New_ACC);

    {NN_manger_PID,terminate}->
      io:format("Neuron:~p has termianted.~n",[self()]),
      ok
  end.

% case of bias in last element in inputs vector.
loop(Id,ExoSelf_PId,AF,{[Bias],MInput_PIdPs},Output_PIds,Acc)->
  Output = functions:AF(Acc+Bias),
 % do someting with result,
  loop(Id,ExoSelf_PId,AF,{MInput_PIdPs,MInput_PIdPs},Output_PIds,0);

% case of no bias in inputs vector.
loop(Id,ExoSelf_PId,AF,{[],MInput_PIdPs},Output_PIds,Acc)->
  Output = functions:AF(Acc),
%  do someting with result,
  loop(Id,ExoSelf_PId,AF,{MInput_PIdPs,MInput_PIdPs},Output_PIds,0).
