-module(neuron).
-export([generate_neuron/2,loop/6,init/1]).
-compile(export_all).

generate_neuron(NN_manger_PID,Node)->
  spawn(Node,?MODULE,init,[NN_manger_PID]).

init(NN_manger_PID) ->
  receive
    {NN_manger_PID,{Id,AF,Input_PIdPs,Output_PIds}} ->
      loop(Id,NN_manger_PID,AF,{Input_PIdPs,Input_PIdPs},Output_PIds,0)
  end.

loop(Id,NN_manger_PID,AF,{[{Input_PId,Weight}|Input_PIdPs],MInput_PIdPs},Output_PIds,Acc)->
  receive
  % update the inputs and outputs lists when created new net.
    {new_net,Input_PIdPs,Output_PIds,NN_manger_PID} ->
      loop(Id,NN_manger_PID,AF,{Input_PIdPs,Input_PIdPs},Output_PIds,0);

    {Input_PId,forward,Input}->
      New_ACC= Acc+(Input*Weight),
      loop(Id,NN_manger_PID,AF,{Input_PIdPs,MInput_PIdPs},Output_PIds,New_ACC);

    {NN_manger_PID,terminate}->
      io:format("Neuron:~p has termianted.~n",[self()]),
      ok
  end;

% case of bias in last element in inputs vector.
loop(Id,ExoSelf_PId,AF,{[Bias],MInput_PIdPs},Output_PIds,Acc)->
  Output = functions:AF(Acc+Bias),
  [Output_PId ! {self(),forward,[Output]} || Output_PId <- Output_PIds],
  loop(Id,ExoSelf_PId,AF,{MInput_PIdPs,MInput_PIdPs},Output_PIds,0);

% case of no bias in inputs vector.
loop(Id,ExoSelf_PId,AF,{[],MInput_PIdPs},Output_PIds,Acc)->
  Output = functions:AF(Acc),
  [Output_PId ! {self(),forward,[Output]} || Output_PId <- Output_PIds],
  loop(Id,ExoSelf_PId,AF,{MInput_PIdPs,MInput_PIdPs},Output_PIds,0).