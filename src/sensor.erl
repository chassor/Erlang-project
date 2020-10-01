-module(sensor).
-export([]).
-compile(export_all).


generate_sensor(NN_manger_PID,Node)->
  spawn(Node,?MODULE,init,[NN_manger_PID]).

init(NN_manger_PID) ->
  receive
    {NN_manger_PID,{Id,AF,Input_PIdPs,Output_PIds}} ->
      loop(Id,NN_manger_PID,Input_PIdPs,Output_PIds,0)
  end.

loop(Id,NN_manger_PID,Input,Out_PIds,Weight)->
  receive
    {insert_input,W}->
      [Pid ! {self(),forward,W} || Pid <- Out_PIds],
      loop(Id,NN_manger_PID,Input,Out_PIds,W);
    {NN_manger_PID,terminate} ->
      io:format("Sensor:~p is terminating.~n",[Id]),
      ok
  end.
