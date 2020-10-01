-module(sensor).
-export([]).
-compile(export_all).


generate_sensor(NN_manger_PID,Node)->
  spawn(Node,?MODULE,init,[NN_manger_PID]).

init(NN_manger_PID) ->
  receive
    {NN_manger_PID,{Id,Input,Out_PIds}} ->
      loop(Id,NN_manger_PID,Input,Out_PIds)
  end.

loop(Id,NN_manger_PID,Input,Out_PIds)->
  receive
    {insert_input}->
      [Pid ! {self(),forward,Input} || Pid <- Out_PIds],
      loop(Id,NN_manger_PID,Input,Out_PIds);
    {NN_manger_PID,terminate} ->
      io:format("Sensor:~p is terminating.~n",[Id]),
      ok
  end.
