-module(main).
-export([run/0]).

run()->
Y=generate_id(),
  X=nn_agent:start_link(SensorNum=2,ActuatorNum=2,NumOfLayers=2,NumOfNeuronsEachLayer=3,ManagerPid=self(),Id=5),
  G=gen_statem:call(X,{self(),get_graph}),
gen_statem:cast(X,{self(),insert_input,[1,2]}),
  loop(X).
loop(X)->

receive
{_Fromm,result,ResultList}->
Result=ResultList,
X2=2,
  L=ResultList,
    gen_statem:stop(X),
X4=is_process_alive(X),
  L2= [is_process_alive(Pid)||Pid<-L],
  L=[random:uniform(5),random:uniform(5)],
  %gen_statem:cast(X,{self(),insert_input,L}),

  loop2(X)
end


.


loop2(X) ->
  loop2(X).

generate_id() ->
  {MegaSeconds,Seconds,MicroSeconds} = now(),
  (MegaSeconds*1000000 + Seconds + MicroSeconds/1000000).