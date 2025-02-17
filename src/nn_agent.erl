%%%-------------------------------------------------------------------
%%% @author hananel assor, dor avni
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2020 15:55
%%%-------------------------------------------------------------------
-module(nn_agent).
-author("chass").

-behaviour(gen_statem).
-import(digraph,[new/1,add_vertex/2,vertices/1,get_short_path/3,del_vertex/2,add_edge/3]).
%% API
-export([start_link/7, idle/3, wait_for_result/3, generate_id/0, createNN/2, randomWeight/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

-define(SERVER, ?MODULE).

-record(nn_agent_state, {sensor_list,actuator_list,actuator_list2,manger_pid,id,graph,input_list,results,result_list}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link(SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,ManagerPid,Id,G2) ->
  {_ok,PID}=gen_statem:start_link({local,Id}, ?MODULE, {SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,ManagerPid,Id,G2}, []),
  PID.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init({SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,ManagerPid,Id,G2}) ->
  process_flag(trap_exit, true),
  if
    G2 =:=new ->{G,SensorList,ActList}=createNN(SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,Id) ;
    true -> {G,SensorList,ActList}=createNN(G2,Id)
  end
  ,
  {ok, idle, #nn_agent_state{manger_pid = ManagerPid,id = Id,graph = G,results = maps:new()
    ,sensor_list = SensorList,actuator_list = maps:from_list(ActList),actuator_list2 = maps:from_list(ActList),result_list = no_result }}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  state_functions.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
state_name(_EventType, _EventContent, State = #nn_agent_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.



%% idle state waiting for insert result
idle(cast, {_From,insert_input,InputList}, State = #nn_agent_state{result_list = ResList}) ->
  if
    is_atom(ResList) =/=true-> gen_statem:cast(State#nn_agent_state.manger_pid,{State#nn_agent_state.id,result,ResList}),
    {keep_state, State};
    true-> sendInput(InputList,State#nn_agent_state.sensor_list),
NextStateName = wait_for_result,
{next_state, NextStateName, State#nn_agent_state{input_list = InputList}}
  end ;

idle(EventType, EventContent, Data) ->
  A=handle_common(EventType, EventContent, Data,idle),
A.


%% wait to result from the actuators
wait_for_result(cast, {From,result,Result},State = #nn_agent_state{}) ->
  Act_PIds=State#nn_agent_state.actuator_list2,
  {_,ID}=maps:get(From,Act_PIds),
  ResultsMap2=State#nn_agent_state.results,
  ResultsMap=maps:put(ID,Result,ResultsMap2),
  Map2=maps:remove(From,Act_PIds),
  case maps:size(Map2) =:= 0 of
    true ->
         PID_manager=State#nn_agent_state.manger_pid,
      ResultList=[B||{_A,B} <-maps:to_list(ResultsMap)],
     gen_statem:cast(PID_manager,{State#nn_agent_state.id,result,ResultList}),
      {next_state,idle, State#nn_agent_state{actuator_list2 = State#nn_agent_state.actuator_list,results = maps:new(),result_list = ResultList}};
    false->{keep_state,State#nn_agent_state{actuator_list2 = Map2,results = ResultsMap}}
end;


wait_for_result(EventType, EventContent, Data) ->
  handle_common(EventType, EventContent, Data,wait_for_result).




%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #nn_agent_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.


%%func to  flush mailbox
all_messages(Messages) ->
  receive
    AnyMessage ->
      all_messages( [AnyMessage|Messages])
  after 0 ->
    lists:reverse(Messages)
  end.


%%function that send the graph of the nn
handle_common({call,From}, {_XFrom,get_graph},State = #nn_agent_state{},_State) ->
  {keep_state, State,[{reply,From,State#nn_agent_state.graph}]};


%%function that reset of the nn
handle_common({call,From}, {_XFrom,reset},State = #nn_agent_state{},_State) ->
  VertexList= getVerticesList(State#nn_agent_state.graph),
  [gen_statem:cast(Pid,{State#nn_agent_state.id,reset})||{_V,{_,_,Pid,_,_,_}}<-VertexList],
  {next_state,idle, State#nn_agent_state{actuator_list2 = State#nn_agent_state.actuator_list,results = maps:new()}
    , State#nn_agent_state{},[{reply,From,ok}]};


%%command  the nn to mutate
handle_common({call,From}, {_XFrom,mutate},State = #nn_agent_state{},_State) ->
  mutate_generator:mutateAgent(State#nn_agent_state.graph),
  {keep_state, State,[{reply,From,State#nn_agent_state.graph}]};


%% deal with neuron crash for some reason
handle_common(info, {'EXIT',PID,_},State = #nn_agent_state{},CuRR_State) ->
  NN_id=State#nn_agent_state.id,
  G=State#nn_agent_state.graph,
  VertexList= getVerticesList(State),
  timer:sleep(50),
  resetProcess(VertexList,NN_id),
  resetProcess(VertexList,NN_id),

  {V,Kind,_OldPid,PID1,E,Bias,AF}= getinfoByID(PID,VertexList),
  NewPID=neuron2:start_link(self(),PID1,Kind),
 digraph:add_vertex(G,V,{Kind,NewPID,PID1,E,Bias,AF}),
  sendInfo(G,NN_id,[{V,{Kind,NewPID,PID1,E,Bias,AF}}]),
  if
      CuRR_State=:=wait_for_result->
        deleteResults([]),
        sendInput(State#nn_agent_state.input_list,State#nn_agent_state.sensor_list),
      {keep_state, State#nn_agent_state{actuator_list2 = State#nn_agent_state.actuator_list,results = maps:new()}};
    true->{keep_state, State}
  end ;



handle_common(info,_,State = #nn_agent_state{},_State) ->
  {keep_state, State};

handle_common(_,_,State = #nn_agent_state{},_State) ->
  {keep_state, State}.



%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, State = #nn_agent_state{}) ->
  VertexList= getVerticesList(State),
  stopProcess(VertexList),
%%L= [{is_process_alive(Pid1),Pid}||{_V,{_,Pid1,Pid,_,_,_}}<-VertexList],
%% L2= [gen_statem:stop(Pid)||{true,Pid}<-L],
 all_messages([]),
ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #nn_agent_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================


%%func to create new NN
createNN(SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,PID1)->
  G=new([acyclic]),
  SensorsL=createSensors(G,SensorNum,[]),
  createNeurons(G,NumOfNeuronsEachLayer*NumOfLayers),
  ActL=createActuators(G,ActuatorNum,[]),
  createSensorsEdges(G,SensorNum,NumOfNeuronsEachLayer,NumOfNeuronsEachLayer),
  createNeuronsLayers(G,NumOfLayers,NumOfNeuronsEachLayer),
  createActuatorLayers(G,NumOfLayers*NumOfNeuronsEachLayer,
    NumOfNeuronsEachLayer*(NumOfLayers-1) ,ActuatorNum,ActuatorNum),
  A=digraph:vertices(G),
  VertexList= [digraph:vertex(G,V) || V <- A],
  sendInfo(G,PID1,VertexList),
  X= {G,SensorsL,ActL},
  X.


%%func to create new NN duplicate another NN
createNN(G,ID)->
  G_new=digraph:new([acyclic]),
  A=digraph:vertices(G),
  B=digraph:edges(G),
  C= [digraph:edge(G,E) || E <- B],
  VertexList= [digraph:vertex(G,V) || V <- A],
  VertexList2= [add_ver_to_new_graph(G_new,K,V,ID10,Bias,AF) || {V,{K,_Pid,_ID2,ID10,Bias,AF}} <- VertexList],
  Map=maps:from_list(VertexList2),
  [digraph:add_edge(G_new,maps:get(A30,Map),maps:get(B30,Map),W) || {_,A30,B30,W} <- C],
  A1=digraph:vertices(G_new),
  VertexList_new= [digraph:vertex(G_new,V) || V <- A1],
  sendInfo(G_new,ID,VertexList_new),
  SensL1=[{ID20,Pid,ID2} || {_V,{sensor,Pid,ID2,ID20,_,_}} <- VertexList_new],
  SensL=lists:sort(SensL1),
  ActL=[{ID2,{Pid,ID30}} ||  {_V,{actuator,Pid,ID2,ID30,_,_}} <- VertexList_new],
  X= {G_new,SensL,ActL},
  X.


%%add neuron to the graph
add_ver_to_new_graph(G_new,K,V,ID2,Bias,AF)->
  ID=genarateIdfromAtom(K),
  PID=neuron2:start_link(self(),ID,K),
  digraph:add_vertex(G_new,ID,{K,PID,ID,ID2,Bias,AF}),
  {V,ID}.



%%genarate unic ID
genarateIdfromAtom(sensor)->list_to_atom(lists:flatten(io_lib:format("sensor~p", [generate_id()])));
genarateIdfromAtom(neuron)->list_to_atom(lists:flatten(io_lib:format("neuron~p", [generate_id()])));
genarateIdfromAtom(actuator)->list_to_atom(lists:flatten(io_lib:format("actuator~p", [generate_id()]))).


%%func to get vertices from graph
getVerticesList(State)->
G=State#nn_agent_state.graph,
A=digraph:vertices(G),
[digraph:vertex(G,V) || V <- A].



%%func to send input to graph neurons
sendInput(_InVector,[])->ok;
sendInput([],[{_ID2,_ID,PID}|T2])->
  gen_statem:cast(PID,{insert_input,self(),0}),
  sendInput([],T2);
sendInput([H|T],[{_ID2,_ID,PID}|T2])->
  gen_statem:cast(PID,{insert_input,self(),H}),
  sendInput(T,T2).



%%func to create sensor
createSensors(_G,0,L)->L;
createSensors(G,N,L)->
  ID=list_to_atom(lists:flatten(io_lib:format("sensor~p", [N]))),
  ID2=list_to_atom(lists:flatten(io_lib:format("sensor~p", [generate_id()]))),
  PID=neuron2:start_link(self(),ID2,sensor),
  L2=[{ID,PID,ID2}]++L,
  digraph:add_vertex(G,ID,{sensor,PID,ID2,ID,0,relu}),
  createSensors(G,N-1,L2).

%%func to create neuron
createNeurons(_G, 0) ->ok;
createNeurons(G,N) ->
  ID=list_to_atom(lists:flatten(io_lib:format("neuron~p", [N]))),
  ID2=list_to_atom(lists:flatten(io_lib:format("neuron~p", [generate_id()]))),
  PID=neuron2:start_link(self(),ID2,neuron),
  Bias=rand:uniform(),
  digraph:add_vertex(G,ID,{neuron,PID,ID2,ok,Bias,relu}),
  createNeurons(G, N-1).



%%func to create actuators
createActuators(_, 0,L) -> L;
createActuators(G, N,L) ->
  ID=list_to_atom(lists:flatten(io_lib:format("actuator~p", [N]))),
  ID2=list_to_atom(lists:flatten(io_lib:format("actuator~p", [generate_id()]))),
  PID=neuron2:start_link(self(),ID2,actuator),
  L2=[{ID2,{PID,ID}}]++L,
  Bias=rand:uniform(),
  digraph:add_vertex(G,ID,{actuator,PID,ID2,ID,Bias,relu}),
  createActuators(G,N-1,L2).


%%func to create sensor edges
createSensorsEdges(_,0,_,_)-> ok;
createSensorsEdges(G,S,0,T)-> createSensorsEdges(G,S-1,T,T);
createSensorsEdges(G,S,N,T) ->
  A=list_to_atom(lists:flatten(io_lib:format("neuron~p", [N]))),
  B=list_to_atom(lists:flatten(io_lib:format("sensor~p", [S]))),
  digraph:add_edge(G,B,A,randomWeight()),
  createSensorsEdges(G,S,N-1,T).

%%func to create neurons edges
createNeuronsLayers(_,PrevLayer, _,_,_,Prev3) when PrevLayer =:= Prev3->ok;
createNeuronsLayers(G,PrevLayer, NextLayer,Prev2,Next2,Prev3) when NextLayer =:= Prev2 ->
  createNeuronsLayers(G,PrevLayer-1, Next2,Prev2,Next2,Prev3);
createNeuronsLayers(G,PrevLayer, NextLayer,Prev2,Next2,Prev3) ->
  A=list_to_atom(lists:flatten(io_lib:format("neuron~p", [NextLayer]))),
  B=list_to_atom(lists:flatten(io_lib:format("neuron~p", [PrevLayer]))),
  digraph:add_edge(G,B, A,randomWeight()),
  createNeuronsLayers(G,PrevLayer, NextLayer-1,Prev2,Next2,Prev3).


createNeuronsLayers(_, 1, _) ->ok;
createNeuronsLayers(G, NumOfLayers, NumOfNeuronsEachLayer) ->
  A=(NumOfLayers-1)*NumOfNeuronsEachLayer,
  B=NumOfLayers*NumOfNeuronsEachLayer,
  C=A-NumOfNeuronsEachLayer,
  createNeuronsLayers(G,A,B,A,B,C),
  createNeuronsLayers(G, NumOfLayers-1, NumOfNeuronsEachLayer).


createActuatorLayers(_, IDX1, IDX2, _,_) when IDX1 =:= IDX2->ok;
createActuatorLayers(G, IDX1, IDX2, 0,ActuatorNum)->
  createActuatorLayers(G, IDX1-1, IDX2, ActuatorNum,ActuatorNum);
createActuatorLayers(G, IDX1, IDX2, ActuatorNumIDX,ActuatorNum) ->
  A=list_to_atom(lists:flatten(io_lib:format("actuator~p", [ActuatorNumIDX]))),
  B=list_to_atom(lists:flatten(io_lib:format("neuron~p", [IDX1]))),
  digraph:add_edge(G,B, A,randomWeight()),
  createActuatorLayers(G,IDX1,IDX2,ActuatorNumIDX-1,ActuatorNum).


%%func to send info to graph neurons for initialize
sendInfo(_,_MID, []) ->ok;
sendInfo(G,MID, [{V,{_,_Pid,ID2,_ID,Bias,AF}}|T]) ->
  Neighbours_in =digraph:in_edges(G,V),
  C= [digraph:edge(G,E) || E <- Neighbours_in],
  Z=[{getPID(G,B),D} || {_,B,_,D} <- C],
  Neighbours_Out=digraph:out_edges(G,V),
  F= [digraph:edge(G,E) || E <- Neighbours_Out],
  Z2=[getPID(G,B2) || {_,_,B2,_} <- F],
  gen_statem:call(ID2,{MID,{ID2,AF,Z,Z2,Bias}}),
  sendInfo(G,MID,T).


getPID(G,V)->
  {_,{_,_PID,ID2,_ID,_,_}}=digraph:vertex(G,V),
  ID2.


%%%===================================================================


%%generate unic Weight
generate_id() ->
  {MegaSeconds,Seconds,MicroSeconds} = erlang:timestamp(),%%replace erlang:now()
  1/(MegaSeconds*1000000 + Seconds + MicroSeconds/1000000).



%%generate random weight between 5 to -5
randomWeight()->
  X=5*(1.0 - rand:uniform()),

  case rand:uniform(2) of
    1-> Y=X;
    2-> Y=-1*X
    end,
  Y.



%%%get info from graph by pid
getinfoByID(_PID, []) ->ok;
getinfoByID(PID, [{V,{Kind,OldPid,PID1,E,Bias,AF}}|T]) ->
  if
   PID =:= OldPid  -> {V,Kind,OldPid,PID1,E,Bias,AF};
    true -> getinfoByID(PID, T)
  end
.


%%%reset neuron
resetProcess([],_)->ok;
resetProcess([{_V,{_,Pid1,Pid,_,_,_}}|T],NN_id)->
  X=is_process_alive(Pid1),
  if
    X=:=true->   gen_statem:call(Pid,{NN_id,reset}),
      resetProcess(T,NN_id);
    true->
      resetProcess(T,NN_id)
  end.


%%% stop neurons
stopProcess([])->ok;
stopProcess([{_V,{_,Pid1,_Pid,_,_,_}}|T])->
  try
    gen_statem:stop(Pid1) of
    _Result->stopProcess(T)
  catch
    _Reason:_Reason1->stopProcess(T)
  end.


%%empty results from mailbox when reset
deleteResults(L)->
  receive
    {_X,{From,result,Result}}->deleteResults(L++[{From,result,Result}])

  after 0->L
  end.