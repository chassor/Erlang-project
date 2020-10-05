-module(nn_agent).
-behaviour(gen_statem).
-import(digraph,[new/1,add_vertex/2,vertices/1,get_short_path/3,del_vertex/2,add_edge/3]).
%% API
-export([start_link/6, idle/3, wait_for_result/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

-define(SERVER, ?MODULE).

-record(nn_agent_state, {sensor_list,actuator_list,actuator_list2,manger_pid,id,graph,results}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link(SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,ManagerPid,Id) ->
  {_ok,PID}=gen_statem:start_link({local, ?SERVER}, ?MODULE, {SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,ManagerPid,Id}, []),
  PID.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init({SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,ManagerPid,Id}) ->
  {G,SensorList,ActList}=createNN(SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,ManagerPid),
  {ok, idle, #nn_agent_state{manger_pid = ManagerPid,id = Id,graph = G,results = maps:new()
    ,sensor_list = SensorList,actuator_list = maps:from_list(inverse(ActList)),actuator_list2 = maps:from_list(inverse(ActList)) }}.

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


idle(cast, {From,insert_input,InputList}, State = #nn_agent_state{}) ->
  sendInput(InputList,State#nn_agent_state.sensor_list),
  NextStateName = wait_for_result,
  {next_state, NextStateName, State}.


wait_for_result(cast, {From,result,Result},State = #nn_agent_state{}) ->
  Act_PIds=State#nn_agent_state.actuator_list2,
  ResultsMap2=State#nn_agent_state.results,
  ResultsMap=maps:put(maps:get(From,Act_PIds),Result,ResultsMap2),
  Map2=maps:remove(From,Act_PIds),
  case maps:size(Map2) =:= 0 of
    true ->

      PID_manager=State#nn_agent_state.manger_pid,
      ResultList=[B||{_A,B} <-maps:to_list(ResultsMap) ],

      PID_manager !{self(),result,ResultList},
      %     gen_statem:cast(PIDmanger,{self(),result,Result});

      {next_state,idle, State#nn_agent_state{actuator_list2 = State#nn_agent_state.actuator_list,results = maps:new()}};
    false->{keep_state,State#nn_agent_state{actuator_list2 = Map2,results = ResultsMap}}
  end.




%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #nn_agent_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, State = #nn_agent_state{}) ->
  G=State#nn_agent_state.graph,
  A=digraph:vertices(G),
  VertexList= [digraph:vertex(G,V) || V <- A],
  [gen_statem:stop(Pid)||{_V,{_,Pid}}<-VertexList],

  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #nn_agent_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
createNN(SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,PID1)->
  G=new([acyclic]),
  SensorsL=createSensors(G,SensorNum,[]),
  createNeurons(G,NumOfNeuronsEachLayer*NumOfLayers),
  ActL=createActuators(G,ActuatorNum,[]),

  createSensorsEdges(G,SensorNum,NumOfNeuronsEachLayer,NumOfNeuronsEachLayer),
  createNeuronsLayers(G,NumOfLayers,NumOfNeuronsEachLayer),
  createAcoautorLayers(G,NumOfLayers*NumOfNeuronsEachLayer,
    NumOfNeuronsEachLayer*(NumOfLayers-1) ,ActuatorNum,ActuatorNum),
  A=digraph:vertices(G),
  B=digraph:edges(G),
  C= [digraph:edge(G,E) || E <- B],
  VertexList= [digraph:vertex(G,V) || V <- A],
%  O= digraph:add_edge(G,neuron10, neuron2,rand:uniform(5)),
  sendInfo(G,VertexList),

  X= {G,SensorsL,ActL},
  X.




sendInput(_InVector,[])->ok;
sendInput([],[{_ID,PID}|T2])->
  gen_statem:cast(PID,{insert_input,self(),0}),
  sendInput([],T2);

sendInput([H|T],[{_ID,PID}|T2])->
  gen_statem:cast(PID,{insert_input,self(),H}),
  sendInput(T,T2).




createSensors(_G,0,L)->L;
createSensors(G,N,L)->
  ID=list_to_atom(lists:flatten(io_lib:format("sensor~p", [N]))),
  PID=neuron2:start_link(self(),ID,sensor),
  %PID=sensor:generate_sensor(self(),node()),
  % NAME=list_to_atom(lists:flatten(io_lib:format("sensor~p", [N]))),
  L2=[{ID,PID}]++L,
  digraph:add_vertex(G,ID,{sensor,PID}),
  createSensors(G,N-1,L2).


createNeurons(_G, 0) ->ok;
createNeurons(G,N) ->
  ID=list_to_atom(lists:flatten(io_lib:format("neuron~p", [N]))),
  PID=neuron2:start_link(self(),ID,neuron),
  %PID=neuron:generate_neuron(self(),node()),
  digraph:add_vertex(G,ID,{neuron,PID}),
  createNeurons(G, N-1).


createActuators(_, 0,L) -> L;
createActuators(G, N,L) ->
  ID=list_to_atom(lists:flatten(io_lib:format("actuator~p", [N]))),
  PID=neuron2:start_link(self(),ID,actuator),
  % PID=actuator:generate_actuator(self(),node()),
%  NAME=list_to_atom(lists:flatten(io_lib:format("actuator~p", [N]))),
  L2=[{ID,PID}]++L,
  digraph:add_vertex(G,ID,{actuator,PID}),
  createActuators(G,N-1,L2).



createSensorsEdges(_,0,_,_)-> ok;
createSensorsEdges(G,S,0,T)-> createSensorsEdges(G,S-1,T,T);
createSensorsEdges(G,S,N,T) ->
  A=list_to_atom(lists:flatten(io_lib:format("neuron~p", [N]))),
  B=list_to_atom(lists:flatten(io_lib:format("sensor~p", [S]))),
  %digraph:add_edge(G,B,A,5*(1.0 - rand:uniform())),
  digraph:add_edge(G,B,A,rand:uniform(5)),
  createSensorsEdges(G,S,N-1,T).

createNeuronsLayers(_,PrevLayer, _,_,_,Prev3) when PrevLayer =:= Prev3->ok;

createNeuronsLayers(G,PrevLayer, NextLayer,Prev2,Next2,Prev3) when NextLayer =:= Prev2 ->
  createNeuronsLayers(G,PrevLayer-1, Next2,Prev2,Next2,Prev3);
createNeuronsLayers(G,PrevLayer, NextLayer,Prev2,Next2,Prev3) ->
  A=list_to_atom(lists:flatten(io_lib:format("neuron~p", [NextLayer]))),
  B=list_to_atom(lists:flatten(io_lib:format("neuron~p", [PrevLayer]))),
  %digraph:add_edge(G,B, A,5*(1.0 - rand:uniform())),
  digraph:add_edge(G,B, A,rand:uniform(5)),
  createNeuronsLayers(G,PrevLayer, NextLayer-1,Prev2,Next2,Prev3).


createNeuronsLayers(_, 1, _) ->ok;
createNeuronsLayers(G, NumOfLayers, NumOfNeuronsEachLayer) ->
  A=(NumOfLayers-1)*NumOfNeuronsEachLayer,
  B=NumOfLayers*NumOfNeuronsEachLayer,
  C=A-NumOfNeuronsEachLayer,
  createNeuronsLayers(G,A,B,A,B,C),
  createNeuronsLayers(G, NumOfLayers-1, NumOfNeuronsEachLayer).




createAcoautorLayers(_, IDX1, IDX2, _,_) when IDX1 =:= IDX2->ok;
createAcoautorLayers(G, IDX1, IDX2, 0,ActuatorNum)->
  createAcoautorLayers(G, IDX1-1, IDX2, ActuatorNum,ActuatorNum);
createAcoautorLayers(G, IDX1, IDX2, ActuatorNumIDX,ActuatorNum) ->
  A=list_to_atom(lists:flatten(io_lib:format("actuator~p", [ActuatorNumIDX]))),
  B=list_to_atom(lists:flatten(io_lib:format("neuron~p", [IDX1]))),
  digraph:add_edge(G,B, A,rand:uniform(5)),
  %digraph:add_edge(G,B, A,5*(1.0 - rand:uniform())),
  createAcoautorLayers(G,IDX1,IDX2,ActuatorNumIDX-1,ActuatorNum).



sendInfo(_, []) ->ok;
sendInfo(G, [{V,{_,Pid}}|T]) ->
  Neighbours_in =digraph:in_edges(G,V),
  C= [digraph:edge(G,E) || E <- Neighbours_in],
  Z=[{getPID(G,B),D} || {_,B,_,D} <- C],
  Neighbours_Out=digraph:out_edges(G,V),
  F= [digraph:edge(G,E) || E <- Neighbours_Out],
  Z2=[getPID(G,B2) || {_,_,B2,_} <- F],
  %Pid ! {self(),{V,t,Z,Z2}},
  X=gen_statem:call(Pid,{self(),{V,t,Z,Z2}}),
  sendInfo(G, T).


getPID(G,V)->
  {_,{_,PID}}=digraph:vertex(G,V),
  PID.


inverse(L) ->[{Y,X} || {X,Y} <- L].


%%%===================================================================
