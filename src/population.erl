%%%-------------------------------------------------------------------
%%% @author DOR
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
-module(population).
-author("DOR").
-behaviour(gen_statem).
-export([start_link/9,fitness/2,network_in_computation/3,minn/2,create_new_generation/3,idle/3,c/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

-define(SERVER, ?MODULE).

-record(population_state, {id,num_of_nn,main_PID,sensorNum,actuatorNum,numOfLayers,numOfNeuronsEachLayer,af,nn_pids_map,nn_pids_map2, finesses_Map , inputs}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link(Main_PID,SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs,ID) ->
  {_,PID }= gen_statem:start_link({global,ID}, ?MODULE, {Main_PID,SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs,ID},[]),
  PID.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init({Main_PID,SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs,ID}) ->
  %process_flag(trap_exit, true),
  io:format("im in node1 , my id is : ~p ~n" , [ID]),
  State=#population_state{
    id = ID ,
    inputs = Inputs,
    main_PID = Main_PID,
    num_of_nn = Num_Of_NN_AGENTS,
    actuatorNum = ActuatorNum,
    numOfLayers = NumOfLayers,
    numOfNeuronsEachLayer = NumOfNeuronsEachLayer,
    sensorNum = SensorNum,
    af = AF,
    finesses_Map = maps:new()},
  Map=create_NN_Agents(Num_Of_NN_AGENTS,maps:new(),State),
  Map2= received_graphs(Map,maps:to_list(Map)),
  {ok, idle, State#population_state{nn_pids_map = Map2, nn_pids_map2 = Map2}}.

idle(cast,{start_insert},State = #population_state{})->
  io:format("im in node1 idle state ~n"),
  Map=State#population_state.nn_pids_map,
  Inputs=State#population_state.inputs,
  insert_inputs(maps:to_list(Map),Inputs),
  NextStateName = network_in_computation,
  {next_state, NextStateName, State}.

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

state_name(_EventType, _EventContent, State = #population_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.



network_in_computation(cast,{KEY,result,NN_Result},State = #population_state{id = Id})->
  io:format("im in node1 computation state ~n"),
  XX=1/(3-rand:uniform(8)),
Fitness = fitness(NN_Result,0),
UpdateFitnessMap = maps:put(KEY,{Fitness,NN_Result},State#population_state.finesses_Map),
  Counter = maps:remove(KEY,State#population_state.nn_pids_map2),
  Size = maps:size(Counter),
  if
    Size =:= 0 ->
      MAP=State#population_state.nn_pids_map,
      R = split_nn(UpdateFitnessMap),
      BestNN  = element(1,R),
      WorstNN = element(2,R),
      {_Pid,G} = maps:get(element(3,R),MAP),
      Result_for_master = {element(4,R),element(5,R),G},
      Map2=terminate_worst_nn(WorstNN,MAP),
      Map3 = create_NN_Agents_M(Map2,BestNN,State),
      %Keys_good_map=maps:without(maps:keys(Map2),Map3),
      MangerPid=State#population_state.main_PID,
      FitnessMap=update_fitness_map(UpdateFitnessMap,WorstNN),
      %MangerPid ! {self(),new_gen_hit_me,Result_for_master},
      Answer=gen_server:cast({global,master},{Id,new_gen_hit_me,Result_for_master}),
      %State#population_state.main_PID ! {cast_me , Map3}, % for check
      {next_state,idle,State#population_state{nn_pids_map = Map3, nn_pids_map2 = Map3 , finesses_Map = FitnessMap}};

      true -> {keep_state,State#population_state{finesses_Map = UpdateFitnessMap , nn_pids_map2 = Counter }}
  end.

create_new_generation(cast,{KEY,new_nn_mutation,Mutation_G},State  = #population_state{})->
  Tuple={element(1,maps:get(KEY,State#population_state.nn_pids_map)),Mutation_G},
  Map2 = maps:put(KEY,Tuple,State#population_state.nn_pids_map),
  Counter = maps:remove(KEY,State#population_state.nn_pids_map2),
  Size = maps:size(Counter),
  if
    Size =:= 0 ->
      MangerPid=State#population_state.main_PID,
      MangerPid ! {self(),new_gen_hit_me},
      {next_state,idle,State#population_state{nn_pids_map = Map2, nn_pids_map2 = Map2  }}  ;
    true -> {keep_state,State#population_state{nn_pids_map2 = Counter , nn_pids_map = Map2 }}
  end.





%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #population_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #population_state{nn_pids_map = Map }) ->
  L = maps:to_list(Map),
  [ gen_statem:stop(PID)  || {_KEY,{PID,_G}} <- L],
   ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #population_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%fitness_function(Inputs_List,Outputs_List,Sum)->
  %%List=lists:append(Inputs_List,Outputs_List),
  %%fitness(List,Sum).

fitness([],Sum)->
  abs(3-math:sqrt(Sum));
fitness([H|T],Sum)->
  Sum2=Sum+(H*H),
fitness(T,Sum2).

create_NN_Agents(0,Map,_)->Map;
create_NN_Agents(N,Map,S)->
  Key = genarateIdfromAtom(),
  PID = nn_agent:start_link(S#population_state.sensorNum,S#population_state.actuatorNum,S#population_state.numOfLayers,
    S#population_state.numOfNeuronsEachLayer,self(),Key,new),
  Map2 = maps:put(Key,{PID,undefined},Map),
  create_NN_Agents(N-1,Map2,S).

create_NN_Agents_M(M,[],_)->M;
create_NN_Agents_M(M,[H|T],S)->
  NewKey = genarateIdfromAtom(),
  Key=element(1,H),
  G = element(2,maps:get(Key,M)),
  PID = nn_agent:start_link(S#population_state.sensorNum,S#population_state.actuatorNum,S#population_state.numOfLayers,S#population_state.numOfNeuronsEachLayer,self(),NewKey,G),
  %PID = rand:uniform(),
  NewG = gen_statem:call(PID,{self(),get_graph}),
   gen_statem:call(PID,{self(),mutate}),
  Map2 = maps:put(NewKey,{PID,NewG},M),
  create_NN_Agents_M(Map2,T,S).





terminate_worst_nn([],Map)->Map;

terminate_worst_nn([H|T],Map)->
  {KEY,_}=H,
  PID=element(1,maps:get(KEY,Map)),
 % G = element(2,maps:get(KEY,Map)),
 % digraph:delete(G),
  gen_statem:stop(PID),
  Map2=maps:remove(KEY,Map),
  terminate_worst_nn(T,Map2).



split_nn(FitnessMAp)->
    Fun=fun(A,B)->minn(A,B)end,
    Sort_List = lists:sort(Fun,maps:to_list(FitnessMAp)),
     X=round(length(Sort_List)/2),
     L=lists:nth(1,Sort_List),
    {BestNN , WorstNN} = lists:split(X,Sort_List),
      Return = {BestNN,WorstNN,element(1,L),element(1,element(2,L)),element(2,element(2,L))},
      Return.

    minn(A,B) when is_tuple(A) and is_tuple(B)->
      {_Key1,{Val1,_Result1}} = A,
      {_Key2,{Val2,_Result2}} = B,
      if
        Val1<Val2 -> true ;
        true -> false
      end.

received_graphs(Map,[])->Map;
received_graphs(Map,[H|T])->
  Key = element(1,H),
  PID = element(1,element(2,H)),
  G = gen_statem:call(PID,{self(),get_graph}),
  %G=digraph:new(),
  Tuple = setelement(2,element(2,H),G),
  Map2 = maps:put(Key,Tuple,Map),
  received_graphs(Map2,T).

generate_id() ->
  {MegaSeconds,Seconds,MicroSeconds} = now(),
  1/(MegaSeconds*1000000 + Seconds + MicroSeconds/1000000).
%The generate_id/0 creates a unique Id using current time, the Id is a floating point value. The generate_ids/2 function creates a list of unique Ids.

insert_inputs([],_)->ok;
insert_inputs([{_KEY,{PID,_G}}|T],Inputs) ->
  io:format("im in pop insert mean the gen cast works ~n"),
  gen_statem:cast(PID,{self(),insert_input,Inputs}),
  insert_inputs(T,Inputs).


genarateIdfromAtom()->list_to_atom(lists:flatten(io_lib:format("nn~p", [generate_id()]))).

%[{Key,Fintess},{}..]
update_fitness_map(M,[])->M;
update_fitness_map(M,[{Key,_}|T]) ->
  M2 = maps:remove(Key,M),
  update_fitness_map(M2,T).


c()->
  compile:file('nn_agent'),
  compile:file('neuron2'),
  compile:file('mutate_generator').

