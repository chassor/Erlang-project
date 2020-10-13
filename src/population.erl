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

-record(population_state, {id,num_of_nn,main_PID,sensorNum,actuatorNum,numOfLayers,numOfNeuronsEachLayer,af,nn_pids_map,nn_pids_map2, finesses_Map , inputs ,highest_score,generation_Map}).

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
 % process_flag(trap_exit, true),
 Y2= global:register_name(ID,self()),
 io:format(" pop: im in pop , my id is : ~p ~n" , [ID]),
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
    highest_score = 1000000000,
    finesses_Map = maps:new()},
  {PIMap,GeMAp}=create_NN_Agents(Num_Of_NN_AGENTS,maps:new(),maps:new(),State),
  Map2= received_graphs(PIMap,maps:to_list(PIMap)),
  io:format(" finish_initilize node ~n" , []),
  {ok, idle, State#population_state{nn_pids_map = Map2, nn_pids_map2 = Map2 , generation_Map = GeMAp}}.

idle(cast,{start_insert,Highest_score},State = #population_state{id = Id})->
  io:format("pop: im in ~p idle state ~n" ,[Id]),
  Map=State#population_state.nn_pids_map,
  Inputs=State#population_state.inputs,
  insert_inputs(maps:to_list(Map),Inputs),
  NextStateName = network_in_computation,
  {next_state, NextStateName, State#population_state{highest_score = Highest_score}};

idle(EventType, EventContent, Data) ->
  handle_common(EventType, EventContent, Data,idle).

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



network_in_computation(cast,{KEY,result,NN_Result},State = #population_state{id = Id , main_PID = MainPid ,highest_score = Score , finesses_Map = FitnessMap,generation_Map = Generation_map})->
  io:format("pop: im in node1 network_in_computation state ~n"),
Fitness = std_fitness(NN_Result),
UpdateFitnessMap = maps:put(KEY,{Fitness,NN_Result},FitnessMap),
  Counter = maps:remove(KEY,State#population_state.nn_pids_map2),
 % io:format("MAP1 =~p ~n",[maps:keys(State#population_state.nn_pids_map)]),
 % io:format("MAP2 =~p ~n",[maps:keys(State#population_state.nn_pids_map2)]),
  %io:format("KEY =~p ~n",[KEY]),
 % io:format("countermap =~p ~n",[Counter]),
  Size = maps:size(Counter),
 % io:format("pop: im in node1 mapSize =~p ~n",[Size]),
  if
     Size =:= 0 ->
   %    io:format("pop: im in node mapSize =0 ~n"),
      MAP=State#population_state.nn_pids_map,
      R = split_nn(UpdateFitnessMap),
      BestNN  = element(1,R),
      WorstNN = element(2,R),
      {_Pid,G2} = maps:get(element(3,R),MAP),
       G={toGraph:getVerticesList(G2),toGraph:getEdgesList(G2)},
       Best_Fitness_of_this_iteration = element(4,R),
       Generation = maps:get(KEY,Generation_map),
      Result_for_master = {Best_Fitness_of_this_iteration,element(5,R),G,Generation},
       {Map2,Gmap}=terminate_worst_nn(WorstNN,MAP,Generation_map),
       {Map3,Gmap2} = create_NN_Agents_M(Map2,BestNN,Gmap,State),
      %Keys_good_map=maps:without(maps:keys(Map2),Map3),
      MangerPid=State#population_state.main_PID,
      FitnessMap=update_fitness_map(UpdateFitnessMap,WorstNN),
      %MangerPid ! {self(),new_gen_hit_me,Result_for_master},
  %     io:format("going to cast master ~n"),
       if
         Best_Fitness_of_this_iteration < Score ->
          gen_server:cast(MainPid,{Id,new_gen_hit_me,Result_for_master}) ;
           true ->
             io:format(" best score is: ~p ,get low score : ~p so  pop dont sending to master result! ~n",[Score,Best_Fitness_of_this_iteration]),
             gen_server:cast(MainPid,{Id,worst_result})

       end,

  %     io:format("casted result to master ~n"),
      %State#population_state.main_PID ! {cast_me , Map3}, % for check
      {next_state,idle,State#population_state{nn_pids_map = Map3, nn_pids_map2 = Map3 , finesses_Map = FitnessMap}};

      true -> {keep_state,State#population_state{finesses_Map = UpdateFitnessMap , nn_pids_map2 = Counter,generation_Map = Gmap2 }}
  end;


network_in_computation(EventType, EventContent, Data) ->
  handle_common(EventType, EventContent, Data,network_in_computation).

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
  end;

create_new_generation(EventType, EventContent, Data) ->
  handle_common(EventType, EventContent, Data,create_new_generation).



handle_common(info, {'EXIT',PID,normal},State = #population_state{},CuRR_State) ->
  io:format("{'EXIT',PID,normal} handle common node1 ~n"),
   {keep_state, State};

handle_common(info, {'EXIT',PID,_Reason},State = #population_state{},CuRR_State) ->
  io:format("{'EXIT',PID,_Reason} handle common node1 ~n"),
  {keep_state, State}
;



handle_common(info,_,State = #population_state{},_State) ->
  io:format("info,_,State = #population_state{},_State handle common node1 ~n"),
  {keep_state, State};

handle_common(_,_,State = #population_state{},_State) ->
  io:format("_,_,State = #population_state{},_State handle common node1 ~n"),
  {keep_state, State}.

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
terminate(Reason, StateName, _State = #population_state{nn_pids_map = Map }) ->
  io:format("node terminate ~p ~p ~n",[Reason,StateName]),
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





create_NN_Agents(0,Map,Gmap,_)->{Map,Gmap};
create_NN_Agents(N,Map,Gmap,S)->
  io:format("pop:create_NN_Agents ~n"),
  Key = genarateIdfromAtom(),
  PID = nn_agent:start_link(S#population_state.sensorNum,S#population_state.actuatorNum,S#population_state.numOfLayers,
    S#population_state.numOfNeuronsEachLayer,self(),Key,new),
  Map2 = maps:put(Key,{PID,undefined},Map),
  Gmap2 = maps:put (Key,1,Gmap),
  create_NN_Agents(N-1,Map2,Gmap2,S).

create_NN_Agents_M(M,[],Gmap,_)->{M,Gmap};
create_NN_Agents_M(M,[H|T],Gmap,S)->
  NewKey = genarateIdfromAtom(),
  Key=element(1,H),
  G = element(2,maps:get(Key,M)),
  PID = nn_agent:start_link(S#population_state.sensorNum,S#population_state.actuatorNum,S#population_state.numOfLayers,S#population_state.numOfNeuronsEachLayer,self(),NewKey,G),
  %PID = rand:uniform(),
  NewG = gen_statem:call(PID,{self(),get_graph}),
   gen_statem:call(PID,{self(),mutate}),
  Map2 = maps:put(NewKey,{PID,NewG},M),
  Generation = maps:get(Key,Gmap) +1,
  Gmap2 = maps:put(NewKey,Generation,Gmap),
  create_NN_Agents_M(Map2,T,Gmap2,S).





terminate_worst_nn([],Map,Gmap)->{Map,Gmap};

terminate_worst_nn([H|T],Map,Gmap)->
  {KEY,_}=H,
  PID=element(1,maps:get(KEY,Map)),
 % G = element(2,maps:get(KEY,Map)),
 % digraph:delete(G),
  gen_statem:stop(PID),
  Map2=maps:remove(KEY,Map),
  Gmap2=maps:remove(KEY,Gmap),
  terminate_worst_nn(T,Map2,Gmap2).



split_nn(FitnessMAp)->
    Fun=fun(A,B)->minn(A,B)end,
    Sort_List = lists:sort(Fun,maps:to_list(FitnessMAp)),
     X=round(length(Sort_List)/2),
     L=lists:nth(1,Sort_List),
    {BestNN , WorstNN} = lists:split(X,Sort_List),
      Return = {BestNN,WorstNN,element(1,L),element(1,element(2,L)),element(2,element(2,L))},
      Return.

    minn(A,B) when is_tuple(A) and is_tuple(B)->
      {_Key1,{Val1,_Result1,_}} = A,
      {_Key2,{Val2,_Result2,_}} = B,
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
  io:format("pop: im in pop insert mean the gen cast works ~n"),
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


avg(List)->
  lists:sum(List)/length(List).


std_fitness(List)->
  Avg = avg(List),
  std_fitness(List,Avg,[]).

std_fitness([Val|List],Avg,Acc)->
  std_fitness(List,Avg,[math:pow(Avg-Val,2)|Acc]);
std_fitness([],_Avg,Acc)->
  Variance = lists:sum(Acc)/length(Acc),
  math:sqrt(Variance).

fitness(L)->fitness(L,0).

fitness([],Sum)->
  abs(3-math:sqrt(Sum));

fitness([H|T],Sum)->
  Sum2=Sum+(H*H),
  fitness(T,Sum2).
