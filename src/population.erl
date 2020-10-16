%%%-------------------------------------------------------------------
%%% @author DOR
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
-module(population).
-author("DOR").
-behaviour(gen_statem).
-export([start_link/9,fitness/2,network_in_computation/3,minn/2,idle/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

-define(SERVER, ?MODULE).

-record(population_state, {id,num_of_nn,main_PID,sensorNum,actuatorNum,numOfLayers,numOfNeuronsEachLayer,fitnessFunc,nn_pids_map,nn_pids_map2, finesses_Map , inputs ,highest_score,generation_Map}).

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


%%  this function is called by the new process to initialize the population records values, and build our networks by the passed values .
init({Main_PID,SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer, Fit,Num_Of_NN_AGENTS,Inputs,ID}) ->
 global:register_name(ID,self()),
 io:format("initilize popupaltion; id:  ~p ~n" , [ID]),
  State=#population_state{
    id = ID ,
    inputs = Inputs,
    main_PID = Main_PID,
    num_of_nn = Num_Of_NN_AGENTS,
    actuatorNum = ActuatorNum,
    numOfLayers = NumOfLayers,
    numOfNeuronsEachLayer = NumOfNeuronsEachLayer,
    sensorNum = SensorNum,
    fitnessFunc = Fit,
    highest_score = 1000000000,
    finesses_Map = maps:new()},
  {PIMap,GeMAp} = create_NN_Agents(Num_Of_NN_AGENTS,maps:new(),maps:new(),State),
  Map2 = received_graphs(PIMap,maps:to_list(PIMap)),
  io:format("finish initilize popupaltion; id:  ~p ~n" , [ID]),
  {ok, idle, State#population_state{nn_pids_map = Map2, nn_pids_map2 = Map2 , generation_Map = GeMAp}}.


%%  this function is called to insert inputs to the networks for calculation , so we sending insert message to all neurons network with the inputs .
idle(cast,{start_insert,Highest_score},State = #population_state{nn_pids_map = Map ,inputs = Inputs})->
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


% in this state we gets result from all the neuron networks that under this population ,
% when all result received we choose the networks that achieve the best fitness and the networks that achieve the worst fitness ,
%best networks survived and the result of the one with the best fitness forward to master , worst networks terminates , and new networks created with mutate the best networks.
network_in_computation(cast,{KEY,result,NN_Result},State = #population_state{id = Id , main_PID = MainPid ,highest_score = Score , finesses_Map = FitnessMap,
  generation_Map = Generation_map,fitnessFunc = Fit,inputs = InputsList})->
Fitness = calcFitness(NN_Result,Fit,InputsList),
UpdateFitnessMap = maps:put(KEY,{Fitness,NN_Result},FitnessMap),
  Counter = maps:remove(KEY,State#population_state.nn_pids_map2),
  Size = maps:size(Counter),
  if
    Size =:= 0 ->
         MAP=State#population_state.nn_pids_map,
         R = split_nn(UpdateFitnessMap),
         BestNN  = element(1,R),
         WorstNN = element(2,R),
         {_Pid,G2} = maps:get(element(3,R),MAP),
         G={toGraph:getVerticesList(G2),toGraph:getEdgesList(G2)},
         Best_Fitness_of_this_iteration = element(4,R),
         Generation = maps:get(KEY,Generation_map),
         Result_for_master = {Best_Fitness_of_this_iteration,element(5,R),G,Generation},
         {Map2,Gmap} = terminate_worst_nn(WorstNN,MAP,Generation_map),
         {Map3,Gmap2} = create_NN_Agents_M(Map2,BestNN,Gmap,State),
         FitnessMap3=update_fitness_map(UpdateFitnessMap,WorstNN),
         Processes = master:num_of_alive_processes(),
       if
         Best_Fitness_of_this_iteration < Score ->
          gen_server:cast(MainPid,{Id,new_gen_hit_me,Result_for_master,Processes}) ;
           true ->
             gen_server:cast(MainPid,{Id,worst_result,Processes})

       end,

      {next_state,idle,State#population_state{nn_pids_map = Map3, nn_pids_map2 = Map3 , finesses_Map = FitnessMap3,generation_Map = Gmap2}};

      true -> {keep_state,State#population_state{finesses_Map = UpdateFitnessMap , nn_pids_map2 = Counter }}
  end;


network_in_computation(EventType, EventContent, Data) ->
  handle_common(EventType, EventContent, Data,network_in_computation).





handle_common(info, {'EXIT',_PID,normal},State = #population_state{},_CuRR_State) ->
   {keep_state, State};

handle_common(info, {'EXIT',_PID,_Reason},State = #population_state{},_CuRR_State) ->
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

%%This function is called by a gen_statem when it is about to terminate , so we terminates all the neuron networks that we created .
terminate(Reason, StateName, _State = #population_state{nn_pids_map = Map ,id = ID}) ->
  io:format("population ~p terminate ~p in ~p state ~n",[ID,Reason,StateName]),
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




%% In this func we create the networks and update the relevant maps.
create_NN_Agents(0,Map,Gmap,_)->{Map,Gmap};
create_NN_Agents(N,Map,Gmap,S)->
  Key = genarateIdfromAtom(),
  PID = nn_agent:start_link(S#population_state.sensorNum,S#population_state.actuatorNum,S#population_state.numOfLayers,
    S#population_state.numOfNeuronsEachLayer,self(),Key,new),
  Map2 = maps:put(Key,{PID,undefined},Map),
  Gmap2 = maps:put (Key,1,Gmap),
  create_NN_Agents(N-1,Map2,Gmap2,S).


%% In this func we create the mutate networks and update the relevant maps.
create_NN_Agents_M(M,[],Gmap,_)->{M,Gmap};
create_NN_Agents_M(M,[H|T],Gmap,S)->
  NewKey = genarateIdfromAtom(),
  Key=element(1,H),
  G = element(2,maps:get(Key,M)),
  PID = nn_agent:start_link(S#population_state.sensorNum,S#population_state.actuatorNum,S#population_state.numOfLayers,S#population_state.numOfNeuronsEachLayer,self(),NewKey,G),
  NewG = gen_statem:call(PID,{self(),get_graph}),
   gen_statem:call(PID,{self(),mutate}),
  Map2 = maps:put(NewKey,{PID,NewG},M),
  Generation = maps:get(Key,Gmap) +1,
  Gmap2 = maps:put(NewKey,Generation,Gmap),
  create_NN_Agents_M(Map2,T,Gmap2,S).




%% In this func we terminate the worst networks and update the relevant maps.
terminate_worst_nn([],Map,Gmap)->{Map,Gmap};
terminate_worst_nn([H|T],Map,Gmap)->
  {KEY,_}=H,
  PID=element(1,maps:get(KEY,Map)),
  gen_statem:stop(PID),
  Map2=maps:remove(KEY,Map),
  Gmap2=maps:remove(KEY,Gmap),
  terminate_worst_nn(T,Map2,Gmap2).


%% In this func we get the fitness map ,
%% and return tuple with the result for master include the two list of best and worst networks and the digraph and the best fitness.
split_nn(FitnessMAp)->
    Fun=fun(A,B)->minn(A,B)end,
    Sort_List = lists:sort(Fun,maps:to_list(FitnessMAp)),
     X=round(length(Sort_List)/2),
     L=lists:nth(1,Sort_List),
    {BestNN , WorstNN} = lists:split(X,Sort_List),
      Return = {BestNN,WorstNN,element(1,L),element(1,element(2,L)),element(2,element(2,L))},
      Return.

%% this function check min between two finesses that store in tuples
    minn(A,B) when is_tuple(A) and is_tuple(B)->
      {_Key1,{Val1,_Result1}} = A,
      {_Key2,{Val2,_Result2}} = B,
      if
        Val1<Val2 -> true ;
        true -> false
      end.

%% In this func we store in our maps the digraph for every network .
received_graphs(Map,[])->Map;
received_graphs(Map,[H|T])->
  Key = element(1,H),
  PID = element(1,element(2,H)),
  G = gen_statem:call(PID,{self(),get_graph}),
  %G=digraph:new(),
  Tuple = setelement(2,element(2,H),G),
  Map2 = maps:put(Key,Tuple,Map),
  received_graphs(Map2,T).





%% In this func we send start massage to networks for calculation .
insert_inputs([],_)->ok;
insert_inputs([{_KEY,{PID,_G}}|T],Inputs) ->
  gen_statem:cast(PID,{self(),insert_input,Inputs}),
  insert_inputs(T,Inputs).

%The genarateIdfromAtom creates a unique atom by using the unique Ids from generate_id func .
genarateIdfromAtom()->list_to_atom(lists:flatten(io_lib:format("nn~p", [nn_agent:generate_id()]))).

%% this function remove the worst networks from the map .
update_fitness_map(M,[])->M;
update_fitness_map(M,[{Key,_}|T]) ->
  M2 = maps:remove(Key,M),
  update_fitness_map(M2,T).

% return avg of list
avg(List)->
  lists:sum(List)/length(List).



fitness([H|T],Sum)->
  Sum2=Sum+(H*H),
  fitness(T,Sum2).

%% this function retuen the wanted fitness by calling the relevant fitness function .
calcFitness(NNResult, Fitness,InputList) ->
case Fitness of
  go_to_pi->goToPi(NNResult,0);
  go_to_e->goToE(NNResult,0,math:exp(1));
  fibonacci->fibonacci(NNResult,0,1,1);
  avg->abs(avg(NNResult)-avg(InputList))

end.

%% In this func we calculate distance of every result from pi
goToPi([],Sum)->Sum;
goToPi([H|T], Sum) ->
  goToPi(T,Sum+abs(math:pi()-H)).

%% In this func we calculate distance of every result from E
goToE([],Sum,_)->Sum;
goToE([H|T], Sum,E) ->
  goToE(T,Sum+abs(E-H),E).


fibonacci([], Sum,_N1,_N2) ->Sum;
fibonacci([H|T], Sum,N1,N2) ->
  fibonacci(T,Sum+abs(N1-H),N2,N1+N2).