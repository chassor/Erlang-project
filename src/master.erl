%%%-------------------------------------------------------------------
%%% @author DOR
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2020 1:10 PM
%%%-------------------------------------------------------------------
-module(master).
-author("DOR").

-behaviour(gen_server).

%% API
-export([start_link/0, makeItCrash/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, num_of_alive_processes/0]).

-define(SERVER, ?MODULE).

-record(master_state, {result_Tuple ,nodes_Map,guiPid,guiName,highestScore,first_run}).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({global,master}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #master_state{}} | {ok, State :: #master_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  process_flag(trap_exit, true),
  {_A,_B,_C,D}=gui:start(node(),gui_nn,self()),

  {ok, #master_state{guiName =gui_nn,guiPid = D }}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #master_state{}) ->
  {reply, Reply :: term(), NewState :: #master_state{}} |
  {reply, Reply :: term(), NewState :: #master_state{}, timeout() | hibernate} |
  {noreply, NewState :: #master_state{}} |
  {noreply, NewState :: #master_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #master_state{}} |
  {stop, Reason :: term(), NewState :: #master_state{}}).






handle_call({init,Sensors ,Actuators ,Layers,  Neurons ,AF ,NN , Nodes},From, State = #master_state{}) ->
  Inputs = generate_inputs(Sensors,[]),
  Map = startChat(Nodes,maps:new(),self() ,Sensors ,Actuators,Layers, Neurons,AF,NN,Inputs),
  {reply, ok, State#master_state{nodes_Map = Map}};





handle_call(_Request, _From, State = #master_state{}) ->
  {reply, ok, State}.



%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #master_state{}) ->
  {noreply, NewState :: #master_state{}} |
  {noreply, NewState :: #master_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #master_state{}}).





%%handle_cast(_Request, State = #master_state{}) ->
%%  {noreply, State};

handle_cast({start,Sensors ,Actuators ,Layers,  Neurons ,AF2 ,NN,Nodes}, State = #master_state{}) ->
  Num_of_live = num_of_alive_processes(),
  Inputs = generate_inputs(Sensors,[]),
  %trytoconnect(Nodes)  ---- for terminal
  Map = startChat(Nodes,maps:new(),self() ,Sensors ,Actuators,Layers, Neurons,AF2,NN,Inputs),
  A=insert_cast(maps:to_list(Map)),
  {noreply, State#master_state{nodes_Map = Map ,highestScore = 1000000000}};

handle_cast({Pop_name,new_gen_hit_me,Result}, State = #master_state{ highestScore = Score,nodes_Map =NodesMap}) ->
  {NewScore,_,_} = Result,
  io:format("master got result"),
  if
    NewScore < Score ->
      Score2 = NewScore,
      timer:sleep(2000),
      io:format("send result to gui"),
      gen_statem:cast(gui_nn,{done,Result}) ;
    true -> Score2 = Score
  end,
  {_nodemap,Pid}=maps:get(Pop_name,NodesMap),
  gen_statem:cast(Pid,{start_insert}),
  {noreply, State#master_state{highestScore = Score2}};

handle_cast({stop},State = #master_state{nodes_Map = Nodes}) ->
  Num_of_live = num_of_alive_processes(),
  L = maps:to_list(Nodes),
  stopProcess(L),
  gen_statem:cast(gui_nn,{finish_terminate}),
  {noreply, State#master_state{highestScore = -10000000000000}}.



%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #master_state{}) ->
  {noreply, NewState :: #master_state{}}|
{noreply, NewState :: #master_state{}, timeout() | hibernate} |
{stop, Reason :: term(), NewState :: #master_state{}}).
handle_info({'EXIT',PID,normal}, State = #master_state{}) ->
  {noreply, State};

handle_info({'EXIT',PID,_Reason}, State = #master_state{}) ->
  {noreply, State};

handle_info(_Info, State = #master_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #master_state{}) -> term()).
terminate(_Reason, _State = #master_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #master_state{},
    Extra :: term()) ->
  {ok, NewState :: #master_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #master_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

startChat([],Map,_,_,_,_,_,_,_,_) -> Map;

startChat([Address|T],Map,Main_PID ,SensorNum ,ActuatorNum,NumOfLayers, NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs ) ->
  N = maps:size(Map)+1,
  PopulationID = list_to_atom(lists:flatten(io_lib:format("node~p", [N]))),
     Answer=rpc:call(Address, population, start_link, [Main_PID,SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs,PopulationID]),
      if
        is_pid(Answer)->
          M2 = maps:put(PopulationID,{Address,Answer},Map),
          startChat(T,M2,Main_PID ,SensorNum ,ActuatorNum,NumOfLayers, NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs);
        true ->  io:format("wrong rpc cast to population")
      end.



generate_id() ->
  {MegaSeconds,Seconds,MicroSeconds} = now(),
  1/(MegaSeconds + Seconds + MicroSeconds).

generate_inputs(0,L)-> L;
generate_inputs(N,L)->
  X = random_input(),
  L2 = L ++ [X] ,
  generate_inputs(N-1,L2).


random_input()->
  X=5*(1.0 - rand:uniform()),
  case rand:uniform(2) of
    1-> Y=X;
    2-> Y=-1*X
  end,
  Y.
trytoconnect([])->ok;
trytoconnect([H|T]) ->
 A =net_kernel:connect_node(H),
  if
    A =:= true -> trytoconnect(T)  ;
    true ->  io:format(" connection mistake")
  end.

insert_cast([])->ok;
insert_cast([{KEY, {_Node,PID}}|T]) ->
  io:format("Start insert to {global,~p}",[KEY]),
  Answer=gen_statem:cast(PID,{start_insert}),
  insert_cast(T).


num_of_alive_processes() ->
  L5= processes(),
  L6=[is_process_alive(A)|| A<-L5],
  L7=[true || true<-L6],
  L=length(L7),
  L.

stopProcess([])->ok;
stopProcess([{Name,{_Node,Pid}}|T])->
  X=is_process_alive(Pid),
  if
    X=:=true->
      gen_statem:stop(Pid),
      stopProcess(T);
    true->stopProcess(T)

  end.

deleteResults(L)->
  receive
    {X,{From,result,Result}}->deleteResults(L++[{From,result,Result}])

  after 0->L
  end.


makeItCrash(N)->
  1/(1-rand:uniform(N))
  .