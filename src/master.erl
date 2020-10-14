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
  %process_flag(trap_exit, true),
 {_A,_B,_C,D}=gui:start(node(),gui_nn,self()), %---------------> todo change back to this
  io:format("starting the gui with pid ~p ~n",[D]),

  {ok, #master_state{guiName =gui_nn,guiPid = D  , highestScore = 1000000000}}.

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






%%handle_call({init,Sensors ,Actuators ,Layers,  Neurons ,AF ,NN , Nodes},From, State = #master_state{}) ->
%%  Inputs = generate_inputs(Sensors,[]),
%%  Map = startChat(Nodes,maps:new(),self() ,Sensors ,Actuators,Layers, Neurons,AF,NN,Inputs),
%%  {reply, ok, State#master_state{nodes_Map = Map}};





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
  HighScore=10000000000,
  PopulationID = list_to_atom(lists:flatten(io_lib:format("node~p", [?MODULE]))),
  io:format("master:im in the master start pops ~n"),
  Num_of_live = num_of_alive_processes(),
  Inputs = generate_inputs(Sensors,[]),
  BB=length(atom_to_list(hd(Nodes))),
  if
      BB =:=0->
      Pid = population:start_link(self(),Sensors,Actuators,Layers,Neurons,AF2,NN,Inputs,PopulationID),
      MapE = maps:put(PopulationID,{node(),Pid} ,maps:new()) ,
      A=insert_cast(maps:to_list(MapE),HighScore),
      io:format("~p ~n",[A]);
    true ->
    {Boolean,List} = trytoconnect(Nodes,[]),
      %{Boolean,List} = {true,[]},
  if
    Boolean andalso length(List) =:= 0 ->
      {Map,Bad_Connection_node_list} = startChat(Nodes,maps:new(),[],self() ,Sensors ,Actuators,Layers, Neurons,AF2,NN,Inputs),
      if   % case of god connections to nodes!
        length(Bad_Connection_node_list) =:= 0 ->
          Pid = population:start_link(self(),Sensors,Actuators,Layers,Neurons,AF2,NN,Inputs,PopulationID),
          MapE = maps:put(PopulationID,{node(),Pid} ,Map) ,
          A=insert_cast(maps:to_list(MapE),HighScore),
          io:format("~p ~n",[A]);
          true ->
            L = maps:to_list(Map),
            stopProcess(L),
            MapE = maps:new(),
            wx_object:cast(gui_nn,{insert_nodes_again,Bad_Connection_node_list})
      end ;
    true ->
      MapE = maps:new(),
      wx_object:cast(gui_nn,{insert_nodes_again , List})
  end
  end,
  {noreply, State#master_state{nodes_Map = MapE, highestScore = HighScore}};

handle_cast({Pop_name,new_gen_hit_me,Result}, State = #master_state{ highestScore = Score ,nodes_Map = Nodes}) ->
%%  io:format("master:in in new gen hit me cast from ~p in master with result : ~p ~n" ,[Pop_name,Result]),
  {NewScore,_,_,_} = Result,
  if
    NewScore < Score ->
      Score2 = NewScore,
      wx_object:cast(gui_nn,{done,Result}) ;

      %io:format("master:ending result for gui from ~p , the result : ~p ~n" ,[Pop_name,NewScore]);
    true -> Score2 = Score
  end,
  {_Node,Pid} = maps:get(Pop_name,Nodes),
 % io:format("master:in start_insert massage to: ~p with best score: ~p ~n" ,[Pop_name,Score2]),
  gen_statem:cast(Pid,{start_insert,Score2}),
  {noreply, State#master_state{highestScore = Score2}};

handle_cast({Pop_name,worst_result}, State = #master_state{nodes_Map = Nodes ,highestScore = H}) ->
%io:format("master:in in worst_result cast from ~p ,start new iteration!! ~n" ,[Pop_name]),
{_Node,Pid} = maps:get(Pop_name,Nodes),
gen_statem:cast(Pid,{start_insert,H}),
{noreply, State#master_state{}};



handle_cast({stop},State = #master_state{nodes_Map = Nodes}) ->
  io:format("master:im in handle stop "),
  Num_of_live = num_of_alive_processes(),
  L = maps:to_list(Nodes),
  stopProcess(L),
  wx_object:cast(gui_nn,{finish_terminate}),
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
terminate(Reason, State = #master_state{}) ->
  io:format("master terminate ~p ~p ~n",[Reason,State]),
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

startChat([],Map,BadList,_,_,_,_,_,_,_,_) -> {Map,BadList};

startChat([Address|T],Map,BadList,Main_PID ,SensorNum ,ActuatorNum,NumOfLayers, NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs ) ->
  N = maps:size(Map)+1,
  PopulationID = list_to_atom(lists:flatten(io_lib:format("node~p", [N]))),
  try
     Answer = rpc:call(Address, population, start_link, [Main_PID,SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs,PopulationID]),
      io:format("master:rpc sent answer: ~p ~n" ,[Answer]),
      if
        is_pid(Answer)->
          M2 = maps:put(PopulationID,{Address,Answer},Map),
          startChat(T,M2,BadList,Main_PID ,SensorNum ,ActuatorNum,NumOfLayers, NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs);
         true ->
           io:format("wrong rpc cast to node ~p",[Address]),
           L2 = BadList ++ [Address],
           startChat(T,Map,L2,Main_PID ,SensorNum ,ActuatorNum,NumOfLayers, NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs)
      end
     catch
         _Reason:_Reason1->
           io:format("catch error in start chat try to rpc node ~p",[_Reason1]),
           startChat(T,Map,BadList ++ [PopulationID],Main_PID ,SensorNum ,ActuatorNum,NumOfLayers, NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs)
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
trytoconnect([],L)->
  if
    length(L) =:= 0 ->
      io:format("master good connections to nodes ~n"),
      {true,L};
    true -> {flase,L}
  end;
trytoconnect([H|T],L) ->
 A =net_kernel:connect_node(H),
  if
    A =:= true -> trytoconnect(T,L);
    true->
          io:format(" connection mistake to node ~p ~n" , [H]),
          L2 = L ++ [H],
          trytoconnect(T,L2)
  end.

insert_cast([],_)->ok;
insert_cast([{KEY,{_Node,Pid}}|T], Highest_score) ->
  io:format("master:start insert from master to ~p ~n" , [KEY]),
  Answer=gen_statem:cast(Pid,{start_insert,Highest_score}),
  %'node1@avnido-VirtualBox'
  %Answer=gen_statem:cast({global,KEY},{start_insert}),
  io:format("master :cast to pop start insert to ~p , Answer is : ~p ~n" , [KEY,Answer]),
  insert_cast(T,Highest_score).


num_of_alive_processes() ->
  L5= processes(),
  L6=[is_process_alive(A)|| A<-L5],
  L7=[true || true<-L6],
  L=length(L7),
  L.




stopProcess([])->ok;
stopProcess([{_Name,{_Node,Pid}}|T])->

  try
    gen_statem:stop(Pid) of
    _Result->stopProcess(T)
  catch
    _Reason:_Reason1->stopProcess(T)

  end.


deleteResults(L)->
  receive
    {X,{From,result,Result}}->deleteResults(L++[{From,result,Result}])

  after 0->L
  end.


makeItCrash(N)->
  1/(1-rand:uniform(N))
  .