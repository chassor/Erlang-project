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
-export([start_link/0, makeItCrash/1, buffer/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, num_of_alive_processes/0,monitor_loop/1]).

-define(SERVER, ?MODULE).

-record(master_state, {result_Tuple ,nodes_Map,guiPid,guiName,highestScore,first_run,bufferPid,monitorPid,inputList,processes_Map}).

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

%% in this function the master start the gui and init the relevant record elements.
init([]) ->
  %process_flag(trap_exit, true),
 {_A,_B,_C,D}=gui:start(node(),gui_nn,self()), %---------------> todo change back to this
  io:format("starting the gui with pid ~p ~n",[D]),

  {ok, #master_state{guiName =gui_nn,guiPid = D  , highestScore = 1000000000,nodes_Map = maps:new() ,processes_Map = maps:new()}}.

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




%% in this case the master received the require values from the gui .
%% the master check the connection to the remotes nodes and start the population on the nodes.
%% in addition master spawn  buffer process ,and monitor process for catch failure.
%% after population created in the require nodes the master update the maps and start the calculation of the networks on each node.

handle_cast({start,Sensors ,Actuators ,Layers,  Neurons ,AF2 ,NN,Nodes}, State = #master_state{}) ->
  BufferPid=spawn(?MODULE,buffer,[]),
  HighScore=10000000000,
  PopulationID = list_to_atom(lists:flatten(io_lib:format("node~p", [?MODULE]))),
  io:format("master:im in the master start pops ~n"),
  Inputs = generate_inputs(Sensors,[]),
  BB=length(atom_to_list(hd(Nodes))),
  if
      BB =:=0->
      Pid = population:start_link(self(),Sensors,Actuators,Layers,Neurons,AF2,NN,Inputs,PopulationID),
      MapE = maps:put(PopulationID,{node(),Pid} ,maps:new()) ,
      A=insert_cast(maps:to_list(MapE),HighScore),
      io:format("~p ~n",[A]),
      MonitorPid = spawn(?MODULE,monitor_loop,[self()]);
    true ->
     {Boolean,List} = trytoconnect(Nodes,[]),
      %{Boolean,List} = {true,[]},
  if
    Boolean andalso length(List) =:= 0 ->
      {Map,Bad_Connection_node_list} = startChat(Nodes,maps:new(),[],self() ,Sensors ,Actuators,Layers, Neurons,AF2,NN,Inputs),
      if   % case of god connections to nodes!
        length(Bad_Connection_node_list) =:= 0 ->
          MonitorPid = spawn(?MODULE,monitor_loop,[self()]),
          io:format("pawn(master,monitor_loop,[]), answer is: ~p ~n",[MonitorPid]),
          Pid = population:start_link(self(),Sensors,Actuators,Layers,Neurons,AF2,NN,Inputs,PopulationID),
          MapE = maps:put(PopulationID,{node(),Pid} ,Map) ,
          A=insert_cast(maps:to_list(MapE),HighScore),
          io:format("~p ~n",[A]);
          true ->
            L = maps:to_list(Map),
            stopProcess(L),
            BufferPid ! kill,
            MapE = maps:new(),
            MonitorPid = ok,
            wx_object:cast(gui_nn,{insert_nodes_again,Bad_Connection_node_list})
      end ;
    true ->
      MapE = maps:new(),
      BufferPid ! kill,
      MonitorPid = ok,
      wx_object:cast(gui_nn,{insert_nodes_again , List})
  end
  end,
  {noreply, State#master_state{nodes_Map = MapE, highestScore = HighScore,bufferPid = BufferPid,monitorPid = MonitorPid,inputList = Inputs}};

%% in this case the master received result from population , so if the result is better than the one that already received ,
%% the best fitness ,result digraph  store and forward to buffer that forward it to the gui.
handle_cast({Pop_name,new_gen_hit_me,Result,Processes}, State = #master_state{ highestScore = Score ,nodes_Map = Nodes,bufferPid = BufferPid,inputList = Inputs,processes_Map = Processes_Map}) ->
%%  io:format("master:in in new gen hit me cast from ~p in master with result : ~p ~n" ,[Pop_name,Result]),
  Processes_MapNew = maps:put(Pop_name,Processes,Processes_Map),
  Number_of_processes = lists:sum(maps:values(Processes_MapNew)),
  {NewScore,_,_,_} = Result,
  if
    NewScore < Score ->
      Score2 = NewScore,
     % timer:sleep(2000),
   %   wx_object:cast(gui_nn,{done,Result}) ;
     BufferPid! {done,{Result,Inputs},Number_of_processes};
      %io:format("master:ending result for gui from ~p , the result : ~p ~n" ,[Pop_name,NewScore]);
    true -> Score2 = Score
  end,
  {_Node,Pid} = maps:get(Pop_name,Nodes),
 % io:format("master:in start_insert massage to: ~p with best score: ~p ~n" ,[Pop_name,Score2]),
  gen_statem:cast(Pid,{start_insert,Score2}),
  {noreply, State#master_state{highestScore = Score2}};


%% in this case we get bad result from population so we start the networks again for more results.
handle_cast({Pop_name,worst_result,Processes}, State = #master_state{nodes_Map = Nodes ,highestScore = H ,processes_Map = Processes_Map }) ->
%io:format("master:in in worst_result cast from ~p ,start new iteration!! ~n" ,[Pop_name]),
  Processes_MapNew = maps:put(Pop_name,Processes,Processes_Map),
{_Node,Pid} = maps:get(Pop_name,Nodes),
gen_statem:cast(Pid,{start_insert,H}),
{noreply, State#master_state{processes_Map = Processes_MapNew}};

%% in this case we take care of catch of node failure , remove him from maps.
handle_cast({node_down,Node}, State = #master_state{nodes_Map = Nodes ,processes_Map = Processes_Map }) ->
  io:format("im in the node_down state in master!"),
  Key = find_key_by_valu(maps:to_list(Nodes),Node),
  if
    Key =/= not_found ->
      Nodes2 = maps:remove(Key,Nodes),
      Processes_Map2 = maps:remove(Key,Processes_Map);
    true ->
      Nodes2 = Nodes,
      Processes_Map2 = Processes_Map
  end,
  {noreply, State#master_state{nodes_Map = Nodes2 ,processes_Map = Processes_Map2 }};


%% in this case we get stop from gui , so we kill monitor and buffer processes terminate nodes and update gui..
handle_cast({stop},State = #master_state{nodes_Map = Nodes,bufferPid = BufferPid,monitorPid = MonitorPid}) ->
  io:format("master:im in handle stop "),
  BufferPid ! kill,
  MonitorPid ! kill,
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

handle_info({'EXIT',_PID,normal}, State = #master_state{}) ->
  {noreply, State};

handle_info({'EXIT',_PID,_Reason}, State = #master_state{}) ->
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
terminate(Reason, State = #master_state{nodes_Map = Nodes,bufferPid = BufferPid,monitorPid = MonitorPid}) ->
  try
    BufferPid ! kill of
    _Result->ok
  catch
    _:_->ok
   end,

      try
        MonitorPid ! kill of
        _Result2->ok
      catch
        _:_->ok
   end,

  L = maps:to_list(Nodes),
  stopProcess(L),
  %wx_object:stop(gui_nn),
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

%% this function we start the population module in remote nodes by rpc call .
%% if for some reason the rpc  unsuccessfully to nodes we return list of those nodes .
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
           io:format("wrong rpc cast to node ~p~n",[Address]),
           L2 = BadList ++ [Address],
           startChat(T,Map,L2,Main_PID ,SensorNum ,ActuatorNum,NumOfLayers, NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs)
      end
     catch
         _Reason:_Reason1->
           io:format("catch error in start chat try to rpc node ~p ~n",[_Reason1]),
           startChat(T,Map,BadList ++ [PopulationID],Main_PID ,SensorNum ,ActuatorNum,NumOfLayers, NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs)
  end.



%% this function generate random list of inputs for the networks.
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


%% this function we check the connection to the another nodes by the net_kernel:connect_node ,
%% if for some reason , the connection unsuccessfully to nodes we return list of those nodes .

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

%% this function start the calculation of the networks.
insert_cast([],_)->ok;
insert_cast([{_KEY,{_Node,Pid}}|T], Highest_score) ->
  %io:format("master:start insert from master to ~p ~n" , [KEY]),
  gen_statem:cast(Pid,{start_insert,Highest_score}),
  %'node1@avnido-VirtualBox'
  %Answer=gen_statem:cast({global,KEY},{start_insert}),
  %io:format("master :cast to pop start insert to ~p , Answer is : ~p ~n" , [KEY,Answer]),
  insert_cast(T,Highest_score).

%% this function check and received the number of live processes in the node.
num_of_alive_processes() ->
  L5= processes(),
  L6=[is_process_alive(A)|| A<-L5],
  L7=[true || true<-L6],
  L=length(L7),
  L.



%% this function terminates the population.
stopProcess([])->ok;
stopProcess([{_Name,{_Node,Pid}}|T])->

  try
    gen_statem:stop(Pid) of
    _Result->stopProcess(T)
  catch
    _Reason:_Reason1->stopProcess(T)

  end.

%% create bag in the program for failure check
makeItCrash(N)->
  1/(1-rand:uniform(N)).

%% this monitor loop catch nodes failure and update the master.
monitor_loop(MainPid)->
  net_kernel:monitor_nodes(true),
  io:format("im in the monitor loop ~n"),
  receive
    {Message, Node} ->
      io:format("im in the monitor loop got a message: ~p from ~p  ~n",[Message,Node]),
      gen_server:cast(MainPid,{node_down,Node}),
      wx_object:cast(gui_nn,{node_down,Message,Node}),
      monitor_loop(MainPid);
    kill -> out
  end.

%% buffer of massages for delay and priority the massages that pass to the gui
buffer()->
  receive
    kill->
     gui:all_messages([]),
      io:format("bufferkilld with messages"),
      ok;

    {done,Result,Num_of_processes}-> wx_object:cast(gui_nn,{done,Result,Num_of_processes}),
      io:format("buffer sent result ~n"),
      buffer2()
  end.

buffer2() ->
  receive
    kill->
      gui:all_messages([]),
      io:format("bufferkilld with messages"),
      ok
  after 1500-> buffer()

  end.

%% return Key associated with value
find_key_by_valu([],_)->not_found;
find_key_by_valu([H|T],Value)->
  {Key,Value2} = H,
  if
    Value2 =:= Value -> Key;
    true -> find_key_by_valu(T,Value)
  end.