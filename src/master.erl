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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(master_state, {result_Tuple ,nodes_Map,guiPid,guiName}).

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

  {ok, #master_state{guiName =gui_nn,guiPid = D}}.

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

handle_cast({start,From ,Sensors ,Actuators ,Layers,  Neurons ,AF2 ,NN,Nodes}, State = #master_state{}) ->
  Inputs = generate_inputs(Sensors,[]),
  Map = startChat(Nodes,maps:new(),self() ,Sensors ,Actuators,Layers, Neurons,AF2,NN,Inputs),
  trytoconnect(Nodes),



  %[gen_statem:cast({global,X},{start_insert}) || X <-maps:keys(Nodes) ],
  {noreply, State};

handle_cast({Pop_name,new_gen_hit_me,Result}, State = #master_state{ nodes_Map = Nodes}) ->
  gen_statem:cast(gui2,{Pop_name,result,Result}),
  {noreply, State}.



%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #master_state{}) ->
  {noreply, NewState :: #master_state{}} |
  {noreply, NewState :: #master_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #master_state{}}).
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

startChat([],Map,_,_,_,_,_,_,_,_) ->
  register(master_pid,erlang:self()),%% Save the Pid of the local host process
  Map;

startChat([Address|T],Map,Main_PID ,SensorNum ,ActuatorNum,NumOfLayers, NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs ) ->
  N = maps:size(Map)+1,
  PopulationID = list_to_atom(lists:flatten(io_lib:format("node~p", [N]))),
  M2 = maps:put(PopulationID,Address,Map),
  case whereis(master_pid) of
    undefined ->
      Answer=rpc:call(Address, population, start_link, [Main_PID,SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs,PopulationID]),
      if
        Answer->startChat(T,M2,Main_PID ,SensorNum ,ActuatorNum,NumOfLayers, NumOfNeuronsEachLayer,AF,Num_Of_NN_AGENTS,Inputs);
        true ->  io:format("wrong rpc cast to population")
      end;
    _ -> io:format("You already started a chat~n")
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

trytoconnect([H|T]) ->
  erlang:error(not_implemented).