%%%-------------------------------------------------------------------
%%% @author chass
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2020 15:55
%%%-------------------------------------------------------------------
-module(neuron2).
-author("chass").

-behaviour(gen_statem).

%% API
-export([start_link/3, initialize/3, idle/3, in_process/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

-define(SERVER, ?MODULE).

-record(neuron2_state, {id,kind,manger_pid,af,inputPIDs,inputPIDs2,outputPIds,bias,acc}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.


start_link(MasterPID,ID,KIND) ->
  {_A,B} =gen_statem:start_link({local, ID}, ?MODULE, {MasterPID,ID,KIND}, []),
B.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init({MasterPID,ID,KIND}) ->
  {ok, initialize, #neuron2_state{id=ID,manger_pid = MasterPID,kind=KIND}}.

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
state_name(_EventType, _EventContent, State = #neuron2_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

%%initialize state,the neuron wait for info from nn_agent
initialize({call,From}, {NN_manger_PID,{Id,AF,Input_PIdPs,Output_PIds,Bias}}, State = #neuron2_state{}) ->
  NextStateName = idle,
  {next_state, NextStateName, State#neuron2_state{manger_pid = NN_manger_PID,outputPIds = Output_PIds, id=Id,af=AF,acc=0,inputPIDs2 =maps:from_list(Input_PIdPs) ,inputPIDs =maps:from_list(Input_PIdPs),bias = Bias},[{reply,From,ok}]};



initialize(_Message,_Type,State=#neuron2_state{})->
  {keep_state, State#neuron2_state{}}.



%% idle state when its sensor, wait for input
idle(cast,{insert_input,_NN_manger_PID,W},State=#neuron2_state{kind=sensor})->
  Out_PIds=State#neuron2_state.outputPIds,
  [gen_statem:cast(Pid,{State#neuron2_state.id,forward,W}) || Pid <- Out_PIds],
  {keep_state, State#neuron2_state{acc=W}};



%% idle state when its actuator or neuron, wait for input
idle(cast,{From,forward,Input},State=#neuron2_state{})->
  In_PIds=State#neuron2_state.inputPIDs2,
  Acc=State#neuron2_state.acc,
  ToAdd=maps:get(From,In_PIds)*Input,
  Map2=maps:remove(From,In_PIds),
  case maps:size(Map2) =:= 0 of
    true ->
      Bias=State#neuron2_state.bias,
      Result= af(Acc+ToAdd+Bias,State#neuron2_state.af),
      Out_PIds=State#neuron2_state.outputPIds,
      K=State#neuron2_state.kind,
      case K of
        neuron->       [gen_statem:cast(Pid,{State#neuron2_state.id,forward,Result}) || Pid <- Out_PIds];
        actuator->
          PIDmanger=State#neuron2_state.manger_pid,
          gen_statem:cast(PIDmanger,{State#neuron2_state.id,result,Result});
        sensor-> ok

      end,
      {keep_state, State#neuron2_state{acc=0,inputPIDs2 = State#neuron2_state.inputPIDs }};
    false->{next_state,in_process,State#neuron2_state{acc=Acc+ToAdd,inputPIDs2 = Map2}}
  end;


idle(EventType, EventContent, Data) ->
  handle_common(EventType, EventContent, Data).




%%the neuron in the middle of processing info state
in_process(cast,{From,forward,Input},State=#neuron2_state{})->
  In_PIds=State#neuron2_state.inputPIDs2,
  Acc=State#neuron2_state.acc,
  ToAdd=maps:get(From,In_PIds)*Input,
  Map2=maps:remove(From,In_PIds),
  case maps:size(Map2) =:= 0 of
    true ->
      Bias=State#neuron2_state.bias,
      Result= af(Acc+ToAdd+Bias,State#neuron2_state.af),
      Out_PIds=State#neuron2_state.outputPIds,
      K=State#neuron2_state.kind,
      case K of
        neuron->       [gen_statem:cast(Pid,{State#neuron2_state.id,forward,Result}) || Pid <- Out_PIds];
        actuator->
          PIDmanger=State#neuron2_state.manger_pid,
          gen_statem:cast(PIDmanger,{State#neuron2_state.id,result,Result});
        sensor-> ok

      end,
      {next_state,idle, State#neuron2_state{acc=0,inputPIDs2 = State#neuron2_state.inputPIDs }};
    false->{keep_state,State#neuron2_state{acc=Acc+ToAdd,inputPIDs2 = Map2}}
  end;

in_process(EventType, EventContent, Data) ->
  handle_common(EventType, EventContent, Data).


%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #neuron2_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #neuron2_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #neuron2_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



%%neuron commit calculate according to its activation func
af(AccToAdd,AF) ->
case AF of

 relu-> case AccToAdd > 0 of
    true -> AccToAdd;
    false -> 0.01*AccToAdd
          end;
  sin->math:sin(AccToAdd);
  cos->math:cos(AccToAdd);
 tanh->math:tanh(AccToAdd);
 bin->case AccToAdd > 0 of
        true->1;
        false->0
      end

end.



 %th:cos(AccToAdd).


%% neuron reset
handle_common(cast, {_XFrom,reset},State = #neuron2_state{}) ->
  deleteResults([]),
  {next_state,idle, State#neuron2_state{acc=0,inputPIDs2 = State#neuron2_state.inputPIDs}};

%% neuron reset
handle_common({call,From}, {_XFrom,reset},State = #neuron2_state{}) ->
  deleteResults([]),
  {next_state,idle, State#neuron2_state{acc=0,inputPIDs2 = State#neuron2_state.inputPIDs},[{reply,From,ok}]};


%% neuron update new out edge
handle_common({call,From}, {_XFrom,new_neighbour_out,ID},State = #neuron2_state{}) ->
  L=State#neuron2_state.outputPIds,
 L2= L++[ID],
  {next_state,idle, State#neuron2_state{outputPIds = L2},[{reply,From,neighbour_out_added}]};


%% neuron update new in edge
handle_common({call,From}, {_XFrom,new_neighbour_in,{ID,Weight}},State = #neuron2_state{}) ->
  Map=State#neuron2_state.inputPIDs,
  Map2=maps:put(ID,Weight,Map),
  {next_state,idle, State#neuron2_state{inputPIDs = Map2,inputPIDs2 = Map2},[{reply,From,neighbour_in_added}]};


%% neuron update new bias
handle_common({call,From}, {_XFrom,newBias,NewBias},State = #neuron2_state{}) ->
  {next_state,idle, State#neuron2_state{bias = NewBias},[{reply,From,bias_changed}]};

%% neuron update new weight in some edge
handle_common({call,From}, {_XFrom,newWeight,{ID,Weight}},State = #neuron2_state{}) ->
  Map=State#neuron2_state.inputPIDs,
  Map2=maps:put(ID,Weight,Map),
  {next_state,idle, State#neuron2_state{inputPIDs = Map2,inputPIDs2 = Map2},[{reply,From,weight_added}]};



%% neuron update new activation  func
handle_common({call,From}, {_XFrom,newAF,NewAF},State = #neuron2_state{}) ->
  {next_state,idle, State#neuron2_state{af = NewAF},[{reply,From,af_changed}]}.


%% flush mail box while reset
deleteResults(L)->
  receive
    {_X,{From,forward,Result}}->deleteResults(L++[{From,result,Result}])

  after 0->L
  end.