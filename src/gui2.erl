%%%-------------------------------------------------------------------
%%% @author DOR
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2020 3:14 PM
%%%-------------------------------------------------------------------
-module(gui2).
-author("DOR").

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0,wait/3]).

-define(SERVER, ?MODULE).

-record(gui2_state, {nodes,nodes2 ,result , master_pid}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([]) ->
  L=['node1@DESKTOP-V723826'],
  {_,Pid} = gen_server:start_link(master,[],[]),
  click_event(L,Pid),
  {ok, wait, #gui2_state{ nodes = L , result = [] ,master_pid = Pid}}.



wait(cast,{Pop_name,result,Result},State = #gui2_state{ nodes =Nodes , nodes2 = L ,result = R ,master_pid = Pid })->
  Length = length(L) -1,
  if
   Length =:= 0  ->
     R2 = R ++ [{Pop_name,Result}],
     L2 = Nodes,
     io:format("result from master ~p ~n",[R2]),
     gen_server:cast(Pid,{start});

    true -> L2 = tl(L)
end,
  {next_state, wait, State#gui2_state{nodes2 = L2 , result = []}}.




%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  handle_event_function.

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
state_name(_EventType, _EventContent, State = #gui2_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #gui2_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #gui2_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #gui2_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
click_event(Nodes,Pid) ->
  Sensors = 3,Actuators = 5,Layers =4,  Neurons =3  ,AF = sin ,NN = 10,
  Answer = gen_server:call(Pid,{init,Sensors ,Actuators ,Layers,  Neurons ,AF ,NN,Nodes}),
  if
    Answer =:= ok -> gen_server:cast(master,{start}) ;
    true -> io:format("wrong init for master")
  end.