%%%-------------------------------------------------------------------
%%% @author hananel assor, dor avni
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2020 15:55
%%%-------------------------------------------------------------------
-module(gui).
-behaviour(wx_object).
-import(util,[integer_to_atom/1]).
-export([start/3, init/1, handle_event/2, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2, all_messages/1, ping_sm/1]).
-include_lib("wx/include/wx.hrl").
-define(SERVER,?MODULE).
-record(state, {clicked, neurons,layers,sensors,actuators,nn,frame,panel,log , main_pid , button , image , node,pic_frame,pic_panel,flag,resTXT,fitTXT,genTXT ,re_insert_nodes_click,width,height,fitnessChoice,resChoices,processTXT,inputTXT}).

start(Node,Name,Pid) ->
  wx_object:start_link({local,Name},?MODULE,[global,Node,Pid],[]).


init([Mode,Node,Pid]) ->
  InitState = initiation(Mode,Node,Pid),
  {InitState#state.frame,InitState}.

initiation(_Mode,_Node,Pid) ->
  wx:new(),
  FitnessChoices = ["results approaches \pi","results approaches e","results approaches fibonacci sequence","results average approaches inputs average"], %supported algorithms
  ResolutionChoices = ["500x1000","600x1200","700x1400","800x1600"],

  GParent = wxWindow:new(),
  Parent = wxFrame:new(GParent, 1, "neuroevolution gui" ,[{size,{600, 700}}]), %create frame
  Panel = wxPanel:new(Parent, []), %create panel

  %% Setup sizer
  FirstNodeSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "insert Nodes (separated by ',')"}]),

  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  ChoicePickerSizerRes = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "Choose simulation resolution"}]),
  ChoicePickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "Choose fitness function algorithm"}]),
  LayersPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "choose number of layers"}]),
  NeuronsPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "choose number of neurons each layer"}]),
  SensorsPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "choose number of sensors"}]),
  ActuatorsPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "choose number of actuators"}]),
  NetworksPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "choose number of networks for each node"}]),
  ButtonPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "start Neuroevolution calculation"}]),
  LogSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "log"}]),
  Nodes  = wxTextCtrl:new(Panel, 5, [{value, ""},
    {style, ?wxDEFAULT}]),



  Choice = wxListBox:new(Panel, 7, [{choices, FitnessChoices}]),
  Choice2 = wxListBox:new(Panel, 7, [{choices, ResolutionChoices}]),
  LayersPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(LayersPicker, 2, 1000),
  NeuronsPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(NeuronsPicker, 2, 1000),
  SensorsPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(SensorsPicker, 2, 1000),
  ActuatorsPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(ActuatorsPicker, 2, 1000),
  NetworksPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(NetworksPicker, 2, 1000),
  ButtonPicker = wxButton:new(Panel, 10, [{label, "press to start"}]),
  Log = wxTextCtrl:new(Panel, 7, [{value, "insert initial values for networks please :) "},
    {style, ?wxDEFAULT}]),


  wxFrame:connect(Parent,close_window), %connect between closing the window and the handler
  wxButton:connect(ButtonPicker,command_button_clicked), %connect between start click and the handler



  %% Add to sizer
  PickerOptions = [{border, 1},{flag, ?wxALL bor ?wxEXPAND}],

  wxSizer:add(LayersPickerSizer, LayersPicker, PickerOptions),
  wxSizer:add(NeuronsPickerSizer, NeuronsPicker, PickerOptions),
  wxSizer:add(SensorsPickerSizer, SensorsPicker, PickerOptions),
  wxSizer:add(ActuatorsPickerSizer, ActuatorsPicker, PickerOptions),
  wxSizer:add(NetworksPickerSizer, NetworksPicker, PickerOptions),
  wxSizer:add(ButtonPickerSizer, ButtonPicker, PickerOptions),
  wxSizer:add(LogSizer, Log, PickerOptions),
  wxSizer:add(FirstNodeSizer, Nodes, PickerOptions),
  wxSizer:add(ChoicePickerSizer, Choice, PickerOptions),
  wxSizer:add(ChoicePickerSizerRes, Choice2, PickerOptions),



  SizerOptions  = [{flag, ?wxEXPAND}],
  wxSizer:add(MainSizer, ChoicePickerSizerRes, [{border, 10},{flag, ?wxTOP bor ?wxEXPAND}]),
  wxSizer:add(MainSizer, ChoicePickerSizer, SizerOptions),
  wxSizer:add(MainSizer, LayersPickerSizer, SizerOptions),
  wxSizer:add(MainSizer, NeuronsPickerSizer, SizerOptions),
  wxSizer:add(MainSizer, SensorsPickerSizer, SizerOptions),
  wxSizer:add(MainSizer, ActuatorsPickerSizer, SizerOptions),
  wxSizer:add(MainSizer, NetworksPickerSizer, SizerOptions),
  wxSizer:add(MainSizer, ButtonPickerSizer, SizerOptions),
  wxSizer:add(MainSizer, LogSizer, SizerOptions),
  wxSizer:add(MainSizer, FirstNodeSizer, SizerOptions),
  wxPanel:setSizer(Panel, MainSizer),
   wxFrame:show(Parent),

   W= wx:new(),


      #state{
       clicked=0,
       flag = run,
       re_insert_nodes_click = false,
       fitnessChoice =Choice,
       resChoices = Choice2,
       neurons=NeuronsPicker,
       layers=LayersPicker,
       sensors=SensorsPicker,
       actuators=ActuatorsPicker,
       nn = NetworksPicker,
       frame=Parent,
       log=Log,

       button = ButtonPicker,
       image = W,
       node = Nodes,
       panel=Panel,
     main_pid = Pid,
       height = 0,
     width = 0}.



%%handle 'start' event
handle_event(#wx{obj = _Button, event = #wxCommand{type = command_button_clicked}},
    State = #state{
      fitnessChoice =FitChoice,
      resChoices = ResChoice,
      neurons=NeuronsP,height = H,width = W,
      layers=LayersP,
      sensors=SensorsP,
      actuators=ActuatorsP,
      nn = NNP,
      log=Log,
      frame=Parent,
      panel=_Panel,
      button = B,
      main_pid = Pid,
      node = Node,
      clicked=Click,
      pic_frame = PrevFrame,
      flag=run}) ->
      L3=string:split(wxTextCtrl:getValue(Node),",",all),
      Nodes_List=[list_to_atom(A)||A<-L3],


  if

          Click=:=0 -> %check if the previous run was completed and the file is valid
            deleteResults([]),
            Flag2=run,
          Neurons = wxSpinCtrl:getValue(NeuronsP), %get the Neurons
          Layers = wxSpinCtrl:getValue(LayersP), %get the Layers
          Sensors = wxSpinCtrl:getValue(SensorsP), %get the Sensors
          Actuators = wxSpinCtrl:getValue(ActuatorsP), %get the Actuators
           NN = wxSpinCtrl:getValue(NNP), %get the nn number
            Fitness=chooseFitness(FitChoice),
            {Height,Width}=chooseRes(ResChoice),
          gen_server:cast(Pid,{start,Sensors ,Actuators ,Layers,  Neurons ,Fitness ,NN,Nodes_List}),
          wxTextCtrl:changeValue(Log, ""),
         Click2 = 1 ,
            wxButton:setLabel(B,"building neural networks"),
            wxButton:disable(B),
            wxPanel:refresh(Parent), %refresh the panel
            if
              {H,W}=/={Height,Width}->
                if
                  H>0 ->wxFrame:destroy(PrevFrame) ;
                  true -> ok
                end,
                {Frame2,Panel2,ResTXT,FitTXT,GenTXT,ProcessesTXT,InpTXT}=toGraph:createFrame(Width,Height),
                wxFrame:connect(Frame2,close_window),
                {noreply, State#state{clicked=Click2 , flag = Flag2,resTXT =  ResTXT, fitTXT = FitTXT,genTXT = GenTXT, pic_frame = Frame2, pic_panel = Panel2,width = Width, height  = Height
                ,inputTXT = InpTXT,processTXT = ProcessesTXT}};
              true-> {noreply, State#state{clicked=Click2 , flag = Flag2}}
            end ;
    true ->
      Flag2 = stop,
      wxButton:disable(B),
      gen_server:cast(Pid,{stop}),
      Click2 = 0 ,
      terminating_func(3,Parent,B),
      wxButton:setLabel(B,"please wait"),
      wxTextCtrl:changeValue(Log, "closing simulation"),
      wxPanel:refresh(Parent), %refresh the panel
      {noreply, State#state{clicked=Click2 , flag = Flag2}}
  end;



%%exit event of the main window
handle_event(#wx{event = #wxClose{},obj = {_,FrameID,_,_}}, %close window event, handle memory leak
    State = #state{ frame=Frame={_,FrameID,_,_} , panel=_Panel ,pic_frame  = Frame2,main_pid = Pid}) ->
  gen_server:stop(Pid),
  wxFrame:destroy(Frame),
  if
    Frame2=/=undefined->  wxFrame:destroy(Frame2);
    true ->ok
  end,

  io:format("exit button pressed : gui closed ~n"),
  {stop,normal,State};


%%exit event of the simulation window window
handle_event(#wx{event = #wxClose{},obj = {_,FrameID,_,_}}, %close window event, handle memory leak
    State = #state{ panel=_Panel ,pic_frame  = {_,FrameID,_,_}}) ->
  {noreply,State};


handle_event(_Ev = #wx{}, State = #state{}) ->
  io:format("Got Event ~n"),
  {noreply, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply, State}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply,Reply,State}.


%% got results from master
handle_cast({done,{Outputs,InPuts}} ,State = #state{frame = Frame,log = Log , flag = Flag,button = B , fitTXT = FitTXT , resTXT = ResultTxT , genTXT = GenTxT ,
  width = W,height =H,inputTXT = InputTXT}) ->
  if
      Flag =:= run ->
      {Fitness,Result,G,Generation} = Outputs,
      Result1=shortcut(Result,[]),
        Input1=shortcut(InPuts,[]),

        try
        toGraph:generateGraph(G) of
          _No_error->
            toGraph:replaceImage(State#state.pic_panel,W,H),
             wxTextCtrl:changeValue(FitTXT, lists:flatten(io_lib:format("~p", [Fitness]))),
            wxTextCtrl:changeValue(ResultTxT, lists:flatten(io_lib:format("~p", [Result1]))),
            wxTextCtrl:changeValue(GenTxT, lists:flatten(io_lib:format("~p", [Generation]))),
            wxTextCtrl:changeValue(InputTXT, lists:flatten(io_lib:format("~p", [Input1]))),
            wxTextCtrl:changeValue(Log,"network in simulation"),
            wxButton:setLabel(B,"press to terminate network"),
            wxButton:enable(B),
            wxPanel:refresh(Frame), %refresh the panel
            wxPanel:refresh(FitTXT),
            wxPanel:refresh(ResultTxT),
            wxPanel:refresh(GenTxT),
            wxPanel:refresh(InputTXT)
        catch
          _Reason:_Reason1-> io:format("gui: error get to G because he located in another node ~n but the Fitness is ~p , and network generation is ~p ~n",[Fitness,Generation])
         end;
        true ->
           deleteResults([])

  end,
  io:format("new best fitness;  score :  ~p~n",[element(1,Outputs)]),
  {noreply,State#state{}};




handle_cast({finish_terminate}, State = #state{
  log=Log,
  frame=Parent,
  panel=_Panel,
  button = B,
  flag=stop}) ->
  wxButton:setLabel(B,"start"),
  wxButton:enable(B),
  wxTextCtrl:changeValue(Log, "you can start again with different values"),
  wxPanel:refresh(Parent), %refresh the panel
  {noreply,State#state{flag=run}};


%% connection problems
handle_cast({insert_nodes_again , List_of_bad_nodes}, State = #state{frame = Parent ,log = Log ,button = B ,node = Nodes }) ->
  M = wxMessageDialog:new(wx:null(), lists:flatten(io_lib:format(" connections error with nodes: ~p", [List_of_bad_nodes])) ),
  wxMessageDialog:showModal(M),
  wxTextCtrl:writeText(Log,"try to connect again "),
  wxTextCtrl:changeValue(Nodes,""),
  wxButton:enable(B),
  wxButton:setLabel(B,"press to insert new nodes"),
  wxTextCtrl:changeValue(Nodes,""),
  wxPanel:refresh(Parent), %refresh the panel ;;
  {noreply,State#state{clicked =0,flag=run}};



%% node down
handle_cast({node_down,_Message,Node}, State = #state{}) ->
  io:format("Node ~p down; the the program still running~n",[Node]),
  {noreply,State#state{}};



%% new number of processes
handle_cast({newProcessesNumber,Number_of_processes}, State = #state{processTXT =ProcessesTXT})->
  wxTextCtrl:changeValue(ProcessesTXT, lists:flatten(io_lib:format("~p", [Number_of_processes]))),
  wxPanel:refresh(ProcessesTXT),
{noreply,State#state{}};


handle_cast(_Msg, State) ->
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _) ->
  io:format("gui terminated ~n"),
  ok.




%%shortcut list
shortcut([],L)->L;
shortcut([H|T],L)->
  if
      H=:=0 ->
      L2 = L ++ [H],
      shortcut(T,L2) ;
    true ->
      H_Float= float(H),
      L2= L ++ [list_to_float(float_to_list(H_Float,[{decimals,4}]))],
      shortcut(T,L2)
  end.

%%check ping results
ping_sm([])->[];
ping_sm([H|T] ) ->
  Length = length(lists:flatten(H)),
  if
    Length > 1 -> [ping_it(H)]++ ping_sm(T);
    true -> ping_sm(T)
  end.
ping_it(NodeAd)->
  io:format("send ping to : ~p ~n", [NodeAd]),
  Result = net_adm:ping(list_to_atom(NodeAd)),

  if
    Result==pong -> list_to_atom(NodeAd);
    true -> []
  end.

%%delete messages
all_messages(Messages) ->
  receive
    AnyMessage ->
      all_messages( [AnyMessage|Messages])
  after 0 ->
    lists:reverse(Messages)
  end.


%%delete results
deleteResults(L)->
  receive
    {done,{Fitness,Result,G}}->deleteResults(L++[{done,{Fitness,Result,G}}])
  after 0->L
  end.



terminating_func(0,_,_)->ok;
terminating_func(N,Parent,B) ->
  deleteResults([]),
  wxButton:setLabel(B, lists:flatten(io_lib:format("terminating in ----> ~p ", [N]))),
  %wxButton:setLabel(B,"terminating"),
  wxPanel:refresh(Parent), %refresh the panel
  timer:sleep(1000),
  terminating_func(N-1,Parent,B).


%% choose fitness from box
chooseFitness(FitChoice) ->
    X= wxListBox:isSelected(FitChoice,0),
    Y=   wxListBox:isSelected(FitChoice,1),
    Z=   wxListBox:isSelected(FitChoice,2),
    W=    wxListBox:isSelected(FitChoice,3),

  if
      X=:=true->go_to_pi;
      Y =:=true ->go_to_e;
      Z=:=true ->fibonacci;
      W=:=true->avg;
      true->go_to_pi

  end.

%% choose resolution from box
chooseRes(ResChoice) ->
  X=   wxListBox:isSelected(ResChoice,0),
  Y=   wxListBox:isSelected(ResChoice,1),
  Z=   wxListBox:isSelected(ResChoice,2),
  W=   wxListBox:isSelected(ResChoice,3),

  if
    X=:=true-> {500,1000};
    Y =:=true ->{600,1200};
    Z=:=true ->{700,1400};
    W=:=true->{800,1600};
    true->{700,1400}

  end.