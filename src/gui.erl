-module(gui).
-behaviour(wx_object).
-import(util,[integer_to_atom/1]).
-export([start/3, init/1, handle_event/2, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2, all_messages/1]).
-include_lib("wx/include/wx.hrl").
-define(SERVER,?MODULE).
-record(state, {clicked, neurons,layers,sensors,actuators,nn,frame,panel,log , main_pid , button , image , node,pic_frame,pic_panel,flag,resTXT,fitTXT,genTXT ,re_insert_nodes_click,width,height,fitnessChoice,resChoices,processTXT,inputTXT}).

start(Node,Name,Pid) ->
  wx_object:start_link({local,Name},?MODULE,[global,Node,Pid],[]).
%%  receive
%%    {loop}->ok
%%  end.


init([Mode,Node,Pid]) ->
  InitState = initiation(Mode,Node,Pid),
  {InitState#state.frame,InitState}.

initiation(_Mode,_Node,Pid) ->
  wx:new(),
  FitnessChoices = ["go to \pi","go to e","fitness func1","fitness func2"], %supported algorithms
  ResolutionChoices = ["500x1000","600x1200","700x1400","800x1600"],

  GParent = wxWindow:new(),
  Parent = wxFrame:new(GParent, 1, "neuroevolution gui" ,[{size,{600, 700}}]), %create frame
  Panel = wxPanel:new(Parent, []), %create panel

  %% Setup sizer
  
  
  FirstNodeSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "insert Nodes (separated by ',')"}]),

  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  ChoicePickerSizerRes = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "Choose simaltion resolution"}]),
  ChoicePickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "Choose activation function algorithm"}]),
  LayersPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "choose number of layers"}]),
  NeuronsPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "choose number of neurons in layer"}]),
  SensorsPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "choose number of sensors"}]),
  ActuatorsPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "choose number of actuators"}]),
  NetworksPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "choose number of networks for each node"}]),
  ButtonPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, " start Neuroevolution calculation"}]),
  LogSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "log"}]),
  Nodes  = wxTextCtrl:new(Panel, 5, [{value, "nonode@nohost"},
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
  Log = wxTextCtrl:new(Panel, 7, [{value, "insert initial values for networs please :) "},
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

  ChoicesSizer = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel),
%%  ChoicesSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
%%    [{label, "insert Nodes (separated by ',')"}]),


  SizerOptions  = [{flag, ?wxEXPAND}],
  wxSizer:add(MainSizer, ChoicePickerSizerRes, [{border, 10},{flag, ?wxTOP bor ?wxEXPAND}]),
  wxSizer:add(MainSizer, ChoicePickerSizer, SizerOptions),
 % wxSizer:add(MainSizer, ChoicesSizer, SizerOptions),
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


     State = #state{
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




handle_event(We=#wx{obj = _Button, event = #wxCommand{type = command_button_clicked}},
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
      re_insert_nodes_click = Nodes_Click,
      clicked=Click,
      pic_frame = PrevFrame,
      flag=run}) ->
      L3=string:split(wxTextCtrl:getValue(Node),",",all),
      Nodes_List=[list_to_atom(A)||A<-L3],
      Nodes_List2_for_dibug = [nonode@nohost],  %todo delete thid


  if

          Click=:=0 -> %check if the previous run was completed and the file is valid
            Deleted_List=deleteResults([]),
            Flag2=run,
          Neurons = wxSpinCtrl:getValue(NeuronsP), %get the Neurons
          Layers = wxSpinCtrl:getValue(LayersP), %get the Layers
          Sensors = wxSpinCtrl:getValue(SensorsP), %get the Sensors
          Actuators = wxSpinCtrl:getValue(ActuatorsP), %get the Actuators
           NN = wxSpinCtrl:getValue(NNP), %get the nn number

           Fitness=chooseFitness(FitChoice),
            {Height,Width}=chooseRes(ResChoice),









          %io:format("i'm in the start gui case ~n ",[]),
          A=gen_server:cast(Pid,{start,Sensors ,Actuators ,Layers,  Neurons ,Fitness ,NN,Nodes_List}),%todo change to nodes list
   %      {Pimaind ! {start,self() ,Sensors ,Actuators ,Layers,  Neurons ,AF2 ,NN}},
          wxTextCtrl:changeValue(Log, ""),
        %run_nn(self()  ,Sensors  ,Actuators  ,Layers ,  Neurons   ,AF  ,Num_Of_NN_AGENTS ,Inputs,PopulationID),
         Click2 = 1 ,
            wxButton:setLabel(B,"building neural network"),
            wxButton:disable(B),
            wxPanel:refresh(Parent), %refresh the panel

            if
              {H,W}=/={Height,Width}->
                if
                  H>0 ->wxFrame:destroy(PrevFrame) ;
                  true -> ok
                end
                ,
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




handle_event(W=#wx{event = #wxClose{},obj = {_,FrameID,_,_}}, %close window event, handle memory leak
    State = #state{ frame=Frame={_,FrameID,_,_} , panel=_Panel ,pic_frame  = Frame2,main_pid = Pid}) ->
  gen_server:stop(Pid),
  wxFrame:destroy(Frame),
  if
    Frame2=/=undefined->  wxFrame:destroy(Frame2);
    true ->ok
  end,

  io:format("exit~n"),
  {stop,normal,State};



handle_event(W=#wx{event = #wxClose{},obj = {_,FrameID,_,_}}, %close window event, handle memory leak
    State = #state{ panel=_Panel ,pic_frame  = {_,FrameID,_,_}}) ->
%%  M = wxMessageDialog:new(wx:null(),"please stop the simulation by using the main menu button"),
%%  wxMessageDialog:showModal(M),
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



%%#wx{obj = _Button, event = #wxCommand{type = command_button_clicked}

handle_cast({done,{Outputs,InPuts},Num_of_processes} ,State = #state{frame = Frame,log = Log , flag = Flag,button = B , fitTXT = FitTXT , resTXT = ResultTxT , genTXT = GenTxT ,
  pic_frame = Frame2, width = W,height =H,inputTXT = InputTXT,processTXT = ProcessesTXT }) ->
% X=#wx{obj = _Button, event = _Type},
  if
      Flag =:= run ->
      {Fitness,Result,G,Generation} = Outputs,
      Result1=shortcut(Result,[]),
        Input1=shortcut(InPuts,[]),

        try
        toGraph:generateGraph(G) of
          _No_error->

            toGraph:replaceImage(State#state.pic_panel,W,H),
            %wxTextCtrl:changeValue(Log, ""), %clean the log
             wxTextCtrl:changeValue(FitTXT, lists:flatten(io_lib:format("~p", [Fitness]))),
            wxTextCtrl:changeValue(ResultTxT, lists:flatten(io_lib:format("~p", [Result1]))),
            wxTextCtrl:changeValue(GenTxT, lists:flatten(io_lib:format("~p", [Generation]))),
            wxTextCtrl:changeValue(ProcessesTXT, lists:flatten(io_lib:format("~p", [Num_of_processes]))),
            wxTextCtrl:changeValue(InputTXT, lists:flatten(io_lib:format("~p", [Input1]))),
            %wxTextCtrl:writeText(FitTXT, lists:flatten(io_lib:format("~p", [Fitness]))),
            wxTextCtrl:changeValue(Log,"network in simulation"),
            wxButton:setLabel(B,"press to terminate network"),
            wxButton:enable(B),
            wxPanel:refresh(Frame), %refresh the panel
            wxPanel:refresh(FitTXT),
            wxPanel:refresh(ResultTxT),
            wxPanel:refresh(GenTxT),
            wxPanel:refresh(InputTXT),
            wxPanel:refresh(ProcessesTXT)
         %   timer:sleep(2000)
        catch
          _Reason:_Reason1-> io:format("gui: error get to G because he located in another node ~n but the Fitness is ~p , and network generation is ~p ~n",[Fitness,Generation])

        end;
        true ->
           L=deleteResults([])

  end,
  io:format("gui: end of done state in gui with fitness ~p~n",[element(1,Outputs)]),
  {noreply,State#state{}};

%%handle_cast({result,Outputs} ,State = #state{frame = Frame,log = Log,main_pid = Pid,image = W}) ->
%%  {Fitness,Result,G} = Outputs,
%%  Result1=shortcut(Result,[]),
%%  wxTextCtrl:changeValue(Log, ""), %clean the log
%%  wxTextCtrl:changeValue(Log, "you can start again with different values"),
%%  wxPanel:refresh(Frame), %refresh the panel
%%  run3(W),
%%  {Pid ! {terminate}},
%%  {noreply,State#state{clicked=0}};



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

handle_cast({insert_nodes_again , List_of_bad_nodes}, State = #state{frame = Parent ,log = Log ,button = B ,node = Nodes }) ->
  M = wxMessageDialog:new(wx:null(), lists:flatten(io_lib:format(" connections error with nodes: ~p", [List_of_bad_nodes])) ),
  wxMessageDialog:showModal(M),
  wxTextCtrl:writeText(Log,"try to connect agin "),
  wxTextCtrl:changeValue(Nodes,""),
  wxButton:enable(B),
  wxButton:setLabel(B,"press to insert new nodes"),
  wxTextCtrl:changeValue(Nodes,""),
  wxPanel:refresh(Parent), %refresh the panel ;;
  {noreply,State#state{clicked =0,flag=run}};


handle_cast({node_down,Message,Node}, State = #state{frame=Parent}) ->
  io:format("im in node down case in the gui!!"),
  M = wxMessageDialog:new(wx:null(), lists:flatten(io_lib:format(" what a catch!!!!!!! ~n amazing monitor created, message : ~p from Node: ~p down. ~n", [Message,Node]))),
  wxMessageDialog:showModal(M),
  wxPanel:refresh(Parent), %refresh the panel
  {noreply,State#state{}};








handle_cast(_Msg, State) ->

  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _) ->
  io:format("gui terminated"),
  ok.



to_file(Graph, File, Format) ->
  {A1,A2,A3} = now(),
  DotFile = lists:concat([File, ".dot-", A1, "-", A2, "-", A3]),
  to_dot(Graph, DotFile),
  DotCommant = lists:concat(["dot -T", Format, " -o", File, " ", DotFile]),
  X=os:cmd(DotCommant),
  X.

%file:delete(DotFile).

to_dot(Graph, File) ->
  {GraphId, Type, GraphOptions, Nodes, Edges} = Graph,
  {GraphType, EdgeType} = Type,

  % open file
  {ok, IODevice} = file:open(File, [write]),

  % print graph
  io:format(IODevice, "~s ~s {~n", [GraphType, GraphId]),

  % print nodes
  lists:foreach(
    fun(Node) ->
      io:format(IODevice, "  ~s;~n",[Node])
    end,
    Nodes
  ),

  % print edges
  lists:foreach(
    fun(Edge) ->
      {NodeOne, NodeTwo} = Edge,
      io:format(IODevice, "  ~s ~s ~s;~n",[NodeOne, EdgeType, NodeTwo])
    end,
    Edges
  ),

  % close file
  io:format(IODevice, "}~n", []),
  file:close(IODevice).

getEdgesList(G)->
  B=digraph:edges(G),
  [digraph:edge(G,E) || E <- B].



redraw(Image, #wx{obj=Panel}) ->
  DC = wxPaintDC:new(Panel),
  wxDC:drawBitmap(DC,Image,{0,0}).

shortcut([],L)->L;
shortcut([H|T],L)->
  if
      H=:=0 ->
        round(H),
      L2 = L ++ [H],
      shortcut(T,L2) ;
    true ->
      H_Float= float(H),
      L2= L ++ [list_to_float(float_to_list(H_Float,[{decimals,4}]))],
      shortcut(T,L2)
  end.


ping_sm([])->[];
ping_sm([H|T] ) ->
  Length = length(lists:flatten(H)),
  if
    Length > 1 -> [ping_it(H)]++ ping_sm(T);
    true -> ping_sm(T)
  end.
ping_it(NodeAd)->
  io:format("send ping to : ~p", [NodeAd]),
  Result = net_adm:ping(list_to_atom(NodeAd)),

  if
    Result==pong -> list_to_atom(NodeAd);
    true -> []
  end.


all_messages(Messages) ->
  receive
    AnyMessage ->
      all_messages( [AnyMessage|Messages])
  after 0 ->
    lists:reverse(Messages)
  end.

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

chooseFitness(FitChoice) ->
    X= wxListBox:isSelected(FitChoice,0),
    Y=   wxListBox:isSelected(FitChoice,1),
    Z=   wxListBox:isSelected(FitChoice,2),
    W=    wxListBox:isSelected(FitChoice,3),

  if
      X=:=true->go_to_pi;
      Y =:=true ->go_to_e;
      Z=:=true ->go_to_pi;
      W=:=true->go_to_pi;
      true->go_to_pi

  end.

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