-module(gui).
-behaviour(wx_object).
-import(util,[integer_to_atom/1]).
-export([start/3, init/1,handle_event/2,handle_info/2, handle_call/3,handle_cast/2,code_change/3,terminate/2]).
-include_lib("wx/include/wx.hrl").
-define(SERVER,?MODULE).
-record(state, {clicked, activation_function, neurons,layers,sensors,actuators,nn,frame,panel,log , main_pid , button , image , node,pic_frame,pic_panel,flag,resTXT,fitTXT,genTXT ,re_insert_nodes_click}).

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
  Choices = ["ReLU","tanh","Binary step","Sin"], %supported algorithms
  GParent = wxWindow:new(),
  Parent = wxFrame:new(GParent, 1, "neuroevolution gui" ,[{size,{600, 600}}]), %create frame
  Panel = wxPanel:new(Parent, []), %create panel

  %% Setup sizer
  FirstNodeSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "insert Nodes (separated by ',')"}]),

  MainSizer = wxBoxSizer:new(?wxVERTICAL),
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
  Nodes  = wxTextCtrl:new(Panel, 5, [{value, "node1@avnido-VirtulBox"},
    {style, ?wxDEFAULT}]),



  Choice = wxListBox:new(Panel, 7, [{choices, Choices}]),
  LayersPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(LayersPicker, 2, 1000),
  NeuronsPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(NeuronsPicker, 2, 1000),
  SensorsPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(SensorsPicker, 2, 1000),
  ActuatorsPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(ActuatorsPicker, 2, 1000),
  NetworksPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(NetworksPicker, 2, 250),
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


  SizerOptions  = [{flag, ?wxEXPAND}],
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
  {Frame2,Panel2,ResTXT,FitTXT,GenTXT}=toGraph:createFrame(),
   W= wx:new(),


     State = #state{
       clicked=0,
       flag = run,
       re_insert_nodes_click = false,
       activation_function=Choice,
       neurons=NeuronsPicker,
       layers=LayersPicker,
       sensors=SensorsPicker,
       actuators=ActuatorsPicker,
       nn = NetworksPicker,
       frame=Parent,
       log=Log,
       resTXT =  ResTXT,
       fitTXT = FitTXT,
       genTXT = GenTXT,
       button = ButtonPicker,
       image = W,
       node = Nodes,
       panel=Panel,
     main_pid = Pid,
       pic_frame = Frame2,
       pic_panel = Panel2}.



%%handle 'start' event
handle_event(#wx{obj = _Button, event = #wxCommand{type = command_button_clicked}},
    State = #state{
      activation_function=Choice,
      neurons=NeuronsP,
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
         ReLU=wxListBox:isSelected(Choice,0), %check if the user chose ReLU as the algorithm
         Tanh =wxListBox:isSelected(Choice,1), %check if the user chose Tanh as the algorithm
         Binary_step =wxListBox:isSelected(Choice,2), %check if the user chose Binary_step as the algorithm
         Sin =wxListBox:isSelected(Choice,3), %check if the user chose Sigmoid as the algorithm
          AF = coosen_AF(ReLU,Tanh,Binary_step,Sin),

          if
            AF =:= empty -> AF2 = relu;
            true -> AF2 = AF
          end,
          %io:format("i'm in the start gui case ~n ",[]),
          A=gen_server:cast(Pid,{start,Sensors ,Actuators ,Layers,  Neurons ,AF2 ,NN,Nodes_List}),%todo change to nodes list
   %      {Pimaind ! {start,self() ,Sensors ,Actuators ,Layers,  Neurons ,AF2 ,NN}},
          wxTextCtrl:changeValue(Log, ""),
        %run_nn(self()  ,Sensors  ,Actuators  ,Layers ,  Neurons   ,AF  ,Num_Of_NN_AGENTS ,Inputs,PopulationID),
         Click2 = 1 ,
            wxButton:setLabel(B,"building neural network"),
            wxButton:disable(B),
            wxPanel:refresh(Parent); %refresh the panel

    true ->
      Flag2 = stop,
      gen_server:cast(Pid,{stop}),
      Click2 = 0 ,
      terminating_func(3,Parent,B),
      wxButton:setLabel(B,"please wait"),
      wxTextCtrl:changeValue(Log, "closing simulation"),

      wxTextCtrl:changeValue(Log, "you can start again with different values"),
      wxPanel:refresh(Parent) %refresh the panel
  end,

  {noreply, State#state{clicked=Click2 , flag = Flag2}};





handle_event(#wx{event = #wxClose{}}, %close window event, handle memory leak
    State = #state{ frame=Frame , panel=_Panel
    }) ->
  wxFrame:destroy(Frame),
  io:format("exit~n"),
  {stop,normal,State};

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



handle_cast({done,Outputs} ,State = #state{frame = Frame,log = Log , flag = Flag,button = B , fitTXT = FitTXT , resTXT = ResultTxT , genTXT = GenTxT ,pic_frame = Frame2 }) ->
  if
      Flag =:= run ->
      {Fitness,Result,G,Generation} = Outputs,
      Result1=shortcut(Result,[]),
        try
        toGraph:generateGraph(G) of
          _No_error->       toGraph:replaceImage(State#state.pic_panel),
            %wxTextCtrl:changeValue(Log, ""), %clean the log
            wxTextCtrl:changeValue(FitTXT, lists:flatten(io_lib:format("~p", [Fitness]))),
            wxTextCtrl:changeValue(ResultTxT, lists:flatten(io_lib:format("~p", [Result1]))),
            wxTextCtrl:changeValue(GenTxT, lists:flatten(io_lib:format("~p", [Generation]))),
            %wxTextCtrl:writeText(FitTXT, lists:flatten(io_lib:format("~p", [Fitness]))),
            wxTextCtrl:changeValue(Log,"network in simulation"),
            wxButton:setLabel(B,"press to terminate network"),
            wxButton:enable(B),
            wxPanel:refresh(Frame), %refresh the panel
            wxPanel:refresh(FitTXT),
            wxPanel:refresh(ResultTxT),
            wxPanel:refresh(GenTxT)
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


handle_cast(_Msg, State) ->

  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _) ->
  ok.

coosen_AF(ReLU,Tanh,Binary_step,Sin)->
  if
    ReLU==true -> relu;
    Tanh==true -> tanh;
    Binary_step==true -> bin;
    Sin==true -> sin;
    true ->empty
  end.

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

run3(W) ->
  Frame = wxFrame:new(W, -1, "result",[{size, {1024, 768}}]),
  Panel = wxPanel:new(Frame,[{size, {1024, 768}}]),
  Vbox = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(Vbox, Panel, [{flag, ?wxEXPAND}]),
  PictureDraw = wxImage:new("dor.png"),
  {Width, Height} = wxPanel:getSize(Panel),
  Image = wxBitmap:new(PictureDraw),
  F = fun(I, _) -> redraw(Image,I) end,
  wxPanel:connect(Panel, paint, [{callback,F}]),
  wxFrame:show(Frame),
  5.

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