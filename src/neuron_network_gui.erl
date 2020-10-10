-module(neuron_network_gui).
-behaviour(wx_object).
-import(util,[integer_to_atom/1]).
-export([start/1, init/1,handle_event/2,handle_info/2, handle_call/3,handle_cast/2,code_change/3,terminate/2]).
-include_lib("wx/include/wx.hrl").
-define(SERVER,?MODULE).
-record(state, {clicked, activation_function, neurons,layers,sensors,actuators,nn,frame,panel,log , main_pid , button , image , node}).

start(Node) ->
  wx_object:start_link({local,?SERVER},?MODULE,[global,Node],[]),
  receive
    {loop}->ok
  end.


init([Mode,Node]) ->
  InitState = initiation(Mode,Node),
  {InitState#state.frame,InitState}.

initiation(_Mode,_Node) ->
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
  Nodes  = wxTextCtrl:new(Panel, 5, [{value, ""},
    {style, ?wxDEFAULT}]),

  Choice = wxListBox:new(Panel, 7, [{choices, Choices}]),
  LayersPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(LayersPicker, 1, 1000),
  NeuronsPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(NeuronsPicker, 1, 1000),
  SensorsPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(SensorsPicker, 1, 1000),
  ActuatorsPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(ActuatorsPicker, 1, 1000),
  NetworksPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(NetworksPicker, 1, 250),
  ButtonPicker = wxButton:new(Panel, 10, [{label, "press to start"}]),
  Log = wxTextCtrl:new(Panel, 7, [{value, "insert initial values for networs please :)"},
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
   %{ok,Pid} = main:start_link(), ----------- mybe on the click event ..
   Pid =  spawn(main2 , loop, [self()]),
   wxFrame:show(Parent),

   W= wx:new(),


     State = #state{
       clicked=0,
       activation_function=Choice,
       neurons=NeuronsPicker,
       layers=LayersPicker,
       sensors=SensorsPicker,
       actuators=ActuatorsPicker,
       nn = NetworksPicker,
       frame=Parent,
       main_pid = Pid,
       log=Log,
       button = ButtonPicker,
       image = W,
       node = Nodes,
       panel=Panel}.



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
      node = Node,
      clicked=Click}) ->

      Noeds_List=string:split(wxTextCtrl:getValue(Node),",",all),
  if

        Click=:=0 -> %check if the previous run was completed and the file is valid
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
          Pid = State#state.main_pid,
         {Pid ! {start,self() ,Sensors ,Actuators ,Layers,  Neurons ,AF2 ,NN}},
          wxTextCtrl:changeValue(Log, ""),
        %run_nn(self()  ,Sensors  ,Actuators  ,Layers ,  Neurons   ,AF  ,Num_Of_NN_AGENTS ,Inputs,PopulationID),
         Click2 = 1 ,
         wxButton:setLabel(B,"press to terminate network"),
          wxPanel:refresh(Parent); %refresh the panel
          %wxTextCtrl:changeValue(Log, " networks calculating");
    true ->
      {State#state.main_pid ! {stop}},
      Click2 = 0 ,
      wxButton:setLabel(B,"start"),
      wxTextCtrl:changeValue(Log, "you can start again with different values"),
      wxPanel:refresh(Parent) %refresh the panel

  end,

  {noreply, State#state{clicked=Click2}};

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

handle_cast({done,Outputs} ,State = #state{frame = Frame,log = Log}) ->
  {Fitness,Result,G} = Outputs,
  Result1=sotrcut(Result,[]),
  wxTextCtrl:changeValue(Log, ""), %clean the log
  wxTextCtrl:writeText(Log, lists:flatten(io_lib:format("Fitness: ~p ,outputs list:~p", [Fitness,Result1]))),
  wxPanel:refresh(Frame), %refresh the panel
  {noreply,State#state{clicked=1}};

handle_cast({final_result,Outputs} ,State = #state{frame = Frame,log = Log,main_pid = Pid,image = W}) ->
  {Fitness,Result,G} = Outputs,
  Result1=sotrcut(Result,[]),
  wxTextCtrl:changeValue(Log, ""), %clean the log
  wxTextCtrl:changeValue(Log, "you can start again with different values"),
  wxPanel:refresh(Frame), %refresh the panel
  toGraph:generateGraph(G),
  run3(W),
  {Pid ! {terminate}},
  {noreply,State#state{clicked=0}};



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

sotrcut([],L)->L;
sotrcut([H|T],L)->
  if
      H=:=0 ->
        round(H),
      L2 = L ++ [H],
      sotrcut(T,L2) ;
    true ->
      H_Float= float(H),
      L2= L ++ [float_to_list(H_Float,[{decimals,2}])],
      sotrcut(T,L2)
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
