-module(neuron_network_gui).
-behaviour(wx_object).
-import(util,[integer_to_atom/1]).
-export([start/1, init/1,handle_event/2,handle_info/2, handle_call/3,handle_cast/2,code_change/3,terminate/2]).
-include_lib("wx/include/wx.hrl").
-define(SERVER,?MODULE).
-record(state, {clicked, activation_function, neurons,layers,sensors,actuators,frame, master, panel,log}).

start(Node) ->
  wx_object:start_link({local,?SERVER},?MODULE,[global,Node],[]),
  ok.

init([_Mode,_Node]) ->
  wx:new(),
  Choices = ["ReLU","tanh","Binary step","Sigmoid"], %supported algorithms
  AF_Frame = wxFrame:new(wx:null(), 1, "neuroevolution gui" ,[{size,{600, 600}}]), %create frame
  Panel = wxPanel:new(AF_Frame, []), %create panel

  %% Setup sizer

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

  Choice = wxListBox:new(Panel, 7, [{choices, Choices}]),
  LayersPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(LayersPicker, 1, 1000000000),
  NeuronsPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(NeuronsPicker, 1, 1000000000),
  SensorsPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(SensorsPicker, 1, 1000000000),
  ActuatorsPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(ActuatorsPicker, 1, 1000000000),
  NetworksPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(NetworksPicker, 1, 1000000000),
  ButtonPicker = wxButton:new(Panel, 10, [{label, "start"}]),


  wxFrame:connect(AF_Frame,close_window), %connect between closing the window and the handler
  wxButton:connect(ButtonPicker,command_button_clicked), %connect between start click and the handler

  %% Add to sizer
  PickerOptions = [{border, 1},{flag, ?wxALL bor ?wxEXPAND}],
  wxSizer:add(LayersPickerSizer, LayersPicker, PickerOptions),
  wxSizer:add(NeuronsPickerSizer, NeuronsPicker, PickerOptions),
  wxSizer:add(SensorsPickerSizer, SensorsPicker, PickerOptions),
  wxSizer:add(ActuatorsPickerSizer, ActuatorsPicker, PickerOptions),
  wxSizer:add(NetworksPickerSizer, NetworksPicker, PickerOptions),
  wxSizer:add(ButtonPickerSizer, ButtonPicker, PickerOptions),


  wxSizer:add(ChoicePickerSizer, Choice, PickerOptions),


  SizerOptions  = [{flag, ?wxEXPAND}],
  wxSizer:add(MainSizer, ChoicePickerSizer, SizerOptions),
  wxSizer:add(MainSizer, LayersPickerSizer, SizerOptions),
  wxSizer:add(MainSizer, NeuronsPickerSizer, SizerOptions),
  wxSizer:add(MainSizer, SensorsPickerSizer, SizerOptions),
  wxSizer:add(MainSizer, ActuatorsPickerSizer, SizerOptions),
  wxSizer:add(MainSizer, NetworksPickerSizer, SizerOptions),
  wxSizer:add(MainSizer, ButtonPickerSizer, SizerOptions),

  wxPanel:setSizer(Panel, MainSizer),
  %{ok,M}=master:start_link(),
  wxFrame:show(AF_Frame),

     State = #state{
      clicked=0,
     activation_function=Choice,
    neurons=NeuronsPicker,
    layers=LayersPicker,
    sensors=SensorsPicker,
    actuators=ActuatorsPicker,
    frame=AF_Frame,
    %master=M,
    panel=Panel},
     State.



%%handle 'start' event
handle_event(#wx{obj = _Button, event = #wxCommand{type = command_button_clicked}},
    State = #state{
      activation_function=Choice,
      neurons=NeuronsP,
      layers=LayersP,
      sensors=SensorsP,
      actuators=ActuatorsP,
      %master=M,
      frame=_Parent,
      panel=_Panel,
      clicked=Click}) ->
  if

      Click==0 -> %check if the previous run was completed and the file is valid
        Neurons = wxSpinCtrl:getValue(NeuronsP), %get the Neurons
        Layers = wxSpinCtrl:getValue(LayersP), %get the Layers
        Sensors = wxSpinCtrl:getValue(SensorsP), %get the Sensors
        Actuators = wxSpinCtrl:getValue(ActuatorsP), %get the Actuators
        ReLU=wxListBox:isSelected(Choice,0), %check if the user chose ReLU as the algorithm
        Tanh =wxListBox:isSelected(Choice,1), %check if the user chose Tanh as the algorithm
        Binary_step =wxListBox:isSelected(Choice,2), %check if the user chose Binary_step as the algorithm
        Sigmoid =wxListBox:isSelected(Choice,3), %check if the user chose Sigmoid as the algorithm
        io:format("Starting neuro evolotaion",[]),
        Click=1;
        % AF=coosen_AF(ReLU,Tanh,Binary_step,Sigmoid),
        %run_nn(self()  ,Sensors  ,Actuators  ,Layers ,  Neurons   ,AF  ,Num_Of_NN_AGENTS ,Inputs,PopulationID),


    true -> todo

  end,
  {noreply, State#state{clicked=Click}};



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

handle_cast({done,Outputs}, %handle completion call from the master
    State = #state{activation_function = Choice ,frame = Frame,log = Log}) ->
  wxTextCtrl:changeValue(Log, ".  "), %clean the log
  SP=wxListBox:isSelected(Choice,0), %chose what output to print
  MST=wxListBox:isSelected(Choice,1),
  BFS=wxListBox:isSelected(Choice,2),
  if
    SP == true -> {Runtime, Iterations, Path, Weight}=Outputs, wxTextCtrl:writeText(Log, lists:flatten(io_lib:format("Runtime: ~p , Iterations: ~p, Path: ~p  , Weight: ~p", [Runtime,Iterations,Path,Weight])));
    MST == true -> {Weight,Iterations,Runtime} = Outputs,wxTextCtrl:writeText(Log, lists:flatten(io_lib:format("Runtime: ~p , Iterations: ~p,  Weight: ~p", [Runtime,Iterations,Weight]))) ;
    BFS == true -> {Runtime,Iterations,MeanDest} = Outputs,wxTextCtrl:writeText(Log, lists:flatten(io_lib:format("Runtime: ~p , Iterations: ~p,  Mean Distance : ~p", [Runtime,Iterations,MeanDest]))) ;
    true->ok
  end,


  wxPanel:refresh(Frame), %refresh the panel
  {noreply,State#state{clicked=0}};

handle_cast(_Msg, State) ->

  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _) ->
  ok.

run_nn()->todo.
