-module(main).
-export([run/0, num_of_alive_processes/0, loop/0, loop2/0]).
-include_lib("wx/include/wx.hrl").
run()->

 AB=master:start_link(),


  loop()



.

loop()->
  timer:sleep(10000),

  loop()
.

loop2()->
  io:format("loop loop loop ~n" , []),
  timer:sleep(10000),

  loop2().
run1()->

  {Frame,Panel,_,_,_} = toGraph:createFrame(),

  %PictureDrawScaled = wxImage:scale(PictureDraw, 1280, 720,[{quality,?wxIMAGE_QUALITY_HIGH}]),
  wxFrame:show(Frame),


  process_flag(trap_exit, true),

  toGraph:replaceImage(Panel),

  X=nn_agent:start_link(SensorNum=3,ActuatorNum=2,NumOfLayers=2,NumOfNeuronsEachLayer=4,ManagerPid=self(),Id=nn5,new),

 G= gen_statem:call(X,{self(),get_graph}),
  digraph:edges(G),
  G=gen_statem:call(X,{self(),get_graph}),
  gen_statem:stop(X),
   timer:sleep(1000),
  X22=3,
  X33=19,
  try
    gen_statem:call(X,{self(),get_graph}) of
    _Result->ok
  catch
    _Reason:_Reason1->
      ok
  end,
 % mutate_generator:mutateAgent(G),
%  Y=nn_agent:start_link(2,2,2,3,ManagerPid=self(),nn70,G),

 % nn_agent:createNN(G,nn12),
X2=7,
  X5=17,
  X!{x,t,z},
gen_statem:cast(X,{self(),insert_input,[1,2,1]}),


  Y=nn_agent:start_link(SensorNum,ActuatorNum,NumOfLayers,NumOfNeuronsEachLayer,ManagerPid=self(),nn6,new),
  loop(X,Y, {0,2000},Panel,Frame).


loop(Y,X, {W1,W2},Panel,Frame)->

receive


{_Fromm,result,ResultList}->
[H|[T]]=ResultList,
X2=2,
  L=ResultList,
  %  gen_statem:stop(X),
%X4=is_process_alive(X),
%  L2= [is_process_alive(Pid)||Pid<-L],
 % L=[random:uniform(5),random:uniform(5)],
  %gen_statem:cast(X,{self(),insert_input,L}),
  G=gen_statem:call(X,{self(),get_graph}),

  %timer:sleep(10),
  Name=list_to_atom(lists:flatten(io_lib:format("nn~p", [nn_agent:generate_id()]))),
  Y2=nn_agent:start_link(2,2,2,3,ManagerPid=self(),Name,G),
  gen_statem:call(Y2,{self(),mutate}),
  G2=gen_statem:call(Y2,{self(),get_graph}),
  L1=mutate_generator:getEdgesList(G2),
  L2=mutate_generator:getVerticesList(G2),
  gen_statem:cast(Y2,{self(),insert_input,[1,2,1]}),

  Counter=num_of_alive_processes(),
  Min =abs(150-abs(W1-W2)),
  Min2=abs(150-abs(H-T)),
  Score=abs(W1-W2),


if
  abs(150-Score)<0.001->
1;
  true->


  if
    Min2<Min ->
      toGraph:generateGraph(G),
      timer:sleep(2000),
      toGraph:replaceImage(Panel),
      gen_statem:stop(X),
    loop(Y2,Y, {H,T},Panel,Frame);

    true ->

      gen_statem:stop(Y),
      loop(Y2,X, {W1,W2},Panel,Frame)

  end

end
end

.


loop2(X) ->
  loop2(X).

generate_id() ->
  {MegaSeconds,Seconds,MicroSeconds} = now(),
  (MegaSeconds*1000000 + Seconds + MicroSeconds/1000000).

num_of_alive_processes() ->
  L5= processes(),
  L6=[is_process_alive(A)|| A<-L5],
  L7=[true || true<-L6],
  L=length(L7),
 L.







generateGraph(G)->
  L1=getEdgesList(G),
  L3=getVerticesList(G),
  Map=creatMapOfNewNames(L3,maps:new(),1),
  L2 = createNodesList(Map,L3,[]),

  L = [{maps:get(A,Map),maps:get(B,Map),C}||{_,A,B,C} <- L1],
  %Vertices_final= [{A,maps:get(B,Map),C}||{A,B,C} <- L],


  Graph = { "G" , {digraph ,"->"} ,[],L2,L},
  to_file( Graph ,"test1.png", "png"),
  Graph.

to_file(Graph, File, Format) ->
  {A1,A2,A3} = now(),
  DotFile = lists:concat([File, ".dot-", A1, "-", A2, "-", A3]),
  to_dot(Graph, DotFile),
  DotCommant = lists:concat(["dot -T", Format, " -o", File, " ", DotFile]),
  X=os:cmd(DotCommant),


file:delete(DotFile),
  X.

to_dot(Graph, File) ->
  {GraphId, Type, GraphOptions, Nodes, Edges} = Graph,
  {GraphType, EdgeType} = Type,



  % open file
  {ok, IODevice} = file:open(File, [write]),

  % print graph
  io:format(IODevice, "~s ~s {~n", [GraphType, GraphId]),

%io:format(IODevice, "size =\"100,100\";~n", []),
io:format(IODevice, "rankdir=LR;~n", []),
%  io:format(IODevice, "splines=line;~n", []),
  io:format(IODevice, "nodesep=0.001;center=true;~n", []),
  io:format(IODevice, "dpi= 90;", []),
  io:format(IODevice, "ratio=0.5;", []),
  io:format(IODevice, "size=\"14,7!\";", []),
%  io:format(IODevice, "margin=0;", []),
  % print nodes
  lists:foreach(
    fun({Label,Name,Bias,AF,_K}) ->


      if
       Bias=:= no-> io:format(IODevice, "  ~s  [label=\"~s\",shape=box,fillcolor=cyan,style=filled];~n",[Name,Label]);
       true->
         if
           Label =:= neuron ->  if
                                Bias=:= 0-> io:format(IODevice, "  ~s  [label=\"~s\\nBias: ~s\\nAF: ~s\",shape=circle,fillcolor=chartreuse1,style=filled,width=1,height=1,fixedsize=true];~n",[Name,Label,"0",AF]);
                                true->io:format(IODevice, "  ~s  [label=\" ~s\\nBias: ~s\\nAF: ~s\",shape=circle,fillcolor=chartreuse1,style=filled,width=1,height=1,fixedsize=true]; ~n",[Name,Label,float_to_list(Bias,[{decimals,3}]),AF])
                              end;
         true -> if
                                 Bias=:= 0-> io:format(IODevice, "  ~s  [label=\"~s\\nBias: ~s\\nAF: ~s\",shape=box,fillcolor=cyan,style=filled];~n",[Name,Label,"0",AF]);
                                 true->io:format(IODevice, "  ~s  [label=\" ~s\\nBias: ~s\\nAF: ~s\",shape=box,fillcolor=cyan,style=filled]; ~n",[Name,Label,float_to_list(Bias,[{decimals,3}]),AF])
                               end
         end

      end

    end,
    Nodes
  ),



  % print edges
  lists:foreach(
    fun(Edge) ->
      {NodeOne, NodeTwo,Weight} = Edge,
      if
       Weight=/=0-> io:format(IODevice, "  ~s ~s ~s [label=~p] ;~n",[NodeOne, EdgeType, NodeTwo,float_to_list(Weight,[{decimals,3}])]);
        %true-> io:format(IODevice, "  ~s ~s ~s [color=none] ;~n",[NodeOne, EdgeType, NodeTwo])
        true-> ok
      end
    end,
    Edges
  ),

  io:format(IODevice, "{ rank=min", []),
  [ io:format(IODevice, ";~s", [B])||{_A,B,_C,_D,sensor}<-Nodes],
  io:format(IODevice, "}~n", []),
  io:format(IODevice, "{ rank=max", []),
  [ io:format(IODevice, ";~s", [B])||{_A,B,_C,_D,actuator}<-Nodes],
  io:format(IODevice, "}~n", []),


  % close file
  io:format(IODevice, "}~n", []),
  file:close(IODevice).

getEdgesList(G)->
  B=digraph:edges(G),
  [digraph:edge(G,E) || E <- B].

getVerticesList(G)->
  A=digraph:vertices(G),
  [digraph:vertex(G,V) || V <- A].


createNodesList(_,[],L) ->lists:sort(L);
createNodesList(Map,[H|T],L) ->
  {A2,{B,_C,_D,E,F,G}}=H ,
  A=maps:get(A2,Map),
  case B of
    neuron-> createNodesList(Map,T,L ++[{neuron,A,F,G,B}]);
    sensor->createNodesList(Map,T,L ++[{E,A,no,no,B}]);
    actuator->createNodesList(Map,T,L ++[{E,A,F,G,B}])
  end
.





creatMapOfNewNames([], Map,N) ->Map;
creatMapOfNewNames([{B,{A,_B,_C,V,_R,_}}|T], Map,N) ->
  if
    A=:=neuron->NewName=list_to_atom(lists:flatten(io_lib:format("x~p", [N])));
    true->NewName=V
  end,

  NewMap=maps:put(B,NewName,Map),
  creatMapOfNewNames(T, NewMap,N+1)
.

run3(Panel,Frame) ->
 % Panel2 = wxPanel:new(Frame,[{size, {1600, 800}}]),
  PictureDraw1 = wxImage:new("test1.png"),
  PictureDraw=wxImage:rescale(PictureDraw1,1400,700,[{quality,?wxIMAGE_QUALITY_HIGH}]),
  Image1 = wxBitmap:new(PictureDraw),
  Image = wxStaticBitmap:new(Panel,12,Image1),
  F = fun(I, _) -> redraw(Image1,I) end,
  wxPanel:connect(Panel, paint, [{callback,F}]),
 % wxWindow:refresh(Panel,[{eraseBackground,false}]),





Panel.

redraw(Image, #wx{obj=Panel}) ->
  DC = wxBufferedPaintDC:new(Panel),
  wxDC:drawBitmap(DC,Image,{0,0}).



