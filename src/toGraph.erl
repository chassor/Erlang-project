%%%-------------------------------------------------------------------
%%% @author chass
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2020 20:09
%%%-------------------------------------------------------------------
-module(toGraph).
-author("chass").

%% API
-export([generateGraph/1, createFrame/0, replaceImage/1]).
-include_lib("wx/include/wx.hrl").

generateGraph(G)->
  L1=getEdgesList(G),
  L3=getVerticesList(G),
  Map=creatMapOfNewNames(L3,maps:new(),1),
  L2 = createNodesList(Map,L3,[]),

  L = [{maps:get(A,Map),maps:get(B,Map),C}||{_,A,B,C} <- L1],
  %Vertices_final= [{A,maps:get(B,Map),C}||{A,B,C} <- L],


  Graph = { "G" , {digraph ,"->"} ,[],L2,L},
  to_file( Graph ,"dor.png", "png"),
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
  io:format(IODevice, "dpi= 100 ;ratio=\"fill\";size=\"14.7,7.3!\";margin=0;", []),
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
  creatMapOfNewNames(T, NewMap,N+1).



createFrame()->
  W = wx:new(),
  Frame = wxFrame:new(W, -1, "result",[{size, {1420, 740}}]),
  Panel = wxPanel:new(Frame,[{size, {1400, 700}}]),
  Label = wxStaticText:new(Frame, ?wxID_ANY, "neuron network"),
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(MainSizer, Label, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  Vbox = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(Vbox, Panel, [{flag, ?wxEXPAND}]),
  {Width, Height} = wxPanel:getSize(Panel),

  %PictureDrawScaled = wxImage:scale(PictureDraw, 1280, 720,[{quality,?wxIMAGE_QUALITY_HIGH}]),
  wxFrame:show(Frame),
  {Frame,Panel}.



replaceImage(Panel) ->
PictureDraw1 = wxImage:new("dor.png"),
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
