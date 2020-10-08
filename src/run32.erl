-module(run32).
-export([run/0,run2/0]).


  run2()->
graphviz:digraph("G"),
graphviz:add_edge(a, b),
graphviz:add_edge(b, c),
graphviz:add_edge(b, d),
X=  graphviz:to_file("test2.png", "png"),
X.
 run()->
   X=nn_agent:start_link(SensorNum=5,ActuatorNum=3,NumOfLayers=5,NumOfNeuronsEachLayer=6,ManagerPid=self(),Id=nn5,new),
   G=gen_statem:call(X,{self(),get_graph}),
   L1=getEdgesList(G),
L = [{A,B}||{_,A,B,_} <- L1],
L2 = [] ,
Graph = { "G" , {digraph ,"->"} ,[],L2,L},
to_file( Graph ,"test1.png", "png"),
Graph.

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