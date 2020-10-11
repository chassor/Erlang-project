%%%-------------------------------------------------------------------
%%% @author chass
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2020 20:01
%%%-------------------------------------------------------------------
-module(run).
-author("chass").
-export([run/0]).


run()->




  X= compile:file("C:/Users/chass/IdeaProjects/ErlangProjectFinal/src/nn_agent",[debug_info]),
  Y= compile:file("C:/Users/chass/IdeaProjects/ErlangProjectFinal/src/main",[debug_info]),
  Z= compile:file("C:/Users/chass/IdeaProjects/ErlangProjectFinal/src/neuron2",[debug_info]),
  W= compile:file("C:/Users/chass/IdeaProjects/ErlangProjectFinal/src/mutate_generator",[debug_info]),
  C= compile:file("C:/Users/chass/IdeaProjects/ErlangProjectFinal/src/toGraph",[debug_info]),
  T= compile:file("C:/Users/chass/IdeaProjects/ErlangProjectFinal/src/master",[debug_info]),
  GUI=compile:file("C:/Users/chass/IdeaProjects/ErlangProjectFinal/src/gui",[debug_info]),
  POP=compile:file("C:/Users/chass/IdeaProjects/ErlangProjectFinal/src/population",[debug_info]),


  if
    is_tuple(T) andalso is_tuple(POP) andalso is_tuple(GUI)andalso
      is_tuple(X) andalso is_tuple(Y) andalso is_tuple(Z)andalso is_tuple(W)  andalso is_tuple(C)->
      main:run();
    true -> badcomile
  end.
%% API




