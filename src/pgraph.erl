
-module(pgraph).

-export([generate_pgraph/0, proc_server/3, crawler/0]).


proc_server(Fproc_list, Vproc_list, Proc_info) ->
  %% Process server maintains the list of yet-to-crawl and already-crawled
  %% processes. Crawlers request the proc_server for processes and report
  %% info on the process back to the server. Once, all the processes are
  %% done, the information on the process is ready in the proc server.
  receive
    {request, Crawler} ->
      case Fproc_list of
        [] ->
          Crawler ! {empty};
        [H|_] ->
          Crawler ! {new, H}
      end,
      proc_server(Fproc_list, Vproc_list, Proc_info);
    {report, Links, _} ->
      case Links of
        [] ->
          proc_server(Fproc_list, Vproc_list,
            Proc_info);
        [_|_] ->
          proc_server(Fproc_list, Vproc_list,
            lists:append(Links, Proc_info))
      end;
    {update, Plist, _} ->
      case Plist of
        [] ->
          proc_server(Fproc_list, Vproc_list,
            Proc_info);
        [_|_] ->
          proc_server(get_new_procs(Plist, Fproc_list, Vproc_list),
            Vproc_list, Proc_info)
      end;
    {done, Proc, _} ->
      proc_server(lists:delete(Proc, Fproc_list), [Proc|Vproc_list],
        Proc_info);
    {quit, _} ->
      store_proc_info(Proc_info),
      quit
  end.


find_proc_names(Plist) ->
  %% Finds the registered names of a list of processes.
  find_proc_names(Plist, []).

find_proc_names([], Pnames) ->
  Pnames;
find_proc_names([H|R], Pnames) ->
  case lists:keyfind(registered_name, 1, process_info(H)) of
    {_, Pname} ->
      find_proc_names(R, [Pname | Pnames]);
    false ->
      Unregistered = "\"unknown" ++ pid_to_list(H) ++ "\"",
      find_proc_names(R, [Unregistered | Pnames])
  end.


get_new_procs(Plist, Fprocs, Vprocs) ->
  %% Finds the processes that are not already visited or already in freshly
  %% discovered list, and returns the new (fresh) process list.
  add_new_procs(Plist, lists:append(Fprocs, Vprocs), Fprocs).

add_new_procs([], _, New_procs)->
  New_procs;
add_new_procs([H|R], Eprocs, New_procs) ->
  case lists:member(H, Eprocs) of
    true ->
      add_new_procs(R, Eprocs, New_procs);
    false ->
      add_new_procs(R, Eprocs, [H | New_procs])
  end.


get_proc_links(Proc) ->
  %% Returns a list of linked processes.
  {_, Plinks} = lists:keyfind(links, 1, process_info(Proc)),
  lists:filter(fun(Item) -> is_pid(Item) end, Plinks).


crawler() ->
  %% Finds a process' association with others.
  pserv ! {request, self()},
  receive
    {empty} ->
      pserv ! {quit, self()};
    {new, Proc} ->
      [Pname] = find_proc_names([Proc|[]]),
      Plinks = get_proc_links(Proc),
      Prels = [{Pname, P} || P <- find_proc_names(Plinks)],
      pserv ! {report, Prels, self()},
      pserv ! {update, Plinks, self()},
      pserv ! {done, Proc, self()},
      crawler()
  end.


store_proc_info(Proc_info) ->
  %% Stores the process' information in a file.
  case file:open("./process-graph.dot", [write]) of
    {ok, Fileid} ->
      io:fwrite(Fileid, "digraph process_map { ~n", []),
      io:fwrite(Fileid,
        "    init [style=filled,color=\".7 .3 .9\"];~n", []),
      store_proc_info(Proc_info, Fileid);
    {_} ->
      io:format("Failed to write to file.~n"),
      io_error
  end.

store_proc_info([], Fileid) ->
  io:fwrite(Fileid, "} ~n", []),
  file:close(Fileid);
store_proc_info([H|R], Fileid) ->
  {P1, P2} = H,
  io:fwrite(Fileid, "    ~s  ->  ~s;  ~n", [P1, P2]),
  store_proc_info(R, Fileid).


generate_pgraph() ->
  register(pserv, spawn(?MODULE, proc_server, [processes(), [], []])),
  spawn(?MODULE, crawler, []).