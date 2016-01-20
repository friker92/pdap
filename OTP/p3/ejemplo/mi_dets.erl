-module(mi_dets).

-export([example_table/0]).

example_table() ->
  T = dets:open_file(mi_tabla, [{file, "tabla.dets"}, {type, set}]),
  dets:insert(mi_tabla,
    [{1, ["Joe Armstrong"], "Programming Erlang", 2013},
     {2, ["Francesco Cesarini", "Simon Thompson"], "Erlang Programming", 2009},
     {3, ["Fred Hébert"], "Learn you some Erlang for the Great Good", 2013}]),
  T.
     

