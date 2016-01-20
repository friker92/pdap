-module(mi_ets).

-export([example_table/0, fact/1]).

example_table() ->
  T = ets:new(mi_tabla, [set]),
  ets:insert(T,
    [{1, ["Joe Armstrong"], "Programming Erlang", 2013},
     {2, ["Francesco Cesarini", "Simon Thompson"], "Erlang Programming", 2009},
     {3, ["Fred Hébert"], "Learn you some Erlang for the Great Good", 2013}]),
  T.


