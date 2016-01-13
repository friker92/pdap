 
-module(gensup).

-export([start_tree/1]).
 
% Introduce aquí la función start_tree, que reciba un número N indicando
% la profundidad del árbol, y devuelva una tupla {ok, Pid}, donde Pid
% es el identificador del proceso del nivel superior.

start_tree(N) ->
	supervTree:start(N).

 