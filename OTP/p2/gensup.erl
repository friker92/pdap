 
-module(gensup).

-export([start_tree/1]).
 
% Introduce aqu� la funci�n start_tree, que reciba un n�mero N indicando
% la profundidad del �rbol, y devuelva una tupla {ok, Pid}, donde Pid
% es el identificador del proceso del nivel superior.

start_tree(N) ->
	supervTree:start(N).

 