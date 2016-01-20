-module(clientes).

-export([read_file/0,read_file/1, load_tabla/1]).

read_file()->
	read_file("./clientes.txt").
read_file(Path) ->
	{ok,F} = file:open(Path,[read,raw]),
	read_csv(F,file:read_line(F),[]).
	
read_csv(F,eof,List)->
	file:close(F),
	lists:reverse(List);
	
read_csv(F,{ok,Line},List) ->
	read_csv(F,file:read_line(F),[toPersona(Line)|List]).
	
toPersona(L) ->
	[I,N,E,C] = lists:map(fun(Field)->parse(string:strip(Field)) end,string:tokens(L,",")),
	P = {I, N, E, C},
	io:format("to persona: ~p~n",[P]),P.
	
parse(E)->
	case string:to_integer(E) of
		{N,[]}->
			N;
		_ ->
			string:strip(E,both,$\n)
	end.

load_tabla(L) ->
	T = ets:new(cliente_tabla, [set]),
	ets:insert(T,L),T.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c(clientes).
% L = read_file().
% T = load_tabla(L).
	
	
%%%%%%%%%%%%%%
% CONSULTA 1 %
%%%%%%%%%%%%%%
% ets:match(T, { 3, '_', '_', '$1'}).

%%%%%%%%%%%%%%
% CONSULTA 2 %
%%%%%%%%%%%%%%
%  ets:select(T, [{{'_', ['$1'|'$2'], '_',"Madrid"},[{'=:=','$1',67}],[['$1'|'$2']]}]).


%%%%%%%%%%%%%%
% CONSULTA 3 %
%%%%%%%%%%%%%%
% ets:select(T, [{{'_', '$1', '$2','_'},[{'>=','$2',20},{'=<','$2',30}],['$1']}]).