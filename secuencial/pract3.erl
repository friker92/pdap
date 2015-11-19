-module(pract3).
-export([lexico/1]).
-export([solutions/3]).
-export([eval/1]).
-export([amistad/2]).

%--------------------------------------------------------------------
% Ejercicio 1 
%--------------------------------------------------------------------
menor(A,[]) ->
    A;
menor({X,Y},[{A,B}|L]) ->
    if X < A ->
	    menor({X,Y},L);
       X==A , Y=<B ->
	    menor({X,Y},L);
       true ->
	    menor({A,B},L)
    end.


lexico([A|L]) -> menor(A,L).

%--------------------------------------------------------------------
% Ejercicio 2
%--------------------------------------------------------------------

solutions(Fun,Min,Max) ->
    [{X,Y} || X <- lists:seq(Min,Max), Y <- lists:seq(Min,Max), Fun(X,Y)==0].

%--------------------------------------------------------------------
% Ejercicio 3
%--------------------------------------------------------------------

operar(suma,A,B) ->
    A+B;
operar(resta,A,B) ->
    A-B;
operar(multiplica,A,B) ->
    A*B;
operar(divide,A,B) ->
    case B of
	0 ->
	    error;
	_ ->
	    A/B
    end;
operar(_,_,_) ->
    error.


eval({T,A}) ->
    case T of
	int ->
	    A;
	_ -> 
	    error
    end;

eval({Op,E1,E2}) ->
    case eval(E1) of
	error ->
	    error;
	A ->
	    case eval(E2) of
		error ->
		    error;
		B ->
		    operar(Op,A,B)
	    end
    end.

%--------------------------------------------------------------------
% Ejercicio 4
%--------------------------------------------------------------------
media([],V,N) ->
    case N of
	0 -> 0;
	_ -> V / N
    end;
media([{Ed,_,_}|Ps],V,N) ->
    media(Ps,V+Ed,N+1).

hacerParejas([]) -> 
    [];
hacerParejas([A|L]) -> 
    hacerParejas(L) ++ lists:map(fun(X) -> {A,X} end,L).

filtroGenero(L,indiferente) ->
    L;
filtroGenero(L, mismo) ->
    lists:filter(fun({{_,X,_},{_,Y,_}})-> X==Y end,L);
filtroGenero(L,diferente) ->
    lists:filter(fun({{_,X,_},{_,Y,_}})-> not (X==Y) end,L).

amistad(L,Filt) ->
    Media = media(L,0,0), %calcular la media
    Parejas = filtroGenero(hacerParejas(L),Filt), % Formar las parejas teniendo en cuenta el genero
    lists:filter(fun({{Ex,_,Ax},{E,_,A}}) -> % filtrar por condiciones
			 ( ((Media-10 =< E ) and (E  =< Media+10) )     and
			 (  (Media-10 =< Ex) and (Ex =< Media+10) ) )   or %ambas parejas proximas a la media Or
			 ( ((Media-10 >  E ) or  (E  >  Media+10) )     and
			 (  (Media-10 >  Ex) or  (Ex >  Media+10) ) )   or % ambas parejas lejos de la media Or
			 ( lists:filter(fun(X) -> lists:member(X,A) end,Ax) /= []) % alguna Aficcion en comun
		 end,Parejas).


	  
