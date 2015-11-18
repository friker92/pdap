-module(pract4).
-compile(export_all).
-include("pract4.hrl").
% carta,{valor=as,palo=corazones}

parejas(#mano{cartas=Cartas})->
	pairs(Cartas).

pairs() -> [{X,Y} || X <- lists:seq(1,N), Y <-lists:seq(N+1,5)].

cut([C|_], 1) -> [C];
cut([C|L], N) -> [C|cut(L,N-1)].
