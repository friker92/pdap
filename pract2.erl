% sol. pract. 2, prog.declarariva avanzada
-module(pract2).
-include_lib("eunit/include/eunit.hrl").
-export([impares/1,sonMultiplos/2, h/1, esta/2, mapTree/2, nNodos/1, mismoConjunto/2, ack/2, normal/1, intersection/2, start/0, saluda/1, ack_test/0]).

start() -> io:format("~w",[pract2:mapTree((fun(E)->E+1 end), {6, {5, {3,{},{}},{}},{}})]). 

saluda([X|_]) -> io:format("hola ~w ~n",[X]).
%%%%%% 1 %%%%%%%%
impares([]) -> [];
impares([X]) -> [X];
impares([X,_|Xs]) -> [X|impares(Xs)].

impares_test() ->
    ?assertEqual(impares([a,b,c,d,e,f]),[a,c,e]).

%%%%%% 2 %%%%%%%%
ack(0,N) -> N+1;
ack(M,N) when M>0, N==0-> ack(M-1,1);
ack(M,N) when M>0, N>0-> ack(M-1,ack(M,N-1)).

ack_test() ->
	125=ack(3,4).

%%%%%% 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%% 3.1 %%%%%%%%
mismoConjunto(X,Y) -> incluido(X,Y) andalso incluido(Y,X).

%% función auxiliar que indica si el conjunto X está incluido en Y (o es igual a Y)
incluido(X,Y) -> lists:all( (fun(E)->lists:member(E,Y) end),X).

%%%%%%% 3.2 %%%%%%%
normal(X) -> normal(X,[]).
normal([],Y) -> Y;
normal([X|Xs],Y) -> case lists:member(X,Y) of
                      true -> normal(Xs,Y);
                      false-> normal(Xs,[X|Y])
                    end. 

%%%%%%% 3.3 %%%%%%%
intersection([],_) -> [];
intersection([X|Xs],Y) -> I=intersection(Xs,Y),
                          case lists:member(X,Y) of
                             true -> [X|I];
                             false-> I
                          end.


%%%%%%%%%%%%%%%%%%%%%%%% 4 %%%%%%%%%%%%%%%%%%

%%%%%% 4.1 %%%%%%%%%
esta(_,{}) -> false;
esta(E,{E,_,_}) -> true;
esta(E,{_,HijoIzq,HijoDer}) -> esta(E,HijoIzq) orelse  esta(E,HijoDer).

%%%%%% 4.2 %%%%%%%%
nNodos({}) -> 0;
nNodos({_,Izq,Der}) -> 1 + nNodos(Izq) + nNodos(Der).

%%%%%% 4.3 %%%%%%%%%%%%
mapTree(_,{}) -> {};
mapTree(F,{E,Izq,Der}) -> {F(E), mapTree(F,Izq), mapTree(F, Der)}.

%%%%%%%%%%%%%% 5 %%%%%%%%%%
sonMultiplos(X,Y) when X==0; Y==0 -> true;
sonMultiplos(X,Y) when X>=Y -> X rem Y =:= 0;
sonMultiplos(X,Y) when Y>X -> Y rem X =:= 0.


%%%%%%%%%%%%% 6 %%%%%%%%%%%%%
h(X) -> fun (Y) when X==0; Y==0 -> true;
            (Y) when X>Y -> false; 
            (Y) when Y>=X  -> Y rem X =:= 0
        end.

