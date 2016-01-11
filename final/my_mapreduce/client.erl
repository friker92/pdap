-module(client).

-export([client/1,doblepeticion/0]).

%%%%% Para ejecutar:
%%% c(master),c(admin),c(worker),c(client).
%%% master:start(7). % 7 es el numero de nodos
%%% client:client(1).
%%% client:doblepeticion().


client(1)->
    {admin,M} = master:mapreduce(5),
    Info = info(1),
    admin:addInfo(M,Info),
    admin:addFase(M,{none,fun(A,B)-> map1(A,B) end, none}),
    admin:addFase(M,{none,fun(A,B)-> reduce1(A,B) end, none}),
    admin:inicia(M);
client(2)->
    {admin,M} = master:mapreduce(5),
    Info = info(2),
    admin:addInfo(M,Info),
    admin:addFase(M,{none,fun(A,B)-> map2(A,B) end, none}),
    admin:addFase(M,{none,fun(A,B)-> reduce2(A,B) end, none}),
    admin:setBasic(M,split,fun(T,N) -> splitText(T,N) end),
    admin:inicia(M).
doblepeticion() ->
    {fin,A} = client(1),
    {fin,B} = client(2),
    io:format("Result 1: ~p~nResult 2: ~p~n",[A,B]).


info(1) ->
    [{barcelona,34},{madrid,26},{madrid,22},{barcelona,37},{teruel,-5}, {teruel, 14}, {madrid, 38}, {teruel, -8}, {barcelona,30},{teruel,10}];
info(2) ->
    "Esto es una prueba para contar palabras porque contar palabras no es una tarea facil para una prueba hay que cambiar la funcion split cambiar la funcion split no es moco que hay que dividir todo esto en palabras son palabras no palabritas".

map1(_Id,{C,T}) ->
    if T >= 28 ->
	    {C,T};
       true ->
	    none
    end.

reduce1(_Id,{C,V}) ->
    {C,lists:max(V)}.

splitText([Text],N) ->
    L = string:tokens(Text," "),
    split_N(L,N).

map2(_Id,W) ->
    {W,1}.
reduce2(_Id,{C,V}) ->
    {C,lists:sum(V)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% misc functions
% Funciones que no tienen que ver directamente con el admin de mapReduce

% divide L en N partes iguales, si Length(L) < N serán length(L) partes y no N
split_N(L,N) ->
    Len = ceil(length(L)/N),
    split(L,Len).

% divide L en partes de tamaño N
split([], _) -> [];
split(L, N) when length(L) < N -> [L];
split(L, N) ->
    {A, B} = lists:split(N, L),
    [A | split(B, N)].

% aproxima a entero superior
ceil(X) when X < 0 ->
    trunc(X);
ceil(X) ->
    T = trunc(X),
    if X == T -> T;
       true -> T + 1
    end.
