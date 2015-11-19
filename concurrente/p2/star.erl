-module(star).
-export([start/3,loop/2,loop_center/3]).

%% empieza el envio
% M numero de veces
% N numero de procesos
% Msg Mensaje a pasar M veces
start(M,N,Msg) ->
    create_center(M,N,Msg).


% Crea el proceso central y este crea los N-1 restantes.
% una vez crea las puntas de la estrella pasa a su bucle
create_center(M,N,Msg) ->
    spawn(fun() -> loop_center(M,Msg,create(N-1,[],self())) end).

% create crea un proceso, si N es 0 da por terminado y devuelve
% la lista de todos los procesos para que el centro lo sepa
create(0,List,_)->
    List;
create(N,List,Centro)->
    Pid = spawn(fun() -> loop(N,Centro) end),
    create(N-1,[Pid|List],Centro).

% loop de las puntas de la estrella
% conocen el centro
% esperan un mensaje que puede llevar el atom last en ese caso mueren
loop(Id,Center) ->
    io:format("Process ~p(~p) is waiting to ~p ~n",[Id,self(),Center]), 
    receive
	{Center,Msg} -> 
	    io:format("Process ~p: Receive ~p~n",[Id,Msg]),
	    Center ! {self(),ok},
	    loop(Id,Center);
	{Center,Msg,last} -> 
	    io:format("Process ~p: Receive ~p~n Y MUERE ~n",[Id,Msg]),
	    Center ! {self(),ok}
    end.

% loop del centro, itera sobre N
% hace send_all a todas las puntas
% y luego las espera
loop_center(1,Msg,List) ->
    io:format("Boss Muere~n",[]),
    send_all({self(),Msg,last},List),
    wait_all(List);
loop_center(N,Msg,List) ->
    io:format("Im the boss:~p~n",[N]),
    send_all({self(),Msg},List),
    wait_all(List),
    loop_center(N-1,Msg,List).


% va uno a uno de la lista mandando el Token
send_all(_,[]) ->
    true;
send_all(Token,[P|Rest]) ->
    P!Token,
    send_all(Token,Rest).

% espera en orden a toda la lista
wait_all([]) ->
    true;
wait_all([P|Rest]) ->
    receive
	{P,ok}->wait_all(Rest)
    end.
