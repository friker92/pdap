-module(ring).
-export([start/3,loop/2]).


start(M,N,Msg) ->
	Pid = create(N),
	Pid ! {M-1,N,Msg}.


create(N) ->
	  spawn(fun() -> Self = self(),loop(N,create(N-1,Self)) end).
create(1,Primero)->
	  spawn(fun() -> loop(1,Primero) end);
create(N,Primero)->
	  spawn(fun() -> loop(N,create(N-1,Primero)) end).

loop(Id,Next) ->
	io:format("Process ~p is waiting ~p ~n",[Id,Next]), 
	 receive
		{0,0,Msg} -> 
			io:format("Process ~p: Receive ~p~n Y SE PARA EL ENVIO~n",[Id,Msg]);
		{0,N,Msg} -> 
			io:format("Process ~p: Receive ~p~n Y MUERE ~n",[Id,Msg]),
			Next ! {0, N-1, Msg};
		{M,0,Msg} -> 
			io:format("Process ~p: Receive ~p~n Y OTRA VUELTA~n",[Id,Msg]),
			Next ! {M-1, Id, Msg},
			loop(Id,Next);
		{M,N,Msg} -> 
			io:format("Process ~p: Receive ~p~n",[Id,Msg]),
			Next ! {M, N-1, Msg},
			loop(Id,Next)
	end.

