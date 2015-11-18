-module(star).
-export([start/2,loop/2,loop_center/3]).


start(N,Msg) ->
	create(N-1,Msg).

create(N,Msg) ->
	spawn(fun() -> loop_center(N+1,Msg,create(N-1,[],self())) end).
create(0,List,_)->
	List;
create(N,List,Centro)->
	  Pid = spawn(fun() -> loop(N,Centro) end),
	  create(N-1,[Pid|List],Centro).

loop(Id,Center) ->
	io:format("Process ~p is waiting ~p ~n",[Id,Center]), 
	 receive
		{Center,Msg} -> 
			io:format("Process ~p: Receive ~p~n",[Id,Msg]),
			Center ! {self(),ok},
			loop(Id,Center);
		{Center,Msg,last} -> 
			io:format("Process ~p: Receive ~p~n Y MUERE ~n",[Id,Msg]),
			Center ! {self(),ok}
	end.


loop_center(1,Msg,List) ->
	send_all({self(),Msg,last},List),
	wait_all(List);
loop_center(N,Msg,List) ->
	send_all({self(),Msg},List),
	wait_all(List),
	loop_center(N-1,Msg,List).

send_all(_,[]) ->
	true;
send_all(Token,[P|Rest]) ->
  P!Token,
	send_all(Token,Rest).
wait_all([]) ->
	true;
wait_all([P|Rest]) ->
	receive
	{P,ok}->wait_all(Rest)
	end.
