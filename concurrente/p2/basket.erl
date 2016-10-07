-module(basket).
-export([start/3,loop/2,start_master/2]).


start(M,N) ->
    create_master(M,N).

create_master(M,N) ->
    spawn(fun() -> start_master(M,N) end).
create(0,List,_)->
    List;
create(N,List,Centro)->
    Pid = register(list_to_atom(["player",N]),spawn(fun() -> receive
								 {Centro,Players} ->
								     io:format("Process ~p(~p) is playing~n",[N,self()]), 
								     loop_player(N,Players) 
							     end 
						    end)),
    create(N-1,[Pid|List],Centro).

loop_player(Id,Players) ->
    receive
	{From,FId,ball} -> 
	    io:format("Player ~p(~p) Receive ball from ~p(~p)~n",[Id,self(),FId,From]),
	    send_ball(self(),Players),
	    loop_player(Id,Players);
	{From,FId,ball} -> 
	    io:format("Process ~p: Receive ~p~n Y MUERE ~n",[Id,Msg]),
	    Center ! {self(),ok}
    end.


start_master(M,N)->
    Players = create(N,[],self()),
    send_all({self(),Players},Players),
    loop_master(M,Players).

loop_master(0,List) ->
    io:format("Master Looking the GAME~n",[]),
    receive
	{stop} ->
	    stopGame(List)
    after 
	30000 ->
	    stopGame(List)
    end;
loop_master(M,List) ->
    io:format("Im the Master with ~p balls.~n",[M]),
    send_ball(self(),List),
    loop_master(M-1,List).

stopGame(List) ->
    send_all({self(),stop},List),
    wait_all(List).



send_all(_,[]) ->
    true;
send_all(Token,[P|Rest]) ->
    send_all(P!Token,Rest).
wait_all([]) ->
    true;
wait_all([P|Rest]) ->
    receive
	{P,ok}->wait_all(Rest)
    end.
