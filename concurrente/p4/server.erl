-module(server).
% export interface -> serverGame
-export([start/0,players/0,addWord/1,points/0,stop/0]).
% export generic interface 
-export([start/1,players/1,addWord/2,points/1,stop/1]).
% export others
-export([loop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                       Words Game                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Interface -> serverGame

start() ->
    start(serverGame).

players() ->
    players(serverGame).

addWord(Word) ->
    addWord(serverGame,Word).

points() ->
    points(serverGame).

stop() ->
    stop(serverGame).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Generic interface

start(Server) ->
    Pid = spawner:spawn_register(Server,fun()-> loop() end),
    Pid.

players(Server) ->
    Server ! players.

addWord(Server, Word)->
    Server ! {addWord,Word}.

points(Server) ->
    Server ! {self(),{getPoints,all}},
    receive
	{_,points,Points} ->
	    io:format("~p~n",[Points])
    end.

stop(Server) ->
    Server ! stop.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Server

loop(Players, Words, Actual) ->
    if Actual =/= undefined ->
	    io:format("Word ~p~n",[Actual]);
       true ->
	    io:format("Ready!~n",[])
    end,
    receive
	play ->
	    if Actual =/= undefined ->
		    loop(Players,Words,Actual);
	       true ->
		    W = new_word(Words,Players),
		    loop(Players,Words,W)
	    end;
	players ->
	    Players,
	    loop(Players,Words,Actual);
	stop ->
	    Stats = get_stadistics(Players,all),
	    send_all(Players,{endGame,"bye, thanks for playing.~n",Stats}),
	    io:format("bye, thanks for playing.~n",[]);
	{From,{getpoints,Player}} ->
	    Stats = get_stadistics(Players,Player),
	    From ! {stats,Stats};
	{From,{join,Name}} ->
	    io:format("New player: ~p (~p)~n",[Name,From]),
	    From ! {self(),ok},
	    loop([{From,Name,0}|Players],Words,Actual);
	{From,{word,W}} ->
	    if W =/= Actual ->
		    From ! {self(),{answer,error,Actual}};
	       true ->
		    From ! {self(),{answer,correct}},
		    send_all(Players,{self(),{From,wins}}),
		    W = new_word(Words,Players),
		    loop(Players,Words,W)
	    end
		       
    end.

get_stadistics(Players,all) ->
    lists:map(fun({_,N,P}) -> {N,P} end,Players);
get_stadistics([{_,Name,Points}|List],Player) ->
    if Name == Player ->
	    {Player,Points};
       true ->
	    get_stadistics(List,Player)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% misc functions

send_all([],_)->
    done;
send_all([{Pid,_,_}|List],Msg) ->
    Pid ! Msg,
    send_all(List,Msg).

select_random(Words) ->
    N = random:uniform(length(Words)),
    lists:nth(N,Words).

new_word(Words,Players) ->
    W = select_random(Words),
    send_all(Players,{self(),{word,W}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% at the end, loop with "too many" words
loop() ->
    loop([],["hola","adios","caracola","juego","divertido"],undefined).
