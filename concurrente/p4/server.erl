-module(server).
% export interface -> serv
-export([start/0,players/0,addWord/1,points/0,player_points/1,stop/0]).
% export generic interface 
-export([start/1,players/1,addWord/2,points/1,player_points/2,stop/1]).
% export others
-export([loop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                       Words Game                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Interface -> serv

start() ->
    spawner:start(),
    start(serv).

players() ->
    players(serv).

addWord(Word) ->
    addWord(serv,Word).

points() ->
    points(serv).

player_points(Player) ->
    player_points(serv,Player).

stop() ->
    stop(serv).


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
    player_points(Server,all).

player_points(Server,Player) ->
    Server ! {self(),{getPoints,Player}},
    receive
	{_,points,Points} ->
	    io:format("~p~n",[Points])
    end.

stop(Server) ->
    Server ! stop.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Server

loop(Players, Words, Actual) ->
    Self = self(),
    receive
	play ->
	    if Actual =/= undefined ->
		    loop(Players,Words,Actual);
	       true ->
		    W = new_word(Words,Players),
		    loop(Players,Words,W)
	    end;
	players ->
	    io:format("players: ~p~n",[Players]),
	    loop(Players,Words,Actual);
	stop ->
	    Stats = get_stadistics(Players,all),
	    async_send_all(Players,{endGame,"bye, thanks for playing.~n",Stats}),
	    io:format("bye, thanks for playing.~n",[]);
	{addWord,W} ->
	    loop(Players,[W|Words],Actual);
	{removeWord,W} ->
	    loop(Players,[W|Words],Actual);
	{removePlayer,Name} ->
	    Play = remove_player(Players,Name),
	    loop(Play,Words,Actual);
	{From,{getpoints,Player}} ->
	    Stats = get_stadistics(Players,Player),
	    From ! {stats,Stats},
	    loop(Players,Words,Actual);
	{From,{join,Name}} ->
	    Exist = check_player_exists(Players,Name),
	    if Exist =/= true ->
		    io:format("New player: ~p (~p)~n",[Name,From]),
		    From ! {Self,ok},
		    if Actual =/= undefined ->
			    From ! {Self,{word,Actual}};
		       true ->
			    ok
		    end,
		    loop([{From,Name,0}|Players],Words,Actual);
	       true ->
		    io:format("Player name repeated: ~p (~p)~n",[Name,From]),
		    From ! {Self,error,"This name already exists!"},
		    loop(Players,Words,Actual)
	    end;
	{From,{word,W}} ->
	    if W =/= Actual ->
		    From ! {Self,{answer,error,Actual}},
		    loop(Players,Words,Actual);
	       true ->
		    Points = length(Actual),
		    From ! {Self,{answer,correct,Points}},
		    Name = get_name(Players,From),
		    Play = add_points(Players,Name,Points),
		    async_send_all(Play,{Self,{Name,wins}}),
		    W2 = new_word(Words,Play),
		    loop(Play,Words,W2)
	    end
    after 3000 ->
	    check_alive(Players,self()),
	    loop(Players,Words,Actual)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% misc functions

async_send_all(List,Msg) ->
    spawn(fun() ->
		  send_all(List,Msg)
	  end).

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
    send_all(Players,{self(),{word,W}}),
    W.

get_name([],_) ->
    "Name not found";
get_name([{Pid,Name,_}|P],From) ->
    if From =/= Pid ->
	    get_name(P,From);
       true ->
	    Name
    end.
check_player_exists([],_) ->
    false;
check_player_exists([{_,Pname,0}|Players],Name) ->
    if Pname =/= Name ->
	    check_player_exists(Players,Name);
       true ->
	    true
    end.

get_stadistics(Players,all) ->
    lists:map(fun({_,N,P}) -> {N,P} end,Players);
get_stadistics([{_,Name,Points}|List],Player) ->
    if Name == Player ->
	    {Player,Points};
       true ->
	    get_stadistics(List,Player)
    end.

remove_player([],_) ->
    [];
remove_player(Players,Name) ->
    remove_player_acumulative([],Players,Name).

remove_player_acumulative(P,[],_) ->
    P;
remove_player_acumulative(Izq,[{Pid,Pname,Points}|Players],Name) ->
    if Pname =/= Name ->
	    remove_player_acumulative([{Pid,Pname,Points}|Izq],Players,Name);
       true ->
	    Izq++Players
    end.
	    
check_alive([],_) -> 
    ok;
check_alive([{P,N,_}|Players],From) ->
    spawn(fun() ->
		  P ! {From,ping,self()},
		  receive
		      {N,pong} ->
			  ok
		  after 3000 ->
			  From ! {removePlayer,N}
		  end
	  end),
    check_alive(Players,From).

add_points([],_,_) ->
    [];
add_points(Players,Name,Points) ->
    add_points_acumulative([],Players,Name,Points).

add_points_acumulative(Izq,[],_,_) ->
    Izq;
add_points_acumulative(Izq,[{Ppid,Pname,Ppoint}|Players],Name,Points) ->
    if Pname =/= Name ->
	    add_points_acumulative([{Ppid,Pname,Ppoint}|Izq],Players,Name,Points);
       true ->
	    Izq++[{Ppid,Pname,Ppoint+Points}|Players]
    end.
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% at the end, loop with "too many" words
loop() ->
    loop([],["hola","adios","caracola","juego","divertido"],undefined),
    io:format("muero2~n").
