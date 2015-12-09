-module(player).
% export basics
-export([start/1,start/2,start/3]).
-export([loop/2]).


start(Node) ->
    start(Node,serv,federico).
start(Node,{name,Name}) ->
    start(Node,serv,Name);
start(Node,{server,Serv}) ->
    start(Node,Serv,federico).

start(Node,Server,Name)->
    Self = self(),
    {Server,Node} ! {Self,{join,Name}},
    receive
	{From,ok} ->
	    io:format("Connected!~n",[]),
	    loop(From,Name);
	{_,error,Reason} ->
	    io:format("Impossible! Reason: ~p~n",[Reason])
    after 2000 ->
	    io:format("Impossible! Reason: Unrecharchable~n",[])
    end.



loop(From,Name) ->
    Self = self(),
    receive
	stop ->
	    io:format("Bye Bye~n");
	points ->
	    From ! {Self,{getpoints,Name}},
	    loop(From,Name);
	stats ->
	    From ! {Self,{getpoints,all}},
	    loop(From,Name);
	{stats,Stats} ->
	    io:format("~nPoints: ~p~n",[Stats]),
	    loop(From,Name);
	{endGame,Text,Stats} ->
	    io:format("~p",[Text]),
	    print_stats(Stats);
	{From,{word,W}} ->
	    write_word(From,Self,W),
	    loop(From,Name);
	{From,{start,W}} ->
	    io:format("Game Start!!!~n",[]),
	    write_word(From,Self,W),
	    loop(From,Name);
	{From,ping,Respond} ->
	    Respond ! {Name,pong},
	    loop(From,Name);
	{_,{answer,error,W}} ->
	    io:format("Sorry!!~n",[]),
	    write_word(From,Self,W),
	    loop(From,Name);
	{From,{answer,correct,Points}} ->
	    io:format("Congratulations yo win ~p points!~n",[Points]),
	    loop(From,Name);
	{From,{Player,wins}} ->
	    if Player =/= Name ->
		    io:format("~nSorry, player ~p wins~n",[Player]);
	       true ->
		    ok
	    end,
	    loop(From,Name);
	X ->
	    io:format("F:~p~n~p~n",[From,X]),
	    loop(From,Name)
    end.


write_word(From,Self,W) ->
    spawn(fun() ->
		  io:format("Write ~p",[W]),

		  W0 = io:get_line("> "),

		  Token = string:substr(W0,1,1),
		  if Token =/= "#" ->
			  W1 = string:substr(W0,1,string:len(W0)-1),			  
			  From ! {Self,{word,W1}};
		     true ->
			  W1 = string:substr(W0,2,string:len(W0)-2),			  
			  io:format("~p~n",[W1]),
			  Self ! list_to_atom(W1),
			  write_word(From,Self,W)
		  end
	  end).
       
print_stats([]) ->
    io:format("-------------~n",[]);
print_stats([{P,Points}|Stats]) ->
    io:format("~p has ~p points.~n",[P,Points]),
    print_stats(Stats).
    
