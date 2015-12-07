-module(player).
% export basics
-export([start/1,start/2]).
-export([loop/1]).


start(Node) ->
    start(Node,federico).

start(Node,Name)->
    Self = self(),
    {serverGame,Node} ! {Self,{join,Name}},
    receive
	{From,ok} ->
	    io:format("Connected!~n",[]),
	    loop(From,Name)
    end.

loop(From,Name) ->
    receive
	{endGame,Text,Stats} ->
	    io:format("~p",[Text]),
	    print_stats(Stats);
	{From,{word,W}} ->
	    io:format("New word: ~p~n",[W]),
	    loop(From,Name);
	{From,{start,W}} ->
	    io:format("Game Start!!!~n",[]),
	    io:format("New word: ~p~n",[W]),
	    write_word(From),
	    loop(From,Name);
	{From,ping} ->
	    From ! pong,
	    loop(From,Name);
	{From,{answer,error,W}} ->
	    io:format("Sorry!!~n",[]),
	    io:format("Current word: ~p~n",[W]),
	    write_word(From),
	    loop(From,Name);
	{From,{answer,correct}} ->
	    io:format("Congratulations!!~n",[]),
	    loop(From,Name);
	{From,{Player,wins}} ->
	    if Player =/= Name ->
		    io:format("Sorry, player ~p wins~n",[Player]);
	       true ->
		    ok
	    end,
	    loop(From,Name);
	dummy ->
	    loop(From,Name)
		%%RECIBIR respuesta del envio de la palabra y hacer write word
    end.


write_word(From) ->
     spawn(fun() ->
		   io:format("Write ~p",[W]),
		   W0 = io:get_line("> "),
		   W1 = string:substr(W0,1,string:len(W0)-1),
		   From ! {self(),{word,W1}}
	   end).
       
print_stats([]) ->
    io:format("-------------~n",[]);
print_stats([{P,Points}|Stats]) ->
    io:format("~p has ~p points.~n",[P,Points]),
    print_stats(Stats).
    
