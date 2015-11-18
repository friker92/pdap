-module(clock).
-export([start/1, stop/0]) .

start(Time) ->
    L = whereis(clock),
    if L == undefined ->
        register(clock,spawn(fun()->
				 tick(Time) end));
    	true -> "already exists"
    end .

stop() ->
    clock ! stop.


tick(Time) ->
    receive
	stop ->
	    void
    after
	Time ->
	    {{_,_,_},{H,M,S}} = erlang:localtime(),
	    io:format("Hora: ~p~n",[{H,M,S}]) ,
	    tick(Time)
    end.
