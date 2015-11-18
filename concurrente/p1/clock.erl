-module(clock).
-export([reloj/1, stop/0]) .

reloj(Time) ->
    register(clock,spawn(fun()->
				 tick(Time) end)) .

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
