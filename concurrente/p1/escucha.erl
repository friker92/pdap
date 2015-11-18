-module(escucha).
-export([start/0, stop/0, send_msg/1,loop/0]) .

start() ->
    register(listen,spawn(escucha,loop,[])) .

stop() ->
    listen ! stop.

loop() ->
    receive
	{Client,Msg} ->
	    Client ! {listen,ok},
	    io:format("~p~n",[Msg]),
	    loop();
	stop ->
	    io:format("Stopped.~n",[])
    after 
	5000 ->
	    io:format("I'm waiting...~n",[]) ,
	    loop()
    end.

send_msg(Msg) ->
    listen ! {self(),Msg},
    receive
	{listen,ok} ->
	    ok
    end.
