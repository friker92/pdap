-module(eco_server) .
-export([start/0, loop/0]).

start() ->
     register(eco, spawn(eco_server, loop, [])) .

loop() ->
    receive
	{_, Msg} ->
	    Msg,
	    loop();
	{stop} ->
	    void
    end.
