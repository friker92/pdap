-module(pingpong).

-export([start/1, ping/1, pong/0]).
-export([start_sec/1, ping_sec/2, pong_sec/0]).

ping(0) ->
    pong ! stop,
    io:format("ping termina~n", []);

ping(N) ->
    pong ! {self(), ping},
    receive
        pong ->
            io:format("Recibe pong~n", []),
	    ping(N - 1)
    end.


pong() ->
    receive
        stop ->
            io:format("pong termina~n", []);
        {Ping, ping} ->
            io:format("Recibe ping~n", []),
            Ping ! pong,
            pong()
    end.

start(N) ->
    register(pong, spawn(pingpong, pong, [])),
    spawn(pingpong, ping, [N]).




start_sec(N) ->
    spawn(pingpong, ping_sec, [spawn(pingpong, pong_sec, []),N]).

    
ping_sec(Pong, 0) ->
    Pong ! stop,
    io:format("ping termina~n", []);

ping_sec(Pong, N) ->
    Pong ! {self(), ping},
    receive
        {Pong, pong} ->
            io:format("Recibe pong~n", []),
	    ping_sec(Pong, N - 1)
    end.


pong_sec() ->
    receive
        stop ->
            io:format("pong termina~n", []);
        {Ping, ping} ->
            io:format("Recibe ping~n", []),
            Ping ! {self(),pong},
            pong_sec()
    end.
