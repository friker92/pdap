-module(eco_client) .
-export([print/1, start/0, stop/0]).


print(Term) ->
    eco ! {self(), Term} .

start() ->
    eco_server:start() .

stop() ->
    eco ! {stop} .
