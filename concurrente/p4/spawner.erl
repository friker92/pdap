-module(spawner).
-export([start/0,stop/0,spawn_register/2,loop/0]).


start() ->
    register(sregis,spawn(fun() ->
				  loop()
			  end)).
stop() ->
    W = whereis(sregis),
    if  W =/= undefined ->
	    sregis ! {self(),stop},
	    receive
		{ok} ->
		    ok
	    after 2000 ->
		    ok
	    end;
	true ->
	    ok
    end.

loop() ->
    receive 
	{From,register,Name,Fun} ->
	    W = whereis(Name),
	    if W =/= undefined ->
		    From ! {error,"Already exists"};
	       true ->
		    Pid = spawn(Fun),
		    register(Name,Pid),
		    From ! {ok,Pid}
	    end,
	    loop();
	 {From,stop} ->
	    From ! {ok}
    end.

spawn_register(Name,Fun) ->
    W = whereis(sregis),
    if W == undefined ->
	    io:format("Please run first: spawner:start() ~n",[]);
       true ->
	    sregis ! {self(),register,Name,Fun},
	    receive
		{error,Reason} ->
		    io:format("An error happens:~n\t~p~n",[Reason]),
		    -1;
		{ok,Pid} ->
		    Pid
	    end
    end.
		
	    
