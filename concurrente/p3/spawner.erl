-module(spawner).
-export([my_spawn/3,my_spawn_monitor/3,my_spawn_on_exit/3,my_spawn_time/4,proceso/0]).

my_spawn(Mod,Func,Args) ->
	Self = self(),
	C = spawn(fun() -> 
		{T,ok} = timer:tc(fun()->
				process_flag(trap_exit,true),
				Pid = spawn_link(Mod,Func,Args),
				Self ! {self(),Pid},
				io:format("PID: ~p~n",[Pid]),
				receive 
					X -> io:format("R: ~p~n",[X])
				end
			end),
		io:format("In time: ~p~n",[T])
	end),
	receive
		{C,Pid} -> Pid
	end.

my_spawn_monitor(Mod,Func,Args) ->
	Pid = spawn(Mod,Func,Args),
	spawn(fun() -> 
		erlang:monitor(process, Pid),
		{T,ok} = timer:tc(fun()->
				receive 
					X -> io:format("R: ~p~n",[X])
				end
			end),
		io:format("In time: ~p~n",[T])
	end),
	Pid.
	
my_spawn_on_exit(Mod,Func,Args) ->
	Pid = spawn(Mod,Func,Args),
	Tim = spawn(fun() ->
			{T,ok} = timer:tc(fun()->
			receive
				X -> io:format("R: ~p~n",[X])
			end
			end),
		io:format("In time: ~p~n",[T])end),
	onExit(Pid,fun(W) ->
			Tim!W
	end).

onExit(Pid,Fun) ->
	spawn(fun() ->
		Ref = erlang:monitor(process,Pid),
		receive
			{'DOWN',Ref,process,Pid,Why} ->
				Fun(Why)
		end
	end).

	
	
my_spawn_time(Mod,Func,Args,Time) ->
	Pid = spawn(Mod,Func,Args),
	spawn(fun()->
		erlang:monitor(process, Pid),
		receive
			X -> io:format("R: ~p~n",[X])
		after
			Time ->
				exit(Pid, kill),
				receive
					X -> io:format("R: ~p~n",[X])
				end
		end
	end),
	Pid.
	
proceso() ->
	io:format("Estoy vivo"),
	receive
		stop -> 
			exit("por stop")
	after
		5000 ->
			proceso()
	end.