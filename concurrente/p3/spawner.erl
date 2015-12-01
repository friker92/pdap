-module(spawner).
-export([my_spawn/3,my_spawn_monitor/3,my_spawn_on_exit/3,my_spawn_time/4,proceso/0]).

% -----------
% ejercicio 1
% -----------
% primera forma sin monitor
my_spawn(Mod,Func,Args) ->
    Self = self(),
    % creamos el proceso que medira el tiempo
    C = spawn(fun() ->
		      % comienza el temporizador
		      {T,ok} = timer:tc(fun()->
				% activar la recepcion de signals
				process_flag(trap_exit,true),
				% spawn que vamos a medir enlazado con el proceso que lo mide
				Pid = spawn_link(Mod,Func,Args),
				% mandamos el Pid del proceso al self para devolverlo
				Self ! {self(),Pid},
				% queda el proceso esperando el exit
				receive 
				    % imprime las causas de la salida
				    X -> io:format("R: ~p~n",[X])
				end
			end),
		% imprime el tiempo de vida del proceso medido
		io:format("In time: ~p~n",[T])
	end),
	receive
	    % self se queda esperando para recibir el pid 
	    {C,Pid} -> Pid
	end.

% ejercicio 1 con monitor
% sigue la misma estructura que antes, 
% pero en vez de linkar el proceso a medir con el medidor
% el segundo se hace monitor del otro.
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


% -----------
% ejercicio 2
% -----------
% misma estructura que el ejercicio 1 con monitor, 
% pero usando la funcion onExit
my_spawn_on_exit(Mod,Func,Args) ->
    % proceso a medir
    Pid = spawn(Mod,Func,Args),
    % proceso medidor queda esperando al que espera el exit
    Tim = spawn(fun() ->
			{T,ok} = timer:tc(fun()->
			receive
			    X -> io:format("R: ~p~n",[X])
			end
			end),
			io:format("In time: ~p~n",[T])end),
    % proceso que espera el exit del proceso a medir
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

	

% -----------
% ejercicio 3
% -----------
% igual que el ejercicio 1 con monitor
% pero con un after que mande matar el proceso si excede.
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

% -----------
% ejercicio 3
% -----------
my_spawn_alive(Mod,Func,Args,Time) ->
    Pid = spawn(Mod,Func,Args),
    monitoriza(Pid,Mod,Func,Args),
    Pid.

monitoriza(Pid,Mod,Func,Args) ->
    MonRef = erlang:monitor(process, Pid),
    receive
	{'DOWN',MonRef,process,Pid,Reason} -> 
	    io:format("R: ~p -> ~p~n",[Pid,Reason]),
	    NewPid = spawn(Mod,Func,Args),
	    io:format("Echo pid ~p~n",[NewPid]),
	    monitoriza(Pid,Mod,Func,Args);
	{'DOWN',MonRef,process,Pid,normal} -> 
	    io:format("END~n",[])
    end.
	     


	
proceso() ->
	io:format("Estoy vivo"),
	receive
		stop -> 
			exit("por stop")
	after
		5000 ->
			proceso()
	end.
