-module(spawner).
-export([my_spawn/3,my_spawn_monitor/3,my_spawn_on_exit/3,my_spawn_time/4,proceso/0]).
-export([my_spawn_alive/3,my_spawn_alive_N/4,my_spawn_one_death_N/4]).

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
% ejercicio 4
% -----------
% utilizar la funcion "proceso" para este apartado (igual que en los anteriores)
my_spawn_alive(Mod,Func,Args) ->
    % se crea el proceso
    Pid = spawn(Mod,Func,Args),
    % se monitoriza
    spawn(fun()->monitoriza(Pid,Mod,Func,Args) end),
    Pid.

monitoriza(Pid,Mod,Func,Args) ->
    % al recibir un Pid lo monitoriza
    erlang:monitor(process, Pid),
    receive
	% este caso es solo para poder ejecutar las pruebas
	stop ->
	    Pid!stop,
	    io:format("END~n",[]);
	% si recibe una muerte del proceso lo relanza
	X ->
	    io:format("R: ~p~n",[X]),
	    NewPid = spawn(Mod,Func,Args),
	    io:format("Relanza con pid ~p~n",[NewPid]),
	    % realiza el bucle con el nuevo Pid del proceso
	    monitoriza(NewPid,Mod,Func,Args)
    end.

% -----------
% ejercicio 5
% -----------
% para simplificar la llamada se lanzará N veces la misma funcion
% para generalizarlo debe recibir una lista de modulos funciones y argumentos
my_spawn_alive_N(N,Mod,Func,Args) ->
    Monitor = spawn(fun() ->
			   monitor_N([],[],[],[])
		    end),
    spawn_N(Monitor,N,Mod,Func,Args).

% si fueran N diferentes la cabecera sería:
% spawn_N(Monitor,1,[Mod],[Func],[Args]) ->
spawn_N(Monitor,1,Mod,Func,Args) ->
    Pid = spawn(Mod,Func,Args),
    Monitor!{monitoriza,Pid,Mod,Func,Args},
    ok;
% si fueran N diferentes la cabecera sería:
% spawn_N(Monitor,N,[Mod|Mods],[Func|Funcs],[Args|Argss]) ->
spawn_N(Monitor,N,Mod,Func,Args) ->
    Pid = spawn(Mod,Func,Args),
    Monitor!{monitoriza,Pid,Mod,Func,Args},
    % para N diferentes la llamada recursiva se realiza con el resto de la lista
    spawn_N(Monitor,N-1,Mod,Func,Args).
    
monitor_N(Pids,Mods,Funcs,Argss)->
    receive
	{monitoriza,Pid,M,F,A} ->
	    io:format("monitorizando ~p~n",[Pid]),
	    erlang:monitor(process, Pid),
	    % y lo añadimos a la lista para poder relanzarlo
	    monitor_N([Pid|Pids],[M|Mods],[F|Funcs],[A|Argss]);
	{'DOWN',_,process,Pid,Reason} ->
	    io:format("Pid ~p ha muerto. Razon: ~p~n",[Pid,Reason]),
	    % relaunch modifica las listas borrando el antiguo pid
	    {NewPid,Ps,Ms,Fs,As} = relaunch(Pid,Pids,Mods,Funcs,Argss),
	    erlang:monitor(process,NewPid),
	    io:format("Relanzado con nuevo Pid: ~p~n",[NewPid]),
	    monitor_N(Ps,Ms,Fs,As);
	stop ->
	    io:format("adios~n",[])
    end.

% relaunch busca el pid en la lista de pids
% y va avanzando en el resto para poder relanzar
% salva una copia de la cabecera para poder devolver 
% la lista completa actualizada
relaunch(P,Ps,Ms,Fs,As) ->
    relaunch(P,[],Ps,[],Ms,[],Fs,[],As).

% Xi es la lista izquierda de X
% Xa es el elemento actual de X
% Xd es la lista derecha de X
% P es el Pid buscado
% Nunca deberia pasar del caso en que Xd = []
relaunch(P,Pi,[Pa|Pd],Mi,[Ma|Md],Fi,[Fa|Fd],Ai,[Aa|Ad]) ->
    if 
	P == Pa ->
	    NewPid = spawn(Ma,Fa,Aa),
	    {NewPid,Pi++[NewPid|Pd],Mi++[Ma|Md],Fi++[Fa|Fd],Ai++[Aa|Ad]};
	true ->
	    relaunch(P,Pi++[Pa],Pd,Mi++[Ma],Md,Fi++[Fa],Fd,Ai++[Aa],Ad)
    end.

% -----------
% ejercicio 6
% -----------
% para simplificar la llamada se lanzará N veces la misma funcion
% para generalizarlo debe recibir una lista de modulos funciones y argumentos
% igual que en el ejercicio 5
my_spawn_one_death_N(N,Mod,Func,Args) ->
    Monitor = spawn(fun() ->
			   monitor_one_death_N([])
		    end),
    spawn_N(Monitor,N,Mod,Func,Args).
	

monitor_one_death_N(Pids) ->
    receive
	{monitoriza,Pid,_,_,_} ->
	    io:format("monitorizando ~p~n",[Pid]),
	    erlang:monitor(process, Pid),
	    % y lo añadimos a la lista para poder matarlos
	    monitor_one_death_N([Pid|Pids]);
	{'DOWN',_,process,Pid,Reason} ->
	    io:format("Pid ~p ha muerto. Razon: ~p~n",[Pid,Reason]),
	    % manda un kill a toda la lista menos a Pid
	    kill_all_except_one(Pid,Pids);
	stop ->
	    io:format("adios~n",[])
    end.
kill_all_except_one(Pid,[]) ->
    io:format("Todos matados excepto ~p que ya estaba muerto~n",[Pid]);

kill_all_except_one(Pid,[P|Pids]) ->
    if
	Pid == P ->
	    kill_all_except_one(Pid,Pids);
	true ->
	    exit(P, kill),
	    kill_all_except_one(Pid,Pids)
    end.
    


proceso() ->
	io:format("Estoy vivo ~p~n",[self()]),
	receive
		stop -> 
			exit("por stop")
	after
		5000 ->
			proceso()
	end.
