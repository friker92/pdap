
-module(boss).
% export gen_server interface functions
-export([init/1,terminate/2,handle_call/3,handle_cast/2,handle_info/2,code_change/3]).
-behaviour(gen_server).
-export([ejemplo/0,iniciar/0,nuevo_trabajo/1,obtener_trabajo/0,trabajo_terminado/1]).
-define(SERVERNAME, ?MODULE).

% para ejecutar un ejemplo hacer:

% f(),c(boss),boss:ejemplo().


ejemplo() ->
    iniciar(),
    obtener_trabajo(),
    [nuevo_trabajo(fun()->nt(N)end) || N <-lists:seq(1,4)],
    [obtener_trabajo() || _ <-lists:seq(1,5)].
    

nt(N) ->
    io:format("Ejecutando tarea ~p~n",[N]),
    receive
    after 1000 -> case random:uniform(4) of
		      N ->
			  io:format("Finalizando tarea ~p~n",[N]);
		      _ ->
			  nt(N)
		  end
    end.


% lanza el servidor
iniciar() ->
    gen_server:start_link({local,?SERVERNAME},?MODULE, {}, []).

% interfaz para pedir un trabajo
obtener_trabajo() ->
    spawn(fun()->
		  case gen_server:call(?SERVERNAME,obtener_trabajo) of
		      no_more_jobs ->
			  io:format("No hay mas trabajos~n");
		      {Ref,F}->
			  F(),
			  trabajo_terminado(Ref)
		  end
	  end).
% interfaz para crear un trabajo
nuevo_trabajo(F) ->
    gen_server:cast(?SERVERNAME,{nuevo_trabajo,F}).

%interfaz para indicar que se ha acabado un trabajo
trabajo_terminado(Ref) ->
    case gen_server:call(?SERVERNAME,{trabajo_terminado,Ref}) of
	ok ->
	    io:format("Trabajo ~p, ACABADO!~n",[Ref]);
	error ->
	    io:format("Trabajo ~p, ERROR! ~n",[Ref])    
    end.


% inicia el servidor sin trabajos pendientes y sin trabajos en proceso
init(_) ->
    {ok,{[],[]}}.


%
handle_call(obtener_trabajo,_From,{[],Doing}) ->
    {reply, no_more_jobs,{[],Doing}};
handle_call(obtener_trabajo,{From,_R},{[P|Pending],Doing}) ->
    Ref = make_ref(),
    io:format("Tarea: ~p para: ~p~n",[Ref,From]),
    {reply,{Ref,P},{Pending,[{Ref,From}|Doing]}};

%
handle_call({trabajo_terminado, Ref},{From,_R},{Pending,Doing}) ->
    case lists:keyfind(Ref,1,Doing) of
	{Ref,From} ->
	    D2 = lists:keydelete(Ref,1,Doing),
	    {reply,ok,{Pending,D2}};
	_ ->
	    {reply,error,{Pending,Doing}}
    end.

%
handle_cast({nuevo_trabajo,F},{Pending,Doing}) ->
    {noreply,{Pending++[F],Doing}};

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
