-module(player).
% export basics
-export([start/1,start/2,start/3]).
-export([loop/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Generic interface

% inicio el jugador:
% llamandolo federico 
% y al servidor registrado en serv
start(Node) ->
    start(Node,serv,federico).

% inicio el jugador:
% llamandolo como quieras
% y al servidor registrado en serv
start(Node,{name,Name}) ->
    start(Node,serv,Name);

% inicio el jugador:
% llamandolo federico 
% y al servidor registrado en donde le indiques
start(Node,{server,Serv}) ->
    start(Node,Serv,federico).

% inicio el jugador:
% llamandolo como quieras
% y al servidor registrado en donde le indiques
start(Node,Server,Name)->
    Self = self(),
    % envio de mensajes a otro nodo
    {Server,Node} ! {Self,{join,Name}},
    receive
	{From,ok} ->
	    % si el servidor le acepta empezamos el bucle
	    io:format("Connected!~n",[]),
	    loop(From,Name);
	{_,error,Reason} ->
	    % si no nos acepta nos vamos
	    io:format("Impossible! Reason: ~p~n",[Reason])
    after 2000 ->
	    % si tarda mucho en aceptarnos decimos que es innalcanzable
	    io:format("Impossible! Reason: Unrecharchable~n",[])
    end.


% loop del player, con la referencia al servidor y su nombre de jugador
loop(From,Name) ->
    Self = self(),
    receive
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%      Mensajes básicos      %%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	stop ->
	    % para al jugador
	    io:format("Bye Bye~n");
	points ->
	    % el jugador pide sus puntos
	    From ! {Self,{getpoints,Name}},
	    loop(From,Name);
	stats ->
	    % el jugador pide todos los puntos
	    From ! {Self,{getpoints,all}},
	    loop(From,Name);

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%      Mensajes Server       %%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	{From,{endGame,Text,Stats}} ->
	    % el servidor dice que termina el juego
	    io:format("~p",[Text]),
	    print_stats(Stats);

	{From,{stats,Stats}} ->
	    % el servidor me ha mandado los puntos del juego
	    % de uno o todos los jugadores
	    io:format("~nPoints: ~p~n",[Stats]),
	    loop(From,Name);
	{From,{word,W}} ->
	    % el servidor nos manda una nueva palabra a escribir
	    write_word(From,Self,W),
	    loop(From,Name);
	{From,ping,Respond} ->
	    % si alguien con autoridad, conoce el From, nos hace ping
	    % le devolvemos un pong
	    Respond ! {Name,pong},
	    loop(From,Name);
	{_From,{answer,error,W}} ->
	    % el servidor nos dice que hemos fallado y nos recuerda la palabra
	    io:format("Sorry!!~n",[]),
	    write_word(From,Self,W),
	    loop(From,Name);
	{From,{answer,correct,Points}} ->
	    % el servidor nos dice que hemos acertado y el numero de puntos ganados
	    io:format("Congratulations yo win ~p points!~n",[Points]),
	    loop(From,Name);
	{From,{Player,wins}} ->
	    % el servidor nos dice que alguien ha acertado
	    if Player =/= Name ->
		    % si no somos nosotros lo notificamos
		    io:format("~nSorry, player ~p wins~n",[Player]);
	       true ->
		    ok
	    end,
	    loop(From,Name);
	_ ->
	    % descartamos los mensajes q no son validos
	    loop(From,Name)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% misc functions

% pedimos al jugador que escriba una palabra
write_word(From,Self,W) ->
    % lanzamos un proceso para no bloquear la recepcion de mensajes
    spawn(fun() ->
		  % mostrar mensaje con la palabra a escribir
		  io:format("Write ~p",[W]),
		  % solicitar un string
		  W0 = io:get_line("> "),
		  % el primer char nos indica si es un comando o no
		  Token = string:substr(W0,1,1),
		  if Token =/= "#" ->
			  % si el token no es # quitamos el intro
			  W1 = string:substr(W0,1,string:len(W0)-1),
			  % y mandamos la palabra con el Pid del jugador, no de este proceso
			  From ! {Self,{word,W1}};
		     true ->
			  % si el token es # estamos metiendo un comando
			  % quitamos # y el intro final
			  W1 = string:substr(W0,2,string:len(W0)-2),
			  % y mandamos el atom a "nosotros mismos" (no el proceso sino el player)
			  Self ! list_to_atom(W1),
			  % y continuamos solicitando una palabra
			  write_word(From,Self,W)
		  end
	  end).



% pinta bonito las estadisticas
print_stats([]) ->
    io:format("-------------~n",[]);
print_stats([{P,Points}|Stats]) ->
    io:format("~p has ~p points.~n",[P,Points]),
    print_stats(Stats).
    
