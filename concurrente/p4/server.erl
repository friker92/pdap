-module(server).
% export interface -> serv
-export([start/0,players/0,addWord/1,points/0,player_points/1,stop/0]).
% export generic interface 
-export([start/1,players/1,addWord/2,points/1,player_points/2,stop/1]).
% export others
-export([loop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                       Words Game                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Interface -> serv
% Todas estas funciones son para facilitar el acceso al servidor
% "serv" aunque en realidad ninguna es necesaria.

% inicia el servidor registrandolo como: serv
start() ->
    start(serv).

% pregunta los players que hay en serv
players() ->
    players(serv).

% añade una nueva palabra al conjunto de palabras de serv
addWord(Word) ->
    addWord(serv,Word).

% pregunta por los puntos de todos los jugadores
points() ->
    points(serv).


% pregunta por los puntos de un jugador concreto
player_points(Player) ->
    player_points(serv,Player).

% para el servidor
stop() ->
    stop(serv).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Generic interface
% Todas estas funciones se realizan para un sevidor "Server"
% que puede elegirse el nombre, son todo atajos para mandar señales
% al servidor. Funcionan a modo de API

% Inicia el servidor asegurando que no existe otro con el mismo nombre.
start(Server) ->
    Pid = spawner:spawn_register(Server,fun()-> loop() end),
    Pid.

% pregunta los players que hay
players(Server) ->
    Server ! players.

% añade una nueva palabra al conjunto de palabras
addWord(Server, Word)->
    Server ! {addWord,Word}.

% pregunta por los puntos de todos los jugadores
points(Server) ->
    player_points(Server,all).

% pregunta por los puntos de un jugador concreto
player_points(Server,Player) ->
    Server ! {self(),{getPoints,Player}},
    receive
	{_,points,Points} ->
	    io:format("~p~n",[Points])
    end.

% para el servidor
stop(Server) ->
    Server ! stop.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Server

% bucle principal del servidor, solo recibe mensajes
loop(Players, Words, Actual) ->
    Self = self(),
    receive
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%      Mensajes básicos      %%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	play ->
	    % iniciamos el juego si no estaba ya iniciado seleccionando una palabra
	    % que mandamos a todos los jugadores
	    if Actual =/= undefined ->
		    loop(Players,Words,Actual);
	       true ->
		    W = new_word(Words,Players),
		    loop(Players,Words,W)
	    end;
	players ->
	    % imprimimos todos los jugadores registrados
	    io:format("players: ~p~n",[Players]),
	    loop(Players,Words,Actual);
	stop ->
	    % paramos el juego
	    % lo primero es coger las puntuaciones para pasarsela a los jugadores
	    Stats = get_stadistics(Players,all),
	    % de manera asincrona le decimos a todos los jugadores que el juego ha
	    % terminado y los resultados
	    async_send_all(Players,{Self,{endGame,"bye, thanks for playing.~n",Stats}}),
	    io:format("bye, bye, game ends.~n",[]);

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%   Mensajes Configuracion   %%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	{addWord,W} ->
	    % añade una palabra a la lista de palabras posibles.
	    loop(Players,[W|Words],Actual);
	{removeWord,W} ->
	    % eliminamos una palabra de la lista
	    Word = remove_word(Words,W),
	    loop(Players,Word,Actual);
	{removePlayer,Name} ->
	    % eliminamos un jugador del juego
	    Play = remove_player(Players,Name),
	    loop(Play,Words,Actual);

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%      Mensajes Players      %%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	{From,{join,Name}} ->
	    %%% Un jugador pide unirse con un nombre
	    % Comprobamos si existe alguien con ese nombre
	    Exist = check_player_exists(Players,Name),
	    if Exist =/= true ->
		    % si no existe lo registramos
		    io:format("New player: ~p (~p)~n",[Name,From]),
		    % le decimos que le hemos aceptado
		    From ! {Self,ok},
		    % si existe una palabra que adivinar se la mandamos
		    % para que pueda jugar desde el principio
		    if Actual =/= undefined ->
			    From ! {Self,{word,Actual}};
		       true ->
			    ok
		    end,
		    % aqui es donde en realidad guardamos al nuevo jugador
		    loop([{From,Name,0}|Players],Words,Actual);
	       true ->
		    % si el nombre ya existe, le decimos que ya existe y
		    % no le admitimos en el juego
		    io:format("Player name repeated: ~p (~p)~n",[Name,From]),
		    From ! {Self,error,"This name already exists!"},
		    loop(Players,Words,Actual)
	    end;
	{From,{getpoints,Player}} ->
	    % un jugador pide la puntuacion de alguien (puede ser de todos)
	    Stats = get_stadistics(Players,Player),
	    From ! {Self,{stats,Stats}},
	    loop(Players,Words,Actual);

	{From,{word,W}} ->
	    % alguien manda una palabra
	    if W =/= Actual ->
		    % si falla le decimos que se ha equivocado
		    From ! {Self,{answer,error,Actual}},
		    loop(Players,Words,Actual);
	       true ->
		    % si acierta:
		    % le damos tantos puntos como larga es la palabra
		    Points = length(Actual),
		    % le decimos que ha acertado y los puntos ganados
		    From ! {Self,{answer,correct,Points}},
		    % Buscamos el nombre del jugador
		    Name = get_name(Players,From),
		    % le sumamos los puntos
		    Play = add_points(Players,Name,Points),
		    % avisamos de manera asincrona a todos los jugadores
		    % que alguien ha acertado ya
		    async_send_all(Play,{Self,{Name,wins}}),
		    % seleccionamos otra palabra y la mandamos
		    W2 = new_word(Words,Play),
		    loop(Play,Words,W2)
	    end;
	_ ->
	    % descartamos los mensajes q no son validos
	    loop(Players,Words,Actual)
    after 3000 ->
	    % cada 3 segundos que no recibimos ningun mensaje 
	    % comprobamos que players han muerto...
	    % se podria hacer en un proceso diferente para asegurarnos
	    % que se ejecuta cada cierto tiempo, pero por simplicidad
	    % lo he implementado asi.. aunque si hay un jugador muy activo
	    % no detectaremos si otros se van.
	    check_alive(Players,self()),
	    loop(Players,Words,Actual)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% misc functions
%% Todas estas son las funciones que hacen la magia


% manda en otro proceso un mensaje a toda la lista
% UN proceso para toda la lista
async_send_all(List,Msg) ->
    spawn(fun() ->
		  send_all(List,Msg)
	  end).

% esta funcion manda un mensaje a toda la lista de players
send_all([],_)->
    done;
send_all([{Pid,_,_}|List],Msg) ->
    Pid ! Msg,
    send_all(List,Msg).

% elige una palabra de la lista de palabras
select_random(Words) ->
    N = random:uniform(length(Words)),
    lists:nth(N,Words).

% elige y manda una nueva palabra a todos los players
new_word(Words,Players) ->
    W = select_random(Words),
    send_all(Players,{self(),{word,W}}),
    W.

% consigue el nombre de un jugador dado su Pid
get_name([],_) ->
    "Name not found";
get_name([{Pid,Name,_}|P],From) ->
    if From =/= Pid ->
	    get_name(P,From);
       true ->
	    Name
    end.

% comprueba si un nombre de jugador esta utilizado
check_player_exists([],_) ->
    % si vaciamos la lista significa que no existe
    false;
check_player_exists([{_,Pname,0}|Players],Name) ->
    if Name == all ->
	    % no permitimos que nadie se llame all
	    true;
	Pname =/= Name ->
	    check_player_exists(Players,Name);
       true ->
	    % si esta utilizado mandamos un true
	    true
    end.

% coge las estaditicas de TODOS los jugadores
get_stadistics(Players,all) ->
    % al tratarse de todos funciona de modo diferente
    lists:map(fun({_,N,P}) -> {N,P} end,Players);
% puntos de un jugador concreto
get_stadistics([{_,Name,Points}|List],Player) ->
    if Name == Player ->
	    % si coincide el nombre lo devolvemos
	    {Player,Points};
       true ->
	    get_stadistics(List,Player)
    end.


% elimina un jugador de la lista
remove_player([],_) ->
    [];
remove_player(Players,Name) ->
    remove_player_acumulative([],Players,Name).

% esta funcion es auxiliar, lo unico que hace es guardar la parte izquierda de la lista
% para poder devolverla
remove_player_acumulative(P,[],_) ->
    P;
remove_player_acumulative(Izq,[{Pid,Pname,Points}|Players],Name) ->
    if Pname =/= Name ->
	    remove_player_acumulative([{Pid,Pname,Points}|Izq],Players,Name);
       true ->
	    Izq++Players
    end.

% elimina una palabra de la lista
remove_word([],_) ->
    [];
remove_word(Words,Word) ->
    remove_word_acumulative([],Words,Word).

% esta funcion es auxiliar, lo unico que hace es guardar la parte izquierda de la lista
% para poder devolverla
remove_word_acumulative(W,[],_) ->
    W;
remove_word_acumulative(Izq,[W|Words],Word) ->
    if W =/= Word ->
	    remove_word_acumulative([W|Izq],Words,Word);
       true ->
	    Izq++Words
    end.


% comprueba que todos los jugadores estan activos
check_alive([],_) -> 
    ok;
check_alive([{P,N,_}|Players],Server) ->
    % para no bloquear el servidor lanzamos un proceso por jugador
    spawn(fun() ->
		  % manda un ping a el jugador
		  % simulamos ser el servidor
		  P ! {Server,ping,self()},
		  receive
		      % espera el pong
		      {N,pong} ->
			  ok
		  after 3000 ->
			  % si no recibe el pong en tres segundos mandamos al server
			  % un mensaje para eliminar el jugador
			  Server ! {removePlayer,N}
		  end
	  end),
    check_alive(Players,Server).

% suma puntos a un jugador concreto
add_points([],_,_) ->
    [];
add_points(Players,Name,Points) ->
    add_points_acumulative([],Players,Name,Points).

% esta funcion es auxiliar, lo unico que hace es guardar la parte izquierda de la lista
% para poder devolverla
add_points_acumulative(Izq,[],_,_) ->
    Izq;
add_points_acumulative(Izq,[{Ppid,Pname,Ppoint}|Players],Name,Points) ->
    if Pname =/= Name ->
	    add_points_acumulative([{Ppid,Pname,Ppoint}|Izq],Players,Name,Points);
       true ->
	    % si el jugador es este le sumamos los puntos
	    Izq++[{Ppid,Pname,Ppoint+Points}|Players]
    end.
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% at the end, loop with "too many" words
% escondemos la inicializacion para que no moleste si ponemos muchas palabras
loop() ->
    loop([],["hola","adios","caracola","juego","divertido"],undefined),
    io:format("Servidor apagado~n").
