-module(master).
% export gen_server interface functions
-export([init/1,terminate/2,handle_call/3,handle_cast/2,handle_info/2,code_change/3]).
-behaviour(gen_server).
% funciones de master
-export([ejemplo1/0,client1/1,fmap1/2,freduce1/3,start/2]).
-define(SERVERNAME, ?MODULE).

% init breve para lanzar el ejemplo
ejemplo1() ->
    start([{madrid,34},{madrid,34},{madrid,34},{barcelona,21},{madrid,22},{barcelona,19},{teruel,-5}, {teruel, 14}, {madrid,37}, {teruel, -8}, {barcelona,30},{teruel,10}],3).

client1(Master) ->
    spawn(fun() ->
		  MMM = gen_server:call(Master,{mapreduce,self(),fun(A,B) -> fmap1(A,B) end,fun(A,B,C) -> freduce1(A,B,C) end}),
		  io:format("RESULT: ~p~n",[MMM])
	  end).
fmap1(Id,Part) ->
    io:format("DEBUG: id: ~p, Part: ~p ~n",[Id,Part]),
    lists:filter(fun({_Ciudad,Grados}) ->
			 Grados >= 28 end, [Part]).

freduce1(Clave,ValorActual,ValorNuevo) ->
    io:format("DEBUG: Clave: ~p, VA: ~p, VN: ~p~n",[Clave,ValorActual,ValorNuevo]),
    max(ValorActual,ValorNuevo).

start(Info,N) ->
    node_master(Info,N).

node_map(Id,Master,Info) -> 
    map:node_map(Id,Master,Info).
node_admin(Nodes,Parent,FMap,FReduce) ->
    admin:node_admin(Nodes,Parent,FMap,FReduce).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server MASTER

node_master(Info,N) -> 
    gen_server:start_link({local,master},?MODULE, {Info,N}, []).

init({Info,N}) ->
    Self = self(),
    Parts = split_N(Info,N),
    Nodes = lists:map(fun(Id) ->
			      node_map(Id,Self,lists:nth(Id,Parts))
		      end,lists:seq(1,length(Parts))),
    {ok, {Info,N,Nodes}}.

handle_call({mapreduce,_Parent,FMap, FReduce},From,{Info,N,Nodes}) ->
    node_admin(Nodes,From,FMap,FReduce),
    {noreply,{Info,N,Nodes}}.
handle_cast({stop},State) ->
    {stop,normal,State};

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% misc functions
% Funciones que no tienen que ver directamente con mapReduce

% divide L en N partes iguales, si Length(L) < N serán length(L) partes y no N
split_N(L,N) ->
    Len = ceil(length(L)/N),
    split(L,Len).

% divide L en partes de tamaño N
split([], _) -> [];
split(L, N) when length(L) < N -> [L];
split(L, N) ->
    {A, B} = lists:split(N, L),
    [A | split(B, N)].

% aproxima a entero superior
ceil(X) when X < 0 ->
    trunc(X);
ceil(X) ->
    T = trunc(X),
    if X == T -> T;
       true -> T + 1
    end.
