-module(master).
% export gen_server interface functions
-export([init/1,terminate/2,handle_call/3,handle_cast/2,handle_info/2,code_change/3]).
-behaviour(gen_server).
% funciones de master
-export([start/1,add_worker/0,stop/0,newnode/0,mapreduce/1]).
-define(SERVERNAME, ?MODULE).

start(N) ->
    {ok,M} = node_master(N),M.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% interfaz de mensajes
stop() ->
    gen_server:cast(?SERVERNAME,{stop}).    
newnode()->
    gen_server:cast(?SERVERNAME,{newnode}).
mapreduce(N) ->
    gen_server:call(?SERVERNAME,{mapreduce,N}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% simplificacion

node_worker(Id) -> 
    {ok,W} = worker:node_worker(Id,?SERVERNAME),W.
node_admin(Nodes,Parent) ->
    {ok,A} = admin:node_admin(Nodes,Parent),A.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server MASTER

add_worker() ->
    gen_server:cast(?SERVERNAME,{newnode}).

node_master(N) -> 
    gen_server:start_link({local,?SERVERNAME},?MODULE, {N}, []).

init({N}) ->
    Nodes = lists:map(fun(Id) ->
			      io:format("Lanzando ~p~n",[Id]),
			      node_worker(Id)
		      end,lists:seq(1,N)),
    {ok, {N,Nodes}}.

handle_call({mapreduce,M},From,{N,Nodes}) ->
    {Nod1,Nod2} = lists:split(M,Nodes),
    Admin = node_admin(Nod1,From),
    io:format("Nodos asignados: ~p~nAdmin: ~p~n",[Nod1,Admin]),
    {reply,{admin,Admin},{N,Nod2++Nod1}}.
handle_cast({newnode},{N,Nodes}) ->
    Pid = node_worker(N),
    {noreply,{N+1,[Pid|Nodes]}};
handle_cast({stop},State) ->
    {stop,normal,State};

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

