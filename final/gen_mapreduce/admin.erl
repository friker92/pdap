-module(admin).
% export gen_server interface functions
-export([init/1,terminate/2,handle_call/3,handle_cast/2,handle_info/2,code_change/3]).
-behaviour(gen_server).
% funciones de admin
-export([node_admin/4]).

node_admin(Nodes,Parent,FMap,FReduce) ->
    gen_server:start_link(?MODULE, {Nodes,Parent,FMap,FReduce}, []).

% This is called when a connection is made to the server
init({Nodes,Parent,FMap,FReduce}) ->
    lists:map(fun({ok,P})-> gen_server:cast(P,{startmap,self(),FMap}) end,Nodes),
    Dict = dict:new(),
    %%%  { Client, Fase, Maps,         Reds, dict, FReduce, Result }
    {ok, { Parent,  map,length(Nodes),    0, Dict, FReduce,     [] }}.

handle_cast({Clave,Valor},{Client,map,NM,NR,Dict,FReduce,Result})->
    case dict:is_key(Clave, Dict) of
	true ->
	    Pid = dict:fetch(Clave,Dict),
	    gen_server:cast(Pid,{newvalue,Valor}),
	    {noreply,{Client,map,NM,NR,Dict,FReduce,Result}};
	false ->
	    {ok,Pid}= reduce:node_reduce(self(),FReduce,Clave,Valor),
	    D2 = dict:store(Clave,Pid,Dict),
	    {noreply,{Client,map,NM,NR+1,D2,FReduce,Result}}
    end;

handle_cast({fin},{Client,map,NM,NR,Dict,FReduce,Result})->
    N = NM - 1,
    case N =< 0 of
	true -> 
	    lists:map(fun({_K,V})-> 
			      gen_server:cast(V,{fin}) 
		      end,dict:to_list(Dict)),
	    {noreply,{Client,reduce,N,NR,Dict,FReduce,Result}};
	false ->
	    {noreply,{Client,map,N,NR,Dict,FReduce,Result}}
    end;

handle_cast({Clave,Valor},{Client,reduce,NM,NR,Dict,FReduce,Result})->
    R2 = Result ++ [{Clave,Valor}],
    N = NR - 1,
    case N =< 0 of
	true ->
	    gen_server:reply(Client,R2),
	    {stop,normal,{Client,reduce,NM,N,Dict,FReduce,R2}};
	false ->
	    {noreply,{Client,reduce,NM,N,Dict,FReduce,R2}}
    end.

% We get compile warnings from gen_server unless we define these
handle_call(Msg,_From,State)-> {reply,{error,Msg},State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
