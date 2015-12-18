-module(reduce).
% export gen_server interface functions
-export([init/1,terminate/2,handle_call/3,handle_cast/2,handle_info/2,code_change/3]).
-behaviour(gen_server).
% funciones de admin
-export([node_reduce/4]).

node_reduce(Admin,FReduce,Clave,Valor) ->
    gen_server:start_link(?MODULE,{Admin,FReduce,Clave,Valor},[]).

init({Admin,FReduce,Clave,Valor}) ->
    {ok,{Admin,FReduce,Clave,Valor}}.

handle_cast({newvalue,Valor},{Admin,FReduce,Clave,ValorActual})->
    V = FReduce(Clave,ValorActual,Valor),
    {noreply,{Admin,FReduce,Clave,V}};
handle_cast({fin},{Admin,FReduce,Clave,ValorActual})->
    gen_server:cast(Admin,{Clave,ValorActual}),
    {stop,normal,{Admin,FReduce,Clave,ValorActual}}.



% We get compile warnings from gen_server unless we define these
handle_call(Msg,_From,State)-> {reply,{error,Msg},State}.
handle_info(_Message, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.
