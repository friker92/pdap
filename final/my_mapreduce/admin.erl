-module(admin).
% export gen_server interface functions
-export([init/1,terminate/2,handle_call/3,handle_cast/2,handle_info/2,code_change/3]).
-behaviour(gen_server).
% funciones de admin
-export([node_admin/2,addInfo/2,inicia/1,addFase/2,setBasic/3]).

addInfo(Server,Info) ->
    gen_server:cast(Server,{addInfo,Info}).
addFase(Server,Fase) ->
    gen_server:cast(Server,{addFase,Fase}).
setBasic(Server,split,F) ->
    gen_server:cast(Server,{setSplit,F});
setBasic(Server,shuffle,F) ->
    gen_server:cast(Server,{setShuffle,F});
setBasic(Server,show,F) ->
    gen_server:cast(Server,{setShow,F}).
    

inicia(Server) ->
    gen_server:call(Server,{init}).

node_admin(Nodes,Parent) ->
    gen_server:start_link(?MODULE, {Nodes,Parent}, []).

init({Nodes,Parent}) ->
    Split =   fun(T,N) -> default_split(T,N)   end,
    Shuffle = fun(T,N) -> default_shuffle(T,N) end,
    Show =    fun(T)   -> default_show(T)      end,
    %%%  { Client, Nodes, Info,               Basics, Fases }
    {ok, { Parent, Nodes,   [], {Split,Shuffle,Show},    [] }}.

handle_cast({addInfo,NewInfo},{ Client, Nodes, Info, Basics, Fases }) ->
    {noreply,{ Client, Nodes, [NewInfo]++Info, Basics, Fases }};
handle_cast({resetInfo},{ Client, Nodes, _Info, Basics, Fases }) ->
    {noreply,{ Client, Nodes, [], Basics, Fases }};


handle_cast({addFase,NewFase},{ Client, Nodes, Info, Basics, Fases }) ->
    {noreply,{ Client, Nodes, Info, Basics, Fases++[NewFase] }};
handle_cast({resetFase},{ Client, Nodes, Info, Basics, _Fases }) ->
    {noreply,{ Client, Nodes, Info, Basics, [] }};

handle_cast({setSplit,NewSplit},{ Client, Nodes, Info, {_Sp,Shu,Sho}, Fases }) ->
    {noreply,{ Client, Nodes, Info, {NewSplit,Shu,Sho}, Fases }};
handle_cast({setShuffle,NewShuffle},{ Client, Nodes, Info, {Sp,_Shu,Sho}, Fases }) ->
    {noreply,{ Client, Nodes, Info, {Sp,NewShuffle,Sho}, Fases }};
handle_cast({setShow,NewShow},{ Client, Nodes, Info, {Sp,Shu,_Sho}, Fases }) ->
    {noreply,{ Client, Nodes, Info, {Sp,Shu,NewShow}, Fases }}.

handle_call({init},_From,{Client,Nodes,Info,{Sp,Shu,Sho},[Fase|Fases] }) ->
    % first Split
    Parts = Sp(Info,length(Nodes)),
    Parts2 = ejecuta_fase(Nodes,Parts,Fase),
    R = lists:foldl(fun(F,P2) -> 
		      P = Shu(P2,length(Nodes)),
		      ejecuta_fase(Nodes,P,F)
	      end,Parts2,Fases),
    Result = Sho(R),
    {reply,{fin,Result},{ Client, Nodes, Info,{Sp,Shu,Sho}, Fases }}.

% We get compile warnings from gen_server unless we define these
%handle_call(Msg,_From,State)-> {reply,{error,Msg},State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% misc functions
% Funciones que no tienen que ver directamente con el admin de mapReduce

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% defaults functions

default_show(T) ->
    T.
default_split([T],N) ->
    split_N(T,N).
default_shuffle(T,N) ->
%%% hay que definir el formato de T para hacer correctamente la mezcla
%%% de los pares clave valor y luego dividirlo en N partes
%% T = [{K,V},{K,V},...]
    Dict = dict:new(),
    Dict2 = ds(T,Dict),
    L = lists:map(fun(K)->
			  {K,dict:fetch(K,Dict2)}
		  end,dict:fetch_keys(Dict2)),
    split_N(L,N).

ds([],D) ->
    D;
ds([{C,V}|T],D) ->
    D2 = case dict:is_key(C, D) of
	     true ->
		 dict:append(C,V,D);
	     false ->
		 dict:store(C,[V],D)
	 end,
    ds(T,D2).
fase(Node,Info,Fase) ->
    worker:fase(Node,Info,Fase).

ejecuta_fase(Nodes,Parts,Fase) ->
    Self =self(),
    Pids = lists:map(fun(Nth) -> 
			     spawn(fun() ->
					   Response = fase(lists:nth(Nth,Nodes),lists:nth(Nth,Parts),Fase),
					   Self ! {self(),Response}
				   end)
		     end,lists:seq(1,length(Parts))),
    recibe_fase(Pids,[]).

recibe_fase([],Res) ->
    Res;
recibe_fase([Pid|Pids],Res) ->
    R = receive
	    {Pid,Response} ->
		Res ++ Response
	end,
    recibe_fase(Pids,R).

