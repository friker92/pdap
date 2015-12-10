-module(mapreduce).
% atajos para ejemplos
-export([ejemplo1/0,fmap1/2,freduce1/3,client1/1]).
% funciones de master
-export([master/2]).
% funciones de nodo
%-export([node_map/3]).


start(Info,N) ->
    Mpid = spawn(fun() ->
			 master(Info,N)
		 end),
    Mpid.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% master functions
master(Info, N) ->
    Self = self(),
    Parts = split_N(Info,N),
    Nodes = lists:map(fun(Id) ->
			      node_map(Id,Self,lists:nth(Id,Parts))				
		      end,lists:seq(1,length(Parts))),
    loop_master(Parts, N, Nodes).

loop_master(Info, N, Nodes) ->
    receive 
	stop ->
	    stop;
	{From,{mapreduce, Parent, FMap, FReduce}} ->
	    From ! {self(),{admin,mradmin(Nodes,Parent,FMap,FReduce)}};
	X ->
	    io:format("~p~n",[X]),
	    loop_master(Info, N, Nodes)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% node functions

node_map(Id,Master,Info) ->
    spawn(fun() ->
		  loop_map(Id,Master,Info)
	  end).
loop_map(Id,Master,Info) ->
    Self = self(),
    receive 
	stop ->
	    stop;
	{From,{startmap,From,FMap}} ->
	    spawn(fun() ->
			  lists:map(fun(Part) ->
					    List = FMap(Id,Part),
					    lists:map(fun({C,V}) ->
							      From ! {Self,{C,V}}
						      end,List)
				    end,Info),
			  From ! {Self,{fin}}
		  end);
		
	X ->
	    io:format("Map...: ~p~n",[X]),
	    loop_map(Id,Master,Info)
    end.
    

node_reduce(Mradmin,FReduce,Clave,Valor) ->
    spawn(fun() ->
		  loop_reduce(Mradmin,FReduce,Clave,Valor)
	  end).
loop_reduce(Mradmin,FReduce,Clave,ValorActual) ->
    receive 
	stop ->
	    stop;
	{_From,{newvalue,Valor}} ->
	    V = FReduce(Clave,ValorActual,Valor),
	    loop_reduce(Mradmin,FReduce,Clave,V);
	{_From,{fin}} ->
	    Mradmin ! {self(),{Clave,ValorActual}};
	X ->
	    io:format("Reduce...: ~p~n",[X]),
	    loop_reduce(Mradmin,FReduce,Clave,ValorActual)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% mradmin functions

mradmin(Nodes,Client,FMap,FReduce) ->
    spawn(fun() ->
		  send_all(Nodes,{self(),{startmap,self(),FMap}}),
		  Dict = dict:new(),
		  {State,R} = loop_admin_map(Dict,length(Nodes),0,FReduce),
		  case State of
		      error ->
			  send_all(Nodes,stop),
			  io:format("Error MAP: ~p~n",[R]);
		      ok ->
			  send_all(R,{self(),{fin}}),
			  {S2,R2} = loop_admin_reduce(length(R),[]),
			  case S2 of 
			      error ->
				  send_all(R,stop),
				  io:format("Error Reduce: ~p~n",[R2]);
			      ok ->
				  Client ! {self(),{R2}}
			  end
		  end
	  end).

loop_admin_map(Dict,Nmappers,Nreducers,FReduce) ->
    receive
	{_From,{Clave,Valor} }->
		case dict:is_key(Clave, Dict) of
		    true ->
			Pid = dict:fetch(Clave,Dict),
			Pid ! {self(),{newvalue,Valor}},
			loop_admin_map(Dict,Nmappers,Nreducers,FReduce);
		    false ->
			Pid = node_reduce(self(),FReduce,Clave,Valor),
			D2 = dict:store(Clave,Pid,Dict),
			loop_admin_map(D2,Nmappers,Nreducers+1,FReduce)
		end;
	{_From,{fin}} -> 
	    N = Nmappers - 1,
	    case N =< 0 of
		true -> 
		    Pids = lists:map(fun({_K,V}) -> V end, dict:to_list(Dict)),
		    {ok,Pids};
		false ->
		    loop_admin_map(Dict,N,Nreducers,FReduce)
	    end;
	X ->
	    Pids = lists:map(fun({_K,V}) -> V end, dict:to_list(Dict)),
	    send_all(Pids,stop),
	    {error,{"Msg",X}}
    end.

loop_admin_reduce(Nreducers,Result) ->
    receive
	{_From,{Clave,Valor}} ->
	    R2 = Result++[{Clave,Valor}],
	    N = Nreducers - 1,
	    case N =< 0 of
		true ->
		    {ok,R2};
		false ->
		    loop_admin_reduce(N,R2)
	    end;
	X ->
	    {error,{"Msg",X}}
    end.
			

	    

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

send_all(List,Msg) ->
    lists:map(fun(Pid) ->
		      Pid ! Msg
	      end,List).

% init breve para lanzar el ejemplo
ejemplo1() ->
    start([{madrid,34},{barcelona,21},{madrid,22},{barcelona,19},{teruel,-5}, {teruel, 14}, {madrid,37}, {teruel, -8}, {barcelona,30},{teruel,10}],3).


fmap1(_Id,Part) ->
    %io:format("DEBUG: id: ~p, Part: ~p ~n",[Id,Part]),
    lists:filter(fun({_Ciudad,Grados}) ->
			 Grados >= 28 end, [Part]).

freduce1(_Clave,ValorActual,ValorNuevo) ->
    %io:format("DEBUG: Clave: ~p, VA: ~p, VN: ~p~n",[Clave,ValorActual,ValorNuevo]),
    max(ValorActual,ValorNuevo).


client1(Master) ->
    spawn(fun() ->
		  Master ! {self(),{mapreduce,self(),fun(A,B) -> fmap1(A,B) end,fun(A,B,C) -> freduce1(A,B,C) end}},
		  receive
		      {Master,{admin,Pid}} ->
			  receive
			      {Pid,X}->
				  io:format("RESULT: ~p~n",[X])
			  end
		  end
	  end).
