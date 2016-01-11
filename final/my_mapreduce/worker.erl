
-module(worker).
% export gen_server interface functions
-export([init/1,terminate/2,handle_call/3,handle_cast/2,handle_info/2,code_change/3]).
-behaviour(gen_server).
% funciones de admin
-export([fase/3,node_worker/2]).


fase(Node,Info,Fase)->
    gen_server:call(Node,{ejecuta,Info,Fase}).

node_worker(Id,Master) ->
    gen_server:start_link(?MODULE, {Id,Master}, []).

init({Id,Master}) ->
    {ok, {Id,Master}}.

handle_call({ejecuta,Info,{F_init,F,F_fin}},From,{Id,Master}) ->
    spawn(fun()->
		  Info2 = case F_init of
			      none ->
				  Info;
			      _ ->
				  F_init(Info)
			  end,
		  MAP = lists:map(fun(Part) ->
					  F(Id,Part)
				  end,Info2),
		  R = lists:filter(fun(P) -> 
					   P =/= none
				   end,MAP),
		  R2 = case F_fin of
			   none ->
			       R;
			   _ ->
			       F_fin(R)
		       end,
		  gen_server:reply(From,R2)
	  end),
    {noreply,{Id,Master}}.


% We get compile warnings from gen_server unless we define these
handle_cast(_Msg,State)-> {noreply,State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
