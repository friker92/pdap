-module(map).
% export gen_server interface functions
-export([init/1,terminate/2,handle_call/3,handle_cast/2,handle_info/2,code_change/3]).
-behaviour(gen_server).
% funciones de admin
-export([node_map/3]).


node_map(Id,Master,Info) ->
    gen_server:start_link(?MODULE, {Id,Master,Info}, []).

init({Id,Master,Info}) ->
    {ok, {Id,Master,Info}}.

handle_cast({startmap,From, FMap},{Id,Master,Info}) ->
    spawn(fun()->
		  lists:map(fun(Part) ->
				    List = FMap(Id,Part),
				    lists:map(fun({C,V}) ->
						      gen_server:cast(From,{C,V})
					      end,List)
			    end,Info),
		  gen_server:cast(From,{fin})
	  end),
    {noreply,{Id,Master,Info}}.


% We get compile warnings from gen_server unless we define these
handle_call(Msg,_From,State)-> {reply,{error,Msg},State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
