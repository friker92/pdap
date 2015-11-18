-module(quicksort).
-export([sort/1]).



sort([]) -> [];
sort([P|List]) ->
    L = [X || X <- List, X < P],
    R = [Y || Y <- List, Y >= P],
    SELF = self(),
    Pid = spawn(fun() ->   Result = sort(R),
   		     	   SELF ! {self(), Result}
	  end),
    ResultL = sort(L),
    receive
       {Pid,ResultR} -> 
           ResultL ++ [P] ++ ResultR
    end.


