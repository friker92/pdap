-module(supervTree).

-behaviour(supervisor).

-export([init/1, start/1]).

start(N) -> supervisor:start_link(?MODULE, [N]).

init([1]) ->
process_flag(trap_exit, true),
    {ok, 
     {{one_for_one, 2, 1}, 
      [
       {hojaIzq, {area_server, start, []}, permanent, 5000, worker, [area_server]},
       {hojaDer, {area_server, start, []}, permanent, 5000, worker, [area_server]}
      ]
     }
    };

init([N]) ->
process_flag(trap_exit, true),
  {ok, 
    {{one_for_one, 2*N, 1}, 
     [
      {nodeIzq, {supervTree, start, [N-1]}, permanent, 5000, supervisor, [supervTree]},
      {nodeDer, {supervTree, start, [N-1]}, permanent, 5000, supervisor, [supervTree]}
     ]
    }
  }.

    
