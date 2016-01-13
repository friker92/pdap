-module(superv).

-behaviour(supervisor).

-export([init/1, start/0]).

start() -> supervisor:start_link(?MODULE, []).

init(_) -> 
  {ok, 
    {{one_for_one, 2, 1}, 
     [
      {file, {afile_server, start, ["/tmp/"]}, permanent, 5000, worker, [afile_server]},
      {area, {area_server, start, []}, permanent, 5000, worker, [area_server]},
      {ticket, {ticket_server, start, [10]}, permanent, 5000, worker, [ticket_server]}
     ]
    }
  }.

    
