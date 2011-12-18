-module(event_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER,?MODULE).

start_link() ->
	supervisor:start_link({local,?SERVER}, ?MODULE, []).

init([]) ->	
	EventManager = {shout_event,{shout_event,start_link,[]},
					permanent,2000,worker,[shout_event]},
					
	RestartStrategy = {one_for_one,4,3600},	
	{ok, {RestartStrategy, [EventManager]}}.