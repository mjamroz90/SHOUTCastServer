-module(conn_sup).

-behaviour(supervisor).

-export([start_link/1, start_child/0, delete_all/0]).

-export([init/1]).

-define(SERVER,?MODULE).

start_link(LSock) ->
	supervisor:start_link({local,?SERVER}, ?MODULE, [LSock]).
	
start_child() ->
	supervisor:start_child(?SERVER,[]).

delete_all() ->	
	ChildList = supervisor:which_children(?SERVER),	
	lists:map(fun({_,Child,_,_}) -> supervisor:terminate_child(?SERVER,Child) end, ChildList).
	
	
init([LSock]) ->
	Server = {connection_server,{connection_server,start_link,[LSock]},
				temporary,2000,worker,[connection_server]},
	RestartStrategy = {simple_one_for_one,0,1},
	shout_event:create("connection_server","connection_supervisor"),
	{ok, {RestartStrategy, [Server]}}.

	