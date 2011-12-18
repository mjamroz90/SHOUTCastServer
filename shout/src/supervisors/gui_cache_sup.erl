-module(gui_cache_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER,?MODULE).

start_link(File) ->
	supervisor:start_link({local,?SERVER}, ?MODULE, [File]).

init([File]) ->
	% Cache_Server = {cache_server,{cache_server,start_link,[File]},
	%			transient,brutal_kill,worker,[cache_server]},
		
	Gui_Server = {gui_server,{gui_server,start_link,[File]},
				temporary,brutal_kill,worker,[gui_server]},	

	RestartStrategy = {one_for_all,4,3600},
	shout_event:create("gui_cache_sup","gui_server"),
	{ok, {RestartStrategy, [Gui_Server]}}.