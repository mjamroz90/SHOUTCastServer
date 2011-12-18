-module(shout_sup).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

-define(SERVER,?MODULE).

start_link(File,Song_File) ->
	supervisor:start_link({local,?SERVER}, ?MODULE, [File,Song_File]).

init([File,Song_File]) ->
	Gui_Cache_Sup = {gui_cache_sup,{gui_cache_sup,start_link,[File]},
				permanent,2000,supervisor,[gui_server,cache_server]},
				
	Song_Server = {song_server,{song_server,start_link,[Song_File]},
				permanent,2000,worker,[song_server]},

	RestartStrategy = {one_for_one,4,3600},
	shout_event:create("song_server","shout_supervisor"),
	{ok, {RestartStrategy, [Gui_Cache_Sup,Song_Server]}}.
