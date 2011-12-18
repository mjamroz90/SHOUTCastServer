-module(shout_app).
-behaviour(application).

-export([start/2,stop/1]).

-define(START_FILE, "./pliki/playlists/mp3data.tmp").
-define(LIST_FILE, "./pliki/listy.dat").
-define(DEFAULT_PORT,3000).

start(_StartType,_StartArgs) ->	
	Port = case application:get_env(shout,port) of
			{ok,P} -> P;
			undefined -> ?DEFAULT_PORT
		end,
	List_File = case application:get_env(shout,list_file) of
			{ok,L} -> L;
			undefined -> ?LIST_FILE
		end,
	Start_File = case application:get_env(shout,start_file) of
			{ok,S} -> S;
			undefined -> ?START_FILE
		end,	
	{ok, LSock} = gen_tcp:listen(Port,[{active,true}]),	
	event_sup:start_link(),
	shout_event_logger:add_handler(),
	shout_sup:start_link(List_File,Start_File),	
	case conn_sup:start_link(LSock) of
		{ok,Pid1} ->
			{ok,Pid1};
		Other1 ->
			{error,Other1}
	end.

stop(_State) ->
	ok.	
		
	