-module(song_server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/1,stop/0,serve_song/1, replace_list/1, get_number/0]).

-record(state,{file,songlist}).

-define(SERVER,?MODULE).

%API=====================================================================

start_link(File) ->
	gen_server:start_link({local,?SERVER},?MODULE,[File],[]).

stop() ->
	gen_server:cast(?SERVER,stop).	

serve_song(Num) ->
	gen_server:call(?SERVER,{serve_song,Num}).
	
replace_list(File) ->
	gen_server:cast(?SERVER,{replace_list,File}).
	
get_number() ->
	gen_server:call(?SERVER,get_number).
	
%Callbacks===============================================================

init([File]) ->
	shout_event:wake_up("song_server"),
	{ok, [SongList]} = file:consult(File),
	random_seed(),	
	{ok,  #state{file = File, songlist = SongList}}.
	
handle_call({serve_song,Num},_From,#state{songlist = SongList} = State) ->	
	% I = random:uniform(length(SongList)),
	Song = lists:nth(Num, SongList),
	{reply, Song, State};
	
handle_call(get_number,_From,#state{songlist = SongList} = State) ->
	{reply,length(SongList),State}.	
		
handle_cast({replace_list,File},State) ->
	{ok,[NewList]} = file:consult(File),	
	{noreply,State#state{file = File,songlist = NewList}};

handle_cast(stop,State) ->
	{stop,normal,State}.	
	
handle_info(_Msg, State) ->
	{noreply,State}.
	
terminate(_Reason,_State) ->
	ok.
	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	
%Internal================================================================

random_seed() ->
    {_,_,X} = erlang:now(),
    {H,M,S} = time(),
    H1 = H * X rem 32767,
    M1 = M * X rem 32767,
    S1 = S * X rem 32767,
    put(random_seed, {H1,M1,S1}).	