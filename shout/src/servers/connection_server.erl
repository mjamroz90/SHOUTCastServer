-module(connection_server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/1,stop/0]).

-import(lists,[map/2,reverse/1]).

-record(state,{lsock,header,num}).

-define(SERVER,?MODULE).

-define(CHUNKSIZE, 24576).

%API=================================================================

start_link(LSock) ->
	gen_server:start_link(?MODULE,[LSock], []).
	
stop() ->
	gen_server:cast(?SERVER,stop).

%CallBacks===========================================================	

init([LSock]) ->
	shout_event:wake_up("connection_server"),
	process_flag(trap_exit, true),
	{ok,#state{lsock = LSock,header = [],num = 0},0}.
	
handle_info(timeout, #state{lsock = LSock} = State) ->
	{ok,Sock} =  gen_tcp:accept(LSock),
	conn_sup:start_child(),	
	shout_event:accept("unknown"),	
	Num = song_server:get_number(),	
	inet:setopts(Sock,[{packet,0},binary, {nodelay,true},{active,true}]),
	% get_request(Sock,Num,[]),
	{noreply,State#state{num = Num}};
	
handle_info({tcp, Socket, Bin},#state{header = L, num = Num} = State) ->
	L1 = L ++ binary_to_list(Bin),	
	case split(L1, []) of
		more ->
			{noreply,State#state{header = L1}};
			% get_request(Socket,Num , L1)
		{Request, _Rest} ->		
			got_request_from_client(Request, Socket, Num),
			{noreply,State}
	end;

handle_info({tcp_closed, _Socket}, State) ->
	{stop,normal,State};

handle_info(_Any, State) ->
	{noreply,State}.	
	
handle_call(_Request,_From,State) ->
	{reply,ok,State}.

handle_cast(stop,State) ->
	io:format("jestem terminowany stop~n"),
	{stop,normal,State}.

terminate(_Reason,_State) ->
	io:format("jestem terminowany~n"),
	gui_server:break_streaming(self()),
	ok.
	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	
%Internal============================================================

got_request_from_client(Request, Socket, Num) ->
    Cmds = string:tokens(Request, "\r\n"),
    Cmds1 = map(fun(I) -> string:tokens(I, " ") end, Cmds),
    is_request_for_stream(Cmds1),
    gen_tcp:send(Socket, [response()]),
	L = lists:seq(1,Num),	
	lists:foreach(fun(X) -> play_songs(Socket,<<>>,X) end,L).	
	
play_songs(Socket, SoFar, I) ->
    Song = song_server:serve_song(I),
    {File,PrintStr,Header} = unpack_song_descriptor(Song),
    case id3_tag_lengths:file(File) of
	error ->
	    play_songs(Socket, SoFar,I+1);
	{Start, Stop} ->
	    io:format("Playing:~p~n",[PrintStr]),
	    {ok, S} = file:open(File, [read,binary,raw]), 
	    send_file(S, {0,Header}, Start, Stop, Socket, SoFar),
	    file:close(S)	   
    end.	

send_file(S, Header, OffSet, Stop, Socket, SoFar) ->
    %% OffSet = first byte to play
    %% Stop   = The last byte we can play
    Need = ?CHUNKSIZE - size(SoFar),
    Last = OffSet + Need,
    if
	Last >= Stop ->
	    %% not enough data so read as much as possible and return
	    Max = Stop - OffSet,
	    {ok, Bin} = file:pread(S, OffSet, Max),
	    list_to_binary([SoFar, Bin]);
	true ->
	    {ok, Bin} = file:pread(S, OffSet, Need),
	    write_data(Socket, SoFar, Bin, Header),
	    send_file(S, bump(Header),
		      OffSet + Need,  Stop, Socket, <<>>)
    end.


write_data(Socket, B0, B1, Header) ->
    %% Check that we really have got a block of the right size
    %% this is a very useful check that our program logic is
    %% correct
    case size(B0) + size(B1) of
	?CHUNKSIZE ->
	    case gen_tcp:send(Socket, [B0, B1, the_header(Header)]) of
		ok -> true;
		{error, closed} ->
		    %% this happens if the player 
		    %% terminates the connection
		    exit(playerClosed)
	    end;
	_Other ->
	    %% don't send the block - report an error
	    io:format("Block length Error: B0 = ~p b1=~p~n",
		      [size(B0), size(B1)])
    end.

bump({K, H})     -> {K+1, H}.

the_header({K, H}) ->
    case K rem 5 of
	0 -> H;
	_ -> <<0>>
    end.	    

is_request_for_stream(_) -> true.

split("\r\n\r\n" ++ T, L) -> {reverse(L), T};
split([H|T], L)           -> split(T, [H|L]);
split([], _)              -> more.

response() ->
    ["ICY 200 OK\r\n",
     "icy-notice1: <BR>This stream requires",
     "<a href=\"http://www.winamp.com/\">Winamp</a><BR>\r\n",
     "icy-notice2: Erlang Shoutcast server<BR>\r\n",
     "icy-name: Erlang mix\r\n",
     "icy-genre: Pop Top 40 Dance Rock\r\n",
     "icy-url: http://localhost:3000\r\n",
     "content-type: audio/mpeg\r\n",
     "icy-pub: 1\r\n",
     "icy-metaint: ",integer_to_list(?CHUNKSIZE),"\r\n",
     "icy-br: 96\r\n\r\n"]. 		 
	 
unpack_song_descriptor({File, {_Tag,Info}}) ->
    PrintStr = list_to_binary(make_header1(Info)),
    L1 = ["StreamTitle='",PrintStr,
	  "';StreamUrl='http://localhost:3000';"],
    %% io:format("L1=~p~n",[L1]),
    Bin = list_to_binary(L1),
    Nblocks = ((size(Bin) - 1) div 16) + 1,
    NPad = Nblocks*16 - size(Bin), 
    Extra = lists:duplicate(NPad, 0),
    Header = list_to_binary([Nblocks, Bin, Extra]),
    %% Header is the Shoutcast header
    {File, PrintStr, Header}.

make_header1([{track,_}|T]) -> 
    make_header1(T);    
make_header1([{Tag,X}|T]) ->
    [atom_to_list(Tag),": ",X," "|make_header1(T)];
make_header1([]) ->
    [].	 
	 