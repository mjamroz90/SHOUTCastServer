-module(cache_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
		 
-export([start_link/1, stop/0, insert/2, lookup/1, delete/1, save/2, load/1, disk_delete/1, load_all/0]).

-define(SERVER, ?MODULE).
-record(state,{ram_table,disk_table}).

%API===============================================================

start_link(File) ->	
	gen_server:start_link({local,?SERVER},?MODULE,[File],[]).

stop() ->
	gen_server:cast(?SERVER,stop).	

insert(Name,List) ->
	gen_server:call(?SERVER,{insert,{Name,List}}).

lookup(Name) ->
	gen_server:call(?SERVER,{lookup,Name}).
	
delete(Name) ->
	gen_server:call(?SERVER,{delete,Name}).		

save(Name,List) ->
	gen_server:call(?SERVER,{save,{Name,List}}).

load(Name) ->
	gen_server:call(?SERVER,{load,Name}).
	
load_all() ->
	gen_server:call(?SERVER,load_all).
	
disk_delete(Name) ->
	gen_server:call(?SERVER,{disk_delete,Name}).	
	
%Callback===========================================================

init([File]) ->
	shout_event:wake_up("cache_server"),
	Ram_table_id1 = ets:new(ram_table,[public]),		
	case dets:open_file(File,[{file,File}]) of
		{ok,File} ->
			Disk_table_id = File,
			Ram_table_id = dets:to_ets(Disk_table_id,Ram_table_id1);
		{error,_Reason} ->
			Disk_table_id = error,
			Ram_table_id = 0
	end,		
	{ok,  #state{ram_table = Ram_table_id,disk_table = Disk_table_id}}.
	
handle_call({insert,{Name,List}}, _From, #state{ram_table = Ram_table_id} = State) ->
	ets:insert(Ram_table_id, {Name,List}),
	{reply,ok,State};
	
handle_call({lookup,Name}, _From, #state{ram_table = Ram_table_id} = State) ->	
	case ets:lookup(Ram_table_id,Name) of
		[{Name, List}] -> 
			Reply = {ok,List};
		[] ->
			Reply = {error,not_found}
	end,
	{reply,Reply,State};

handle_call({delete,Name}, _From, #state{ram_table = Ram_table_id} = State) ->
	ets:match_delete(Ram_table_id, {Name,'_'}),
	{reply, ok, State};
	
handle_call({save,{Name,List}}, _From, #state{disk_table = Disk_table_id} = State) ->	
	Reply = dets:insert(Disk_table_id,[{Name,List}]),
	{reply,Reply,State};
			
handle_call({load,Name}, _From, #state{disk_table = Disk_table_id} = State) ->	
	case dets:lookup(Disk_table_id,Name) of
		[{Name, List}] -> 
			Reply = {ok,List};
		[] ->
			Reply = {error,not_found}
	end,
	{reply,Reply,State};
	
handle_call({disk_delete,Name}, _From, #state{disk_table = Disk_table_id} = State) ->
	Reply = dets:match_delete(Disk_table_id, {Name,'_'}),
	{reply, Reply, State};
	
handle_call(load_all,_From,#state{ram_table = Ram_table_id} = State) ->
	List = ets:match_object(Ram_table_id,'_'),
	List1 = lists:map(fun({X,_}) -> X end, List),
	{reply, List1, State}.	

handle_cast(stop,#state{disk_table = Disk_table_id} = State) ->
	dets:close(Disk_table_id),
	{stop,normal,State}.	
	
handle_info(_Msg, State) ->
	{noreply,State}.
	
terminate(_Reason,_State) ->
	ok.
	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.	
		
%Internal===============================================================
		
	