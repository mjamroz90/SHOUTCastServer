-module(shout_cache).

-export([start_link/1, stop/0, insert/2, lookup/1, delete/1, save/2, load/1, disk_delete/1, load_all/0]).

start_link(File) ->	
	cache_server:start_link(File).

stop() ->
	cache_server:stop().	

insert(Name,List) ->
	cache_server:insert(Name,List).

lookup(Name) ->
	cache_server:lookup(Name).
	
delete(Name) ->
	cache_server:delete(Name).		

save(Name,List) ->
	cache_server:save(Name,List).

load(Name) ->
	cache_server:load(Name).
	
disk_delete(Name) ->
	cache_server:disk_delete(Name).

load_all() ->
	cache_server:load_all().	
	
%load_all(File) ->
%	Ram_table_id = ets:new(ram_table,[public]),		
%	dets:open_file(File,[{file,File}]),
%	dets:to_ets(File,Ram_table_id).			