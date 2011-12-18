-module(shout_event).

-export([start_link/0,
		add_handler/2,
		delete_handler/2,		
		create/2,
		wake_up/1,
		accept/1]).

-define(SERVER, ?MODULE).

start_link() ->
	gen_event:start_link({local, ?SERVER}).

add_handler(Handler,Args) ->
	gen_event:add_handler(?SERVER,Handler,Args).

delete_handler(Handler, Args) ->
	gen_event:delete_handler(?SERVER, Handler, Args).

create(ServerName,Supervisor) ->
	gen_event:notify(?SERVER,{create,{ServerName,Supervisor}}).

wake_up(ServerName) ->
	gen_event:notify(?SERVER, {wake_up,ServerName}).
	
accept(ClientName) ->
	gen_event:notify(?SERVER,{accept,ClientName}).
	