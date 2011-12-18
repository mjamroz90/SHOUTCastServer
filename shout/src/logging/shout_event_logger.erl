-module(shout_event_logger).

-behaviour(gen_event).

-export([add_handler/0, delete_handler/0]).

-export([init/1, handle_event/2, handle_call/2,
			handle_info/2, code_change/3, terminate/2]).
			
-record(state,{}).

init([]) ->
	{ok, #state{}}.

add_handler() ->
	shout_event:add_handler(?MODULE, []).

delete_handler() ->
	shout_event:delete_handler(?MODULE, []).

handle_event({create,{ServerName,Supervisor}}, State) ->
	error_logger:info_msg("~s : created ~s ~n",[Supervisor, ServerName]),
	{ok, State};
	
handle_event({wake_up, ServerName},State) ->
	error_logger:info_msg("~s : I've just woken_up ~n",[ServerName]),
	{ok, State};	

handle_event({accept,ClientName}, State) ->
	error_logger:info_msg("connection_server: accepted connection from ~s ~n",[ClientName]),
	{ok, State}.
	
handle_call(_Request, State) ->
	{reply,ok, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.	