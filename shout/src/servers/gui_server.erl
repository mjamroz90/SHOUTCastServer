-module(gui_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/1,stop/0,break_streaming/1]).		 
		 
-compile(export_all).

-include_lib("wx/include/wx.hrl").

-define(SERVER,?MODULE).
-define(DEFAULT_PORT,3000).

-define(ABOUT,?wxID_ABOUT).
-define(EXIT,?wxID_EXIT).
-define(ADD,132).
-define(DELETE,133).
-define(SAVE,131).
-define(STOP,134).
-define(START,135).
-define(LOGSCREEN,136).
-define(LISTBOX,137).
-define(ITEMBOX,138).
-define(ADDITEM,139).
-define(DELETEITEM,140).

-record(state,{frame,listbox,itembox,itemlist,logscreen,going}).

%API==============================================================

start_link(File) ->
	gen_server:start_link({local,?SERVER},?MODULE,[File],[]).

stop() ->
	gen_server:cast(?SERVER,stop).
	
break_streaming(Pid) ->
	gen_server:cast(?SERVER,{break_streaming,Pid}).
	
%Callbacks========================================================
	
init([File]) ->	
	wx:new(),
	shout_event:wake_up("gui_server"),
	shout_cache:start_link(File),
	{Frame,ListBox,ItemBox,LoggingScreen} = init1(),	
	wxFrame:show(Frame),	
	{ok, #state{frame = Frame, listbox = ListBox, itembox = ItemBox, itemlist = [], logscreen = LoggingScreen,going=false}}.

handle_call(_Msg,_From,State) ->	
	{reply,ok,State}.
	
handle_cast(stop,State) ->
	wx:destroy(),
	{stop,normal,State};

handle_cast({break_streaming,Pid},#state{logscreen = LScreen} = State) ->	
	log_message(pid_to_list(Pid)++" zerwal polaczenie",LScreen),	
	{noreply,State}.
	
terminate(_Reason,_State) ->		
	ok.
	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.

handle_info(#wx{id=?ADD, event=#wxCommand{type=command_togglebutton_clicked}}, 
				#state{frame = Frame, listbox = ListBox, itembox = ItemBox} = State) ->
	Prompt = "Podaj Nazwe Listy.",
	MD = wxTextEntryDialog:new(Frame,Prompt,[{caption, "Nowa Lista"}]), 				     
		case wxTextEntryDialog:showModal(MD) of 
			?wxID_OK -> 
				Str = wxTextEntryDialog:getValue(MD),
				ListName = parse(Str),
				case ListName of
					"" -> ok;							
					_ -> 	
						wxListBox:append(ListBox,Str),
						shout_cache:insert(list_to_atom(ListName),[]),
						shout_cache:save(list_to_atom(ListName),[]),
						wxListBox:setSelection(ListBox,wxListBox:getCount(ListBox)-1),
						wxListBox:set(ItemBox,[])				
				end;
			_ -> 
				ok
		end,
		wxTextEntryDialog:destroy(MD),
	{noreply,State#state{itemlist = []}};
	
handle_info(#wx{id=?DELETE, event=#wxCommand{type=command_togglebutton_clicked}}, 
				#state{frame = Frame, listbox=ListBox, itembox=ItemBox} = State) ->	
		case wxListBox:getSelection(ListBox) of
			?wxNOT_FOUND ->
				void;
			N -> 
				SelectedList = wxListBox:getString(ListBox,N),				
				Msg = "lista " ++ SelectedList ++" zostala usunieta",
				MD = wxMessageDialog:new(Frame,Msg,[{caption, "Delete"}]), 				     
				case wxMessageDialog:showModal(MD) of 
					?wxID_OK ->							
						wxListBox:delete(ListBox,N),					
						shout_cache:delete(list_to_atom(SelectedList)),
						shout_cache:disk_delete(list_to_atom(SelectedList)),
						file:delete(filename:join(get_env(play_Dir,"./pliki/playlists"),SelectedList++".tmp")),
						wxListBox:set(ItemBox,[]);
					_ -> 
						ok
				end		
		end,
	{noreply,State};
			
handle_info(#wx{obj = Frame, event=#wxClose{type= close_window}}, 
				#state{ frame=Frame } = State) ->
		io:format("jest dobrze, bedzie jeszcze dobrzej~n"),
		shout_cache:stop(),
		%wxWindow:close(Frame,[]),		
		stop(),
		application:stop(shout),
		{noreply,State};	
		
handle_info(#wx{id=?LISTBOX, event=#wxCommand{type=command_listbox_selected}},
				#state{ listbox=ListBox, itembox=ItemBox, itemlist = ItemList} = State) ->				
		case wxListBox:getSelection(ListBox) of
			?wxNOT_FOUND ->
				NewList = ItemList,
				void;
			N -> 
				SelectedList = wxListBox:getString(ListBox,N),
				{ok,Items} = shout_cache:lookup(list_to_atom(SelectedList)),					
				Items_to_insert = lists:map(fun({T,_}) -> T end, Items),
				NewList = Items,
				wxListBox:set(ItemBox,Items_to_insert)
		end,		
	{noreply,State#state{itemlist = NewList}};
	
handle_info(#wx{id=?ADDITEM, event=#wxCommand{type=command_togglebutton_clicked}},
				#state{frame=Frame, listbox=ListBox, itembox=ItemBox, itemlist = ItemList} = State ) ->	
		case wxListBox:getSelection(ListBox) of
			?wxNOT_FOUND ->
				PlayList = [],
				void;				
			_ -> 
				FileDialog_Item = wxFileDialog:new(Frame, [{message,"Wybierz"}, {pos,{0,0}}, {sz,{400,250}}]),			
				PlayList=fill_list(FileDialog_Item,ItemBox,[]),			
				wxFileDialog:destroy(FileDialog_Item)				
		end,
	{noreply, State#state{itemlist = lists:append(ItemList,PlayList)}};

handle_info(#wx{id=?DELETEITEM, event=#wxCommand{type=command_togglebutton_clicked}},
				#state{ listbox=ListBox, itembox=ItemBox, itemlist = ItemList} = State ) ->	
		case wxListBox:getSelection(ListBox) of
			?wxNOT_FOUND ->
				NewList = ItemList,
				void;
			N ->
				Item = wxListBox:getString(ItemBox,N),
				NewList = lists:filter(fun({X,_}) -> X /= Item end, ItemList),
				wxListBox:delete(ItemBox,N)
		end,	
	{noreply, State#state{itemlist = NewList}};

handle_info(#wx{id=?SAVE, event=#wxCommand{type=command_togglebutton_clicked}},
				#state{frame=Frame, listbox=ListBox, itemlist = ItemList} = State) ->	
		case wxListBox:getSelection(ListBox) of
			?wxNOT_FOUND ->
				void;
			N ->
				SelectedList = wxListBox:getString(ListBox,N),
				Msg = "lista " ++ SelectedList ++" zostala zachowana",
				MD = wxMessageDialog:new(Frame,Msg,[{caption, "Save"}]),
				Paths = lists:map(fun({_,X}) -> X end, ItemList),	
				case wxMessageDialog:showModal(MD) of 
					?wxID_OK ->							
						shout_cache:insert(list_to_atom(SelectedList),ItemList),
						shout_cache:save(list_to_atom(SelectedList),ItemList),
						mp3_manager:start3(Paths,filename:join(get_env(play_Dir,"./pliki/playlists"),SelectedList));	
					_ -> 
						ok					
				end,				
				wxMessageDialog:destroy(MD)
		end,
	{noreply,State};

handle_info(#wx{id=?START, event=#wxCommand{type=command_togglebutton_clicked}},
			#state{frame=Frame, listbox=ListBox, going = Flag, logscreen = LScreen} = State) ->			
		case Flag of
			true ->
				Msg = "Server chodzi, wcisnij stop",
				MD = wxMessageDialog:new(Frame,Msg,[{caption, "Info"}]),
				case wxMessageDialog:showModal(MD) of 
					_ -> 
						wxMessageDialog:destroy(MD),
						ok	
				end,
				Flag1 = true;
			false ->
				case wxListBox:getSelection(ListBox) of
					?wxNOT_FOUND ->
						Msg = "Wybierz liste",
						MD = wxMessageDialog:new(Frame,Msg,[{caption, "Info"}]),
						case wxMessageDialog:showModal(MD) of 
							_ -> 
								wxMessageDialog:destroy(MD),
								ok
						end,
						Flag1 = false;
					N -> 
						SelectedList = wxListBox:getString(ListBox,N),						
						song_server:replace_list(filename:join(get_env(play_Dir,"./pliki/playlists"),SelectedList ++ ".tmp")),						
						launch_conn_server(),
						log_message("Server chodzi z lista - " ++ SelectedList,LScreen),
						Flag1 = true
				end
		end,
	{noreply, State#state{going = Flag1}};
	
handle_info(#wx{id=?STOP, event=#wxCommand{type=command_togglebutton_clicked}},
			#state{frame=Frame, going = Flag, logscreen = LScreen} = State) ->
		case Flag of
			false ->
				Msg = "Server nie chodzi, wcisnij start",
				MD = wxMessageDialog:new(Frame,Msg,[{caption, "Info"}]),
				case wxMessageDialog:showModal(MD) of 
					_ -> 
						wxMessageDialog:destroy(MD),
						ok	
				end;
			true ->
				conn_sup:delete_all(),				
				log_message("Server przestal chodzic",LScreen)
		end,
	{noreply, State#state{going = false}}.
	
%Internal==========================================================================

get_env(Name,Default) ->
	case application:get_env(shout,Name) of
		{ok,L} -> L;
		undefined -> Default
	end.

log_message(Msg,LScreen) ->
	{H,M,S} = time(),
	L = tuple_to_list({H,M,S}),
	[H1,M1,S1] = lists:map(fun(X) -> case X div 10 of
										0 -> 
											string:concat("0",integer_to_list(X));	
										_ -> 
											integer_to_list(X) 
									end end, L),
	Text_to_Append = H1 ++":"++ M1 ++":" ++S1 ++" - " ++Msg++"\n",
	wxTextCtrl:appendText(LScreen,Text_to_Append).
	
launch_conn_server() ->
	case conn_sup:start_child() of
		{ok,Pid} ->			
			{ok,Pid};	
		Other -> 
			{error,Other}
	 end.

%GUI_INIT===========================================================================
	 
init1() ->
	wxXmlResource:initAllHandlers(wxXmlResource:get()),	
	wxXmlResource:load(wxXmlResource:get(),get_env(gui_file,"./pliki/gui_file.xml")),
		
	Frame = wxXmlResource:loadFrame(wxXmlResource:get(),wx:null(),"mainFrame"),			
	LoggingScreen = wxXmlResource:xrcctrl(Frame,"logScreen",wxTextCtrl),
	wxTextCtrl:setEditable(LoggingScreen,false),
	
	StartButton = wxXmlResource:xrcctrl(Frame,"startButton",wxToggleButton),
	wxToggleButton:setId(StartButton,?START),
	wxToggleButton:connect(StartButton,command_togglebutton_clicked),
	
	StopButton = wxXmlResource:xrcctrl(Frame,"stopButton",wxToggleButton),
	wxToggleButton:setId(StopButton,?STOP),
	wxToggleButton:connect(StopButton,command_togglebutton_clicked),
	
	AddButton = wxXmlResource:xrcctrl(Frame,"addButton",wxToggleButton),
	wxToggleButton:setId(AddButton,?ADD),
	wxToggleButton:connect(AddButton,command_togglebutton_clicked),
	
	DeleteButton = wxXmlResource:xrcctrl(Frame,"deleteButton",wxToggleButton),
	wxToggleButton:setId(DeleteButton,?DELETE),
	wxToggleButton:connect(DeleteButton,command_togglebutton_clicked),
	
	SaveButton = wxXmlResource:xrcctrl(Frame,"saveButton",wxToggleButton),
	wxToggleButton:setId(SaveButton,?SAVE),
	wxToggleButton:connect(SaveButton,command_togglebutton_clicked),
	
	AddItemButton = wxXmlResource:xrcctrl(Frame,"addItemButton",wxToggleButton),
	wxToggleButton:setId(AddItemButton,?ADDITEM),
	wxToggleButton:connect(AddItemButton,command_togglebutton_clicked),
	
	DeleteItemButton = wxXmlResource:xrcctrl(Frame,"deleteItemButton",wxToggleButton),
	wxToggleButton:setId(DeleteItemButton,?DELETEITEM),
	wxToggleButton:connect(DeleteItemButton,command_togglebutton_clicked),
		
	ItemBox = wxXmlResource:xrcctrl(Frame,"itemBox",wxListBox),
	ListBox = wxXmlResource:xrcctrl(Frame,"listBox",wxListBox),
	wxListBox:setId(ListBox,?LISTBOX),
	wxListBox:connect(ListBox,command_listbox_selected),
	
	InitList = lists:map(fun(X) -> atom_to_list(X) end, shout_cache:load_all()),	
	wxListBox:set(ListBox,InitList),	
	{Frame,ListBox,ItemBox,LoggingScreen}.
	
%=====================================================================================
	
fill_list(FileDialog_Item,ItemBox,PlayList) ->
	case wxFileDialog:showModal(FileDialog_Item) of
		?wxID_OK ->		
			Name = wxFileDialog:getFilename(FileDialog_Item),
			Dir = wxFileDialog:getDirectory(FileDialog_Item),
			wxListBox:append(ItemBox,Name),			
			fill_list(FileDialog_Item,ItemBox,[{Name,filename:join(Dir,Name)}|PlayList]);			
		_ -> 
			lists:reverse(PlayList)
	end.	

parse(Str) ->
	Tokens = string:tokens(Str,"\s\t"),	
	Result = concat(Tokens,[]),
	string:sub_string(Result,2,string:len(Result)).
	
concat([], List) -> List;	
concat([H|T], List) ->
	concat(T, List ++ "_" ++ H).	
