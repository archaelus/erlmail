%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       IMAP server response processing
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.6
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Stuart Jackson, Simple Enigma, Inc. All Righs Reserved
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%
%%%---------------------------------------------------------------------------------------
-module(imapd_resp).
-author('sjackson@simpleenigma.com').
-include("../include/imap.hrl").
-include("../include/erlmail.hrl").
-define(MNESIA_TABLE_NAME,imap_resp).
-define(MNESIA_TABLE_RECORD,imap_resp).
-define(MNESIA_EXTRA_KEYS,[mailbox,timestamp]).
-define(MNESIA_TABLE_TYPE,bag).


-behaviour(gen_server).

%% External API
-export([start_link/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
% Mnesia control functions
-export([create/0,join/0,remove/0,check/0,check/1]).




start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE,stop).


%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(stop,State) ->
	{stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info(_,_) -> ok.


%%----------------------------------------------------------------------
%% @spec (Unused::term()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------

init(_) -> 
    process_flag(trap_exit, true),
	check(join),
	{ok, ok}.


%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       'process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason,_State) -> remove().


%%-------------------------------------------------------------------------
%% @spec () -> ok | {error,Reason::atom()}
%% @doc  create imap_resp mnesia table
%% @end
%% @private
%%-------------------------------------------------------------------------
create() -> 
	mnesia:create_table(?MNESIA_TABLE_NAME,[{ram_copies,[node()]},{attributes,record_info(fields, ?MNESIA_TABLE_RECORD)},{type,?MNESIA_TABLE_TYPE}]),
	lists:map(fun(Key) -> 
		mnesia:add_table_index(?MNESIA_TABLE_NAME,Key)
		end,?MNESIA_EXTRA_KEYS),
	ok.
%%-------------------------------------------------------------------------
%% @spec () -> ok | {error,Reason::atom()}
%% @doc  Join existing imap_resp mnesia table
%% @end
%% @private
%%-------------------------------------------------------------------------
join() -> mnesia:add_table_copy(?MNESIA_TABLE_NAME,node(),ram_copies).
%%-------------------------------------------------------------------------
%% @spec () -> ok | {error,Reason::atom()}
%% @doc  Remove node from mnesia table list
%% @end
%% @private
%%-------------------------------------------------------------------------
remove() -> 
	case catch mnesia:table_info(?MNESIA_TABLE_NAME,ram_copies) of
		{'EXIT',_Reason} -> {error,table_not_found};
		[Node] when Node == node() -> 
			mnesia:delete_table(?MNESIA_TABLE_NAME),
			ok;
		NodeList -> 
			case lists:member(node(),NodeList) of
				true -> 
					mnesia:del_table_copy(?MNESIA_TABLE_NAME,node()),
					ok;
				false -> ok
			end
	end.

%%-------------------------------------------------------------------------
%% @spec () -> ok | {error,Reason::atom()}
%% @doc  Checks to see if mnesia is started and imap_resp table is active.
%% @end
%% @private
%%-------------------------------------------------------------------------
check() -> 
	case lists:keysearch(mnesia,1,application:loaded_applications()) of
		{value,_} -> ok;		
		_ -> mnesia:start()
	end,
	case catch mnesia:table_info(?MNESIA_TABLE_NAME,ram_copies) of
		[] -> {error,no_ram_copies};
		NodeList when is_list(NodeList) -> 
			case lists:member(node(),NodeList) of
				true -> ok;
				false -> {error,local_node_not_in_node_list}
			end;
		{'EXIT',_Reason} -> {error,table_not_found}
	end.

%%-------------------------------------------------------------------------
%% @spec (Type::atom()) -> ok | {error,Reason::atom()}
%% @doc  If Type=['join'|'create'] node will run check/0 then init 
%%       imap_resp table. If Type is anything else only check/0 is run.
%% @end
%% @private
%%-------------------------------------------------------------------------
check(join) ->
	case check() of
		ok -> ok;
		{error,table_not_found} -> create();
		{error,local_node_not_in_node_list} -> join()
	end;
check(create) -> check(join);
check(_) -> check().
	























