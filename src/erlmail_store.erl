%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       ErlMail message store server
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
-module(erlmail_store).
-author('sjackson@simpleenigma.com').
-include("../include/imap.hrl").
-include("../include/erlmail.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([select/1]).

%% temp export; most likley private
-export([open/1,close/1,check/1]).
-export([status/0,status/1]).


start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%% API


select({UserName,DomainName}) -> gen_server:call(erlmail_store,{select,{UserName,DomainName}},10000).
	




%%%% Private Functions
status() -> ok.
status({MbxName,UserName,DomainName}) ->
	?D({MbxName,UserName,DomainName}), 
	ok.



open(MailBoxName) ->
	case check(MailBoxName) of
		true   -> create(MailBoxName);
		prmote -> promote(MailBoxName);
		false  -> create(MailBoxName,open)
	end.

close(MailBoxName) -> drop(MailBoxName).


check(MailBoxName) -> 
	Fun = fun() ->
		Open = mnesia:match_object(#message_store{mailbox = MailBoxName, state = open, _ = '_'}),
		All = mnesia:match_object(#message_store{mailbox = MailBoxName, _ = '_'}),
		[length(Open),length(All)]
	end,
	case mnesia:sync_transaction(Fun) of
		{atomic,[0,0]}   -> false;
		{atomic,[0,_]}   -> promote;
		{atomic,[1,_]}   -> true;
		{aborted,Reason} -> {error,Reason};
		{error,Reason}   -> {error,Reason}
	end.
	

promote(_MailBoxName) -> ok.

create(MailBoxName) -> create(MailBoxName,active).
create(MailBoxName,State) ->
	Fun = fun() ->
		mnesia:write(#message_store{client = self(), server = node(), mailbox = MailBoxName, state = State}) 
	end,
	case mnesia:sync_transaction(Fun) of
		{aborted,Reason} -> {error,Reason};
		{error,Reason} -> {error,Reason};
		{atomic,ok} -> {ok,State}
	end.

drop(MailBoxName) ->
	Fun = fun() ->
		case mnesia:match_object(#message_store{client = self(), server = node(), mailbox = MailBoxName, state = '_'}) of
			[#message_store{} = Store] -> mnesia:delete_object(Store) ;
			_ -> error
		end
		
	end,
	case mnesia:sync_transaction(Fun) of
		{aborted,Reason} -> {error,Reason};
		{error,Reason} -> {error,Reason};
		{atomic,error} -> {error,message_store_error};
		{atomic,ok} -> ok
	end.

















%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_,_,_) -> ok.
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
handle_call(Request,From,State) -> 
	?D({Request,From,State}),
	{noreply,State}.
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
handle_cast(_,_) -> ok.
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
handle_info({open,MailBoxName},State) ->
	?D({open,MailBoxName}),
	{noreply,State};
handle_info(test,State) ->
	io:format("Test Message Received~n"),
	{noreply,State};
handle_info(stop,State) ->
	{stop,normal,State};
handle_info(_Info,State) -> 
	?D({_Info,State}),
	{noreply,State}.
%%----------------------------------------------------------------------
%% @spec (Unused::term()) -> {ok, State}           |
%%                           {ok, State, Timeout}  |
%%                           ignore                |
%%                           {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------

init(_) -> 
    process_flag(trap_exit, true),
	erlmail_util:check(message_store,message_store,set,[server,mailbox]),
	System = mnesia_store,
	Domain  = erlmail_conf:lookup_atom(store_type_domain),
	User    = erlmail_conf:lookup_atom(store_type_user),
	Message = erlmail_conf:lookup_atom(store_type_message),
	Mailbox = erlmail_conf:lookup_atom(store_type_mailbox_store),
	State = #erlmail_store{system = System, domain = Domain, user = User, message = Message, mailbox = Mailbox},
	{ok, State}.
%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       'process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason,_State) -> 
	erlmail_util:remove(message_store),
	ok.










