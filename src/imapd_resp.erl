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

-behaviour(gen_server).

%% External API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
% Mnesia control functions
-export([create/0,join/0,exit/0,check/0,check/1]).





start_link(_,_) -> ok.
code_change(_,_,_) -> ok.
handle_call(_,_,_) -> ok.
handle_cast(_,_) -> ok.
handle_info(_,_) -> ok.
init(_) -> check(join).
terminate(_,_) -> exit().


% mnesia rotines
create() -> 
	mnesia:create_table(imap_resp,[{ram_copies,[node()]},{attributes,record_info(fields, imap_resp)},{type,bag}]),
	mnesia:add_table_index(imap_resp,mailbox),
	mnesia:add_table_index(imap_resp,timestamp),
	ok.

join() -> mnesia:add_table_copy(imap_resp,node(),ram_copies).

exit() -> 
	case catch mnesia:table_info(imap_resp,ram_copies) of
		{'EXIT',_Reason} -> {error,table_not_found};
		[Node] when Node == node() -> mnesia:delete_table(imap_resp);
		NodeList -> 
			case lists:member(node(),NodeList) of
				true -> mnesia:del_table_copy(imap_resp,node());
				false -> ok
			end
	end.


check() -> 
	case lists:keysearch(mnesia,1,application:loaded_applications()) of
		{value,_} -> ok;		
		_ -> mnesia:start()
	end,
	case catch mnesia:table_info(imap_resp,ram_copies) of
		[] -> {error,no_ram_copies};
		NodeList when is_list(NodeList) -> 
			case lists:member(node(),NodeList) of
				true -> ok;
				false -> {error,local_node_not_in_node_list}
			end;
		{'EXIT',_Reason} -> {error,table_not_found}
	end.

check(join) ->
	case check() of
		ok -> ok;
		{error,table_not_found} -> create();
		{error,local_node_not_in_node_list} -> join()
	end.
	























