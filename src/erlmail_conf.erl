%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       ErlMail configuration routines
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.5
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
-module(erlmail_conf).
-author('sjackson@simpleenigma.com').

-include("../include/erlmail.hrl").
-include("../include/imap.hrl").
-include("../include/smtp.hrl").

-export([filename/0]).
-export([read/0,read/1,lookup/1,lookup/2]).
-export([lookup_atom/1,lookup_atom/2]).




filename() ->
	case init:get_argument(erlmail) of
		{ok,[[FileName]]} -> 
			case filelib:is_regular(FileName) of
				true -> {ok,FileName};
				false -> {error,config_file_not_found}
			end;
		_ -> {error,undefined}
	end.


read() -> 
	case filename() of
		{ok,FileName} -> read(FileName);
		{error,Reason} -> {error,Reason}
	end.
read(FileName) ->
	{ok,Bin} = file:read_file(FileName),
	List = binary_to_list(Bin),
	Tokens = string:tokens(List,"\r\n"),
	parse(Tokens,[]).


parse([],Acc) -> lists:reverse(Acc);
parse([H|T],Acc) ->
	case lists:split(string:chr(H,32),H) of
		{[],Key} -> parse(T,[{list_to_atom(http_util:to_lower(Key)),[]}|Acc]);
		{Key,Value} -> parse(T,[{list_to_atom(http_util:to_lower(string:strip(Key))),string:strip(Value)}|Acc])
	end.


lookup(Key) -> lookup(Key,read()).

lookup(Key,State) when is_record(State,smtpd_fsm) -> lookup(Key,State#smtpd_fsm.options);
lookup(Key,State) when is_record(State,imapd_fsm) -> lookup(Key,State#imapd_fsm.options);
lookup(Key,List) ->
	case lists:keysearch(Key,1,List) of
		{value,{Key,Value}} -> cleanup(Key,Value);
		false -> cleanup(Key,[])
	end.



% Cleanup config info and supply defaults for some settings if they don't exist
cleanup(server_smtp_port,[])                      -> 25;
cleanup(server_smtp_port,Port) when is_list(Port) -> list_to_integer(Port);
cleanup(server_smtp_max_connection,[])                    -> 25;
cleanup(server_smtp_max_connection,Max) when is_list(Max) -> list_to_integer(Max);
cleanup(server_smtp_greeting,[]) -> "ErlMail http://erlsoft.org (NO UCE)";

cleanup(server_imap_greeting,[]) -> "IMAP4 server ready";
cleanup(server_imap_greeting_capability,Boolean) when Boolean /= "true"; Boolean /= "false" -> false;
cleanup(server_imap_greeting_capability,Boolean)  -> list_to_atom(Boolean);
cleanup(server_imap_port,[])                      -> 143;
cleanup(server_imap_port,Port) when is_list(Port) -> list_to_integer(Port);
cleanup(server_imap_hierarchy,[]) -> '/';
cleanup(server_imap_hierarchy,Hierarchy) when is_list(Hierarchy) -> list_to_atom(Hierarchy);

cleanup(store_type_domain,[])                               -> dets_store;
cleanup(store_type_domain,Store) when is_list(Store)        -> list_to_atom(Store);
cleanup(store_type_user,[])                                 -> dets_store;
cleanup(store_type_user,Store) when is_list(Store)          -> list_to_atom(Store);
cleanup(store_type_message,[])                              -> dets_store;
cleanup(store_type_message,Store) when is_list(Store)       -> list_to_atom(Store);
cleanup(store_type_mailbox_store,[])                        -> dets_store;
cleanup(store_type_mailbox_store,Store) when is_list(Store) -> list_to_atom(Store);

cleanup(dets_table_domain,[])        -> domain;
cleanup(dets_table_user,[])          -> user;
cleanup(dets_table_message,[])       -> message;
cleanup(dets_table_mailbox_store,[]) -> mailbox_store;





cleanup(_Key,Value) -> Value.


lookup_atom(Key) -> lookup_atom(Key,read()).
lookup_atom(Key,State) when is_record(State,smtpd_fsm) -> lookup_atom(Key,State#smtpd_fsm.options);
lookup_atom(Key,State) when is_record(State,imapd_fsm) -> lookup_atom(Key,State#imapd_fsm.options);
lookup_atom(Key,List) ->
	case lookup(Key,List) of
		Value when is_list(Value) -> list_to_atom(Value);
		Value when is_atom(Value) -> Value
	end.