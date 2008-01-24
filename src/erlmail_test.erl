%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       ErlMail Test functions
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.1
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
-module(erlmail_test).
-author('sjackson@simpleenigma.com').
-include("../include/smtp.hrl").
-include("../include/erlmail.hrl").
-include("../include/imap.hrl").
-include("../include/mime.hrl").
-compile(export_all).

-define(RE_SPLIT,"^(\"[^\"]*\")").
-define(MAILBOX,  "INBOX").
-define(USER,     "simpleenigma").
-define(DOMAIN,   "erlsoft.net").
-define(EMAIL,    "simpleenigma@erlsoft.net").
-define(PASSWORD, "erlmail").
-define(TESTIP,   {10,1,1,175}).



s() ->
%	MailBoxName = {?MAILBOX,?USER,?DOMAIN},
%	erlmail_store:open(MailBoxName),
%	erlmail_store:close(MailBoxName),
	ok.




t() -> 
	Tag = 'test',
	R = [#imap_resp{tag='*'},#imap_resp{tag=Tag}],
	imapd_resp:insert(R),
	imapd_resp:resp_list(Tag).



c() -> c(?TESTIP,?EMAIL, ?PASSWORD).
c(IP,Email,Password) -> 
	{ok,Fsm} = imapc:connect(IP),
	cmd(Fsm,capability),
	cmd(Fsm,login,{Email,Password}),
	cmd(Fsm,select,"INBOX"),
	cmd(Fsm,fetch,{[1],[flags,internaldate,envelope,'rfc822.size',uid]}),
%	cmd(Fsm,uid,{fetch,{[1],[body]}}),
%	cmd(Fsm,uid,{fetch,{[1],['rfc822']}}),
%	cmd(Fsm,uid,{fetch,{[1],['rfc822.header']}}),
%	cmd(Fsm,uid,{fetch,{[1],['body.peek[]']}}),



	cmd(Fsm,close),
	cmd(Fsm,logout),
	ok.

d() ->
	Message = #message{name = {"MessageName",?USER,?DOMAIN}, message = m()},
	erlmail_store:deliver(Message).

e() -> 
	Message = #message{message = m()},
	MIME = mime:decode(m()),
	erlmail_store:expand(Message,MIME).

clear() -> 
	mnesia:clear_table(erlmail_message),
	mnesia:clear_table(erlmail_mailbox_store),
	mnesia_store:insert(#mailbox_store{name={"INBOX","simpleenigma","erlsoft.net"}}),
	mnesia_store:insert(#mailbox_store{name={"Drafts","simpleenigma","erlsoft.net"}}),
	mnesia_store:insert(#mailbox_store{name={"Sent Items","simpleenigma","erlsoft.net"}}),
	mnesia_store:insert(#mailbox_store{name={"INBOX","simpleenigma","orgonite.com"}}),
	mnesia_store:insert(#mailbox_store{name={"INBOX","simpleenigma","cloud-busters.com"}}),
	ok.


test_message() -> test_message(1).
test_message(1) -> test_message({76,204,23,210});
test_message(2) -> test_message({10,1,1,175});
test_message(IPAddress) -> test_message(IPAddress,"simpleenigma@erlsoft.net").
test_message(IPAddress,To) ->
	Port = 25,
	Host = "simpleenigma.com",
	From = "sjackson@simpleenigma.com",
	Message = m(From,[To],"Test","This is a test message"),
	smtpc:sendmail(IPAddress,Port,Host,From,To,Message).



m() ->
	m("sjackson@simpleenigma.com",["simpleenigma@erlsoft.net",{"simpleenigmainc","gmail.com","Stuart Jackson"}],"Test","This is a test message").
m(From,To,Subject,Message) ->
	MIME = #mime{header=[{from,From},{to,To},{subject,Subject}],body = Message},
	mime:encode(MIME).






%%% Keep these funcations

cmd(Fsm,Cmd) ->
	Resp = imapc:Cmd(Fsm),
	?D(Resp),
	Resp.
cmd(Fsm,Cmd,{Arg1,Arg2}) ->
	Resp = imapc:Cmd(Fsm,Arg1,Arg2),
	?D(Resp),
	Resp;
cmd(Fsm,Cmd,{Arg1,Arg2,Arg3}) ->
	Resp = imapc:Cmd(Fsm,Arg1,Arg2,Arg3),
	?D(Resp),
	Resp;
cmd(Fsm,Cmd,Arg) ->
	Resp = imapc:Cmd(Fsm,Arg),
	?D(Resp),
	Resp.