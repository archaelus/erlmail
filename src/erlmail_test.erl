%%%----------------------------------------------------------------------
%%% File        : smtpc_fsm
%%% Author      : Stuart Jackson <sjackson@simpleenigma.com> [http://www.simpleenigma.com]
%%% Purpose     : ErlMail Test functions
%%% Created     : 2006-12-14
%%% Initial Rel : 0.0.1
%%% Updated     : 2007-10-18
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



c() -> c({10,1,1,175}).
c(IP) -> 
	{ok,Fsm} = imapc:connect(IP),
	cmd(Fsm,login,{?EMAIL, ?PASSWORD}),
	cmd(Fsm,select,"Test"),
%	cmd(Fsm,store,{[1,2,3,4,5,35],delete,[deleted]}),
%	cmd(Fsm,expunge),
%	cmd(Fsm,copy,{[35],"Test"}),
%	cmd(Fsm,uid,{fetch,{"1,2,3,4",[envelope]}}),
	cmd(Fsm,uid,{fetch,{[1],[flags]}}),


%	cmd(Fsm,close),
	cmd(Fsm,logout),
	ok.



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













m() -> m(35).
m(Id) ->
	Message = mail(Id),
	mime:decode(Message).

e() -> e(35).
e(Id) ->
	MIME = m(Id),
%	?D(MIME),
	imapd_fetch:envelope(MIME).

mail() -> mail(1).
mail(Pos) ->
	MailBox = mnesia_store:select({?MAILBOX,{?USER,?DOMAIN}}),
	MessageName = lists:nth(Pos,MailBox#mailbox_store.messages),
	Message = mnesia_store:select({MessageName,?USER,?DOMAIN}),
	Message#message.message.

f(1) -> f("ENVELOPE RFC822.SIZE UID FLAGS INTERNALDATE");
f(2) -> f("ALL FAST FULL");
f(3) -> f("RFC822 RFC822.SIZE RFC822.TEXT RFC822.HEADER");
f(4) -> f("(BODY.PEEK[HEADER.FIELDS (References X-Ref X-Priority X-MSMail-Priority X-MSOESRec Newsgroups)] ENVELOPE RFC822.SIZE UID FLAGS INTERNALDATE)");
f(5) -> f("BODY.PEEK[4.2.HEADER 1.1.HEADER]");
f(6) -> f("BODY[TEXT]<21.200>");


f(String) -> 
	io:format("Test: ~s~n",[String]),
	imapd_util:fetch_tokens(String).


re_split(1) -> re_split("test test test");
re_split(2) -> re_split("\"test test\" test");
re_split(3) -> re_split("test \"test test\"");
re_split(4) -> re_split("\"\" \"*\"");
re_split(5) -> re_split("\"test test\" \"test test\"");
re_split(6) -> re_split("\"Sent Items/testing\" \"Sent Items/testing folders\"");
re_split(7) -> re_split("\"Sent Items\" (MESSAGES UNSEEN)");

re_split(String) -> imapd_util:re_split(String,?RE_SPLIT,32,34).