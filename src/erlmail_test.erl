%%%----------------------------------------------------------------------
%%% File        : smtpc_fsm
%%% Author      : Stuart Jackson <sjackson@simpleenigma.com> [http://www.simpleenigma.com]
%%% Purpose     : ErlMail Test functions
%%% Created     : 2006-12-14
%%% Initial Rel : 0.0.1
%%% Updated     : 2007-10-18
%%%----------------------------------------------------------------------
-module(erlmail_test).
-author('sjackson@simpleenigma.com').
-include("../include/smtp.hrl").
-include("../include/erlmail.hrl").
-include("../include/imap.hrl").
-include("../include/mime.hrl").
-compile(export_all).

-define(RE_SPLIT,"^(\"[^\"]*\")").
-define(MAILBOX,"INBOX").
-define(USER,   "simpleenigma").
-define(DOMAIN, "erlsoft.net").





m() -> m(1).

m(Id) ->
	Message = mail(Id),
	mime:decode(Message).





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


e() ->
	erlmail_conf:lookup(server_imap_extentions).

re_split(1) -> re_split("test test test");
re_split(2) -> re_split("\"test test\" test");
re_split(3) -> re_split("test \"test test\"");
re_split(4) -> re_split("\"\" \"*\"");
re_split(5) -> re_split("\"test test\" \"test test\"");
re_split(6) -> re_split("\"Sent Items/testing\" \"Sent Items/testing folders\"");
re_split(7) -> re_split("\"Sent Items\" (MESSAGES UNSEEN)");

re_split(String) -> imapd_util:re_split(String,?RE_SPLIT,32,34).