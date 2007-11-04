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
-compile(export_all).

-define(RE_SPLIT,"^\".*\"").


e() ->
	erlmail_conf:lookup(server_imap_extentions).

re_split(1) -> re_split("test test test");
re_split(2) -> re_split("\"test test\" test");
re_split(3) -> re_split("test \"test test\"");
re_split(4) -> re_split("\"\" \"*\"");

re_split(String) -> imapd_util:re_split(String).








g() -> g("* OK [CAPABILITY IMAP4REV1 LOGIN-REFERRALS STARTTLS LOGINDISABLED AUTH=GSSAPI] follmer.hem.za.org IMAP4rev1 2003.339 at Fri, 26 Oct 2007 21:51:41 +0200 (CEST)").

g(Line) ->
	parse(Line).



parse(Line) -> 
	case imap_scan:imap_string(Line ++ [0]) of
		{ok,Tokens,_Lines} -> 
			?D(Tokens),
			imap_parse(Tokens);
		_ -> {error,token_error}
	end.

imap_parse(Tokens) ->
	case catch imap_parser:parse(Tokens) of
		{error,Reason} -> {error,Reason};
		{'EXIT',_} ->
			case imapc_util:clean_tokens(Tokens) of
				{error,Reason} -> {error,Reason};
				NewTokens -> imap_parse(NewTokens)
			end;
		{ok,Results} -> Results
	end.



	



m() ->
	MB = #mailbox{name="INBOX"},
	imapd_util:mailbox_info(MB,{"simpleenigma","erlsoft.net"}).

c() -> c({10,1,1,175}).

c(IP) ->
	{ok,Fsm} = imapc:connect(IP),
	cmd(Fsm,login,"simpleenigma@erlsoft.net","erlmail"),
%	cmd(Fsm,select,"INBOX"),
%	cmd(Fsm,examine,"INBOX"),
%	cmd(Fsm,create,"Clients"),
%	cmd(Fsm,rename,"test","testing"),
%	cmd(Fsm,subscribe,"test"),
%	cmd(Fsm,unsubscribe,"test"),
%	cmd(Fsm,delete,"test"),
%	cmd(Fsm,status,"inbox",[messages,recent,uidnext,uidvalidity,unseen]), % 
	cmd(Fsm,list,"","Sent*"),

%	cmd(Fsm,close),
	cmd(Fsm,logout),
	ok.


cmd(Fsm,Cmd) -> 
	R = imapc:Cmd(Fsm),
	?D(R).
cmd(Fsm,Cmd,Arg1) -> 
	R = imapc:Cmd(Fsm,Arg1),
	?D(R).
cmd(Fsm,Cmd,Arg1,Arg2) -> 
	R = imapc:Cmd(Fsm,Arg1,Arg2),
	?D(R).
	