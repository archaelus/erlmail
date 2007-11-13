%%%---------------------------------------------------------------------------------------
%%% File        : erlmail
%%% Author      : Stuart Jackson <sjackson@simpleenigma.com> [http://www.simpleenigma.com]
%%% Purpose     : ErlMail package start and stop funcations
%%% Created     : 2006-12-14
%%% Initial Rel : 0.0.3
%%% Updated     : 2007-10-18
%%%---------------------------------------------------------------------------------------
-module(erlmail).
-author('sjackson@simpleenigma.com').

-export([start/0,stop/0,restart/0,reload/0]).


start() -> 
	io:format("Starting ErlMail ...~n"),
	application:start(smtpd),
	application:start(imapd).

stop() ->
	io:format("Stopping ErlMail ...~n"),
	application:stop(smtpd),
	application:stop(imapd).

restart() ->
	stop(),
	reload(),
	start().

reload() ->
	io:format("Reloading ErlMail Modules ...~n"),
	reload:reload([
		erlmail_conf,erlmail_util,
		gen_store,dets_store,mnesia_store,
		imapd,imapd_listener,imapd_fsm,imapd_app,imapd_util,imapd_cmd,imapd_util,imapd_ext,imapd_fetch,imapd_search,
		imap_parser,imap_scan,imapc,imapc_fsm,imapc_util,
		mime,
		smtpd_app,smtpd_fsm,smtpd_listener,smtpd_util,smtpd_cmd,
		smtpc,smtpc_fsm,smtpc_util,
		erlmail_test
		]).
	
