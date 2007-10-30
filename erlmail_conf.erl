%%%---------------------------------------------------------------------------------------
%%% File        : erlmail_conf
%%% Author      : Stuart Jackson <sjackson@simpleenigma.com> [http://www.simpleenigma.com]
%%% Purpose     : ErlMail configuration routines
%%% Created     : 2007-10-18
%%% Initial Rel : 0.0.5
%%% Updated     : 2007-10-18
%%%---------------------------------------------------------------------------------------
-module(erlmail_conf).
-author('sjackson@simpleenigma.com').

-include("erlmail.hrl").
-include("imap.hrl").
-include("smtp.hrl").

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
		{value,{Key,Value}} -> Value;
		false -> []
	end.


lookup_atom(Key) -> lookup_atom(Key,read()).
lookup_atom(Key,State) when is_record(State,smtpd_fsm) -> lookup_atom(Key,State#smtpd_fsm.options);
lookup_atom(Key,State) when is_record(State,imapd_fsm) -> lookup_atom(Key,State#imapd_fsm.options);
lookup_atom(Key,List) ->
	case lookup(Key,List) of
		Value when is_list(Value) -> list_to_atom(Value);
		Value when is_atom(Value) -> Value
	end.