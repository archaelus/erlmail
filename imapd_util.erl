%%%----------------------------------------------------------------------
%%% File        : imapd_util
%%% Author      : Stuart Jackson <sjackson@simpleenigma.com> [http://www.simpleenigma.com]
%%% Purpose     : IMAP server utility funcations
%%% Created     : 2007-10-27
%%% Initial Rel : 0.0.6
%%% Updated     : 2007-10-27
%%%----------------------------------------------------------------------
-module(imapd_util).
-author('sjackson@simpleenigma.com').
-include("imap.hrl").
-include("erlmail.hrl").

-export([parse/1,clean/1]).
-export([split_at/1,split_at/2]).
-export([send/2,out/2,out/3]).
-export([mailbox_info/1,mailbox_info/2,mailbox_info/3]).
-export([greeting_capability/1,greeting/1]).
-export([flags_resp/2]).
-export([status_flags/1,status_resp/1,status_info/2]).
-export([heirachy_char/0]).
-export([seq_to_list/1,list_to_seq/1]).



parse(Line) ->
	case split_at(Line,32) of
		{Tag,[]} -> 
			Command = [],
			Data = [];
		{Tag,Rest} ->
			case split_at(Rest,32) of
				{Command,[]} ->
					Data = [];
				{Command,Data} -> ok
			end
	end,
	% TODO: parse DATA differently depending on command
	NextData = case list_to_atom(string:strip(http_util:to_lower(Command))) of
		Cmd = login       -> clean(split_at(Data,32));
		Cmd = select      -> clean(inbox(Data));
		Cmd = delete      -> clean(inbox(Data));
		Cmd = rename      -> split_at(Data);
		Cmd = subscribe   -> clean(inbox(Data));
		Cmd = unsubscribe -> clean(inbox(Data));
		Cmd = status      -> 
			{MailBox,Flags} = split_at(Data),
			{clean(inbox(MailBox)),Flags};
		Cmd = store      -> 
			{Seq,FlagData} = imapd_util:split_at(Data),
			{Action,Flags} = imapd_util:split_at(FlagData),
			{Seq,Action,Flags};
		Cmd               -> Data
	end,
	#imap_cmd{
		line = Line,
		tag  = list_to_atom(string:strip(Tag)),
		cmd  = Cmd,
		data = NextData}.


split_at(String) -> split_at(String,32).
split_at(String,Chr) -> 
	case string:chr(String, Chr) of
		0 -> {String,[]};
		Pos -> 
			case lists:split(Pos,String) of
				{One,Two} -> {string:strip(One),Two};
				Other -> Other
			end
	end.



response(Resp) when is_record(Resp,imap_resp) -> response(Resp,[]).
%% TAG
response(#imap_resp{_ = []} = _Resp,Acc) -> string:strip(lists:flatten(lists:reverse(Acc)));
response(#imap_resp{tag = Tag} = Resp,Acc) when is_atom(Tag), Tag /= [] -> 
	response(Resp#imap_resp{tag = []},[32,atom_to_list(Tag)|Acc]);
response(#imap_resp{tag = Tag} = Resp,Acc) when is_list(Tag), Tag /= [] -> 
	response(Resp#imap_resp{tag = []},[32,Tag|Acc]);
%% STATUS
response(#imap_resp{status = Status} = Resp,Acc) when is_atom(Status), Status /= [] -> 
	response(Resp#imap_resp{status = []},[32,http_util:to_upper(atom_to_list(Status))|Acc]);
response(#imap_resp{status = Status} = Resp,Acc) when is_list(Status), Status /= [] -> 
	response(Resp#imap_resp{status = []},[32,http_util:to_upper(Status)|Acc]);
response(#imap_resp{status = Status} = Resp,Acc) when is_integer(Status), Status /= [] -> 
	response(Resp#imap_resp{status = []},[32,integer_to_list(Status)|Acc]);
%% CODE
response(#imap_resp{code = {capability,Capability}} = Resp,Acc) ->
	response(Resp#imap_resp{code = []},[32,93,Capability,91|Acc]);
response(#imap_resp{code = {unseen,UnSeen}} = Resp,Acc) ->
	response(Resp#imap_resp{code = []},[32,93,integer_to_list(UnSeen),32,"UNSEEN",91|Acc]);
response(#imap_resp{code = {uidvalidity,UIDValidity}} = Resp,Acc) ->
	response(Resp#imap_resp{code = []},[32,93,integer_to_list(UIDValidity),32,"UIDVALIDITY",91|Acc]);
response(#imap_resp{code = {uidnext,UIDNext}} = Resp,Acc) ->
	response(Resp#imap_resp{code = []},[32,93,integer_to_list(UIDNext),32,"UIDNEXT",91|Acc]);
response(#imap_resp{code = {permanentflags,PermanentFlags}} = Resp,Acc) ->
	response(Resp#imap_resp{code = []},[32,93,flags_resp(PermanentFlags),32,"PERMANENTFLAGS",91|Acc]);
response(#imap_resp{code = 'read-write'} = Resp,Acc) ->
	response(Resp#imap_resp{code = []},[32,93,"READ-WRITE",91|Acc]);
response(#imap_resp{code = 'read-only'} = Resp,Acc) ->
	response(Resp#imap_resp{code = []},[32,93,"READ-ONLY",91|Acc]);
response(#imap_resp{code = Code} = Resp,Acc) when is_list(Code), Code /= [] ->
	response(Resp#imap_resp{code = []},[32,93,[],91|Acc]);
%% CMD
response(#imap_resp{cmd = Cmd} = Resp,Acc) when is_atom(Cmd), Cmd /= [] -> 
	response(Resp#imap_resp{cmd = []},[32,http_util:to_upper(atom_to_list(Cmd))|Acc]);
response(#imap_resp{cmd = Cmd} = Resp,Acc) when is_list(Cmd), Cmd /= [] -> 
	response(Resp#imap_resp{cmd = []},[32,http_util:to_upper(Cmd)|Acc]);
%% DATA
response(#imap_resp{data = {flags,Flags}} = Resp,Acc) ->
	response(Resp#imap_resp{data = []},[32,flags_resp(Flags)|Acc]);
response(#imap_resp{data = {status,MailBoxName,Info}} = Resp,Acc) ->
	response(Resp#imap_resp{data = []},[32,status_resp(Info),32,MailBoxName|Acc]);

response(#imap_resp{data = Data} = Resp,Acc) when is_list(Data), Data /= [] ->
	response(Resp#imap_resp{data = []},[32,41,[],40|Acc]);
%% INFO
response(#imap_resp{info = Info} = Resp,Acc) when is_list(Info), Info /= [] -> 
	response(Resp#imap_resp{info = []},[32,Info|Acc]);
response(Resp,Acc) -> 
	?D({Resp,Acc}),
	{error,unkown_response}.


flags_resp([]) -> "()";
flags_resp(List) -> flags_resp(List,[]).
flags_resp([H|T],Acc) when is_atom(H) ->
	flags_resp(T,[http_util:to_upper(atom_to_list(H)),92,32|Acc]);
flags_resp([],Acc) -> "(" ++ string:strip(lists:flatten(lists:reverse(Acc))) ++ ")".
	
status_resp([]) -> "()";
status_resp(List) -> status_resp(List,[]).

status_resp([{Type,Info}|T],Acc) ->
	status_resp(T,[32,integer_to_list(Info),32,http_util:to_upper(atom_to_list(Type))|Acc]);
status_resp([],Acc) -> "(" ++ string:strip(lists:flatten(lists:reverse(Acc))) ++ ")".


status_flags(String) ->
	Tokens = string:tokens(String," ()"),
	lists:map(fun(S) -> list_to_atom(http_util:to_lower(S)) end,Tokens).


status_info(MailBoxInfo,[H|_] = List) when is_integer(H) -> status_info(MailBoxInfo,status_flags(List));
status_info(MailBoxInfo,[H|_] = List) when is_atom(H) -> status_info(MailBoxInfo,List,[]).

status_info(#mailbox{messages = Messages} = MailBoxInfo,[messages|T],Acc) ->
	status_info(MailBoxInfo,T,[{messages, Messages}|Acc]);
status_info(#mailbox{recent = Recent} = MailBoxInfo,[recent|T],Acc) ->
	status_info(MailBoxInfo,T,[{recent, Recent}|Acc]);
status_info(#mailbox{uidnext = UIDNext} = MailBoxInfo,[uidnext|T],Acc) ->
	status_info(MailBoxInfo,T,[{uidnext, UIDNext}|Acc]);
status_info(#mailbox{uidvalidity = UIDValidity} = MailBoxInfo,[uidvalidity|T],Acc) ->
	status_info(MailBoxInfo,T,[{uidvalidity, UIDValidity}|Acc]);
status_info(#mailbox{unseen = UnSeen} = MailBoxInfo,[unseen|T],Acc) ->
	status_info(MailBoxInfo,T,[{unseen, UnSeen}|Acc]);
status_info(_MailBoxInfo,[],Acc) -> lists:reverse(Acc).

	





mailbox_info(MailBoxStore) -> mailbox_info(MailBoxStore,all).

mailbox_info(MailBoxStore,Args) when is_record(MailBoxStore,mailbox_store) -> 
	{MailBoxName,UserName,DomainName} = MailBoxStore#mailbox_store.name,
	mailbox_info(#mailbox{name = MailBoxName},{UserName,DomainName},Args);
mailbox_info(MailBox,{UserName,DomainName}) -> mailbox_info(MailBox,{UserName,DomainName},all).


mailbox_info(MailBox,{UserName,DomainName},all) -> mailbox_info(MailBox,{UserName,DomainName},[exists,messages,unseen,recent,flags,permanentflags]);

mailbox_info(MailBox,{UserName,DomainName},[exists|T]) ->
	Store = erlmail_conf:lookup_atom(store_type_message),
	case Store:select({MailBox#mailbox.name,{UserName,DomainName}}) of
		[] -> mailbox_info(MailBox,{UserName,DomainName},T);
		MailBoxStore -> mailbox_info(MailBox#mailbox{exists=length(MailBoxStore#mailbox_store.messages)},{UserName,DomainName},T)
	end;
mailbox_info(MailBox,{UserName,DomainName},[messages|T]) ->
	Store = erlmail_conf:lookup_atom(store_type_message),
	case Store:select({MailBox#mailbox.name,{UserName,DomainName}}) of
		[] -> mailbox_info(MailBox,{UserName,DomainName},T);
		MailBoxStore -> mailbox_info(MailBox#mailbox{messages=length(MailBoxStore#mailbox_store.messages)},{UserName,DomainName},T)
	end;
mailbox_info(MailBox,{UserName,DomainName},[unseen|T]) ->
	Store = erlmail_conf:lookup_atom(store_type_message),
	case Store:unseen({MailBox#mailbox.name,UserName,DomainName}) of
		{_Seen,Unseen} ->  mailbox_info(MailBox#mailbox{unseen=length(Unseen)},{UserName,DomainName},T);
		_ -> mailbox_info(MailBox,{UserName,DomainName},T)
	end;
mailbox_info(MailBox,{UserName,DomainName},[recent|T]) ->
	Store = erlmail_conf:lookup_atom(store_type_message),
	case Store:recent({MailBox#mailbox.name,UserName,DomainName}) of
		Recent when is_list(Recent) ->  mailbox_info(MailBox#mailbox{recent=length(Recent)},{UserName,DomainName},T);
		_ -> mailbox_info(MailBox,{UserName,DomainName},T)
	end;
mailbox_info(MailBox,{UserName,DomainName},[flags|T]) ->
	mailbox_info(MailBox#mailbox{flags=[answered,flagged,draft,deleted,seen]},{UserName,DomainName},T);
% TODO: UIDNEXT
% TODO: UIDVALIDITY


mailbox_info(MailBox,{UserName,DomainName},[_H|T]) ->
	?D(_H),
	mailbox_info(MailBox,{UserName,DomainName},T);

mailbox_info(MailBox,{_UserName,_DomainName},[]) -> MailBox.











greeting_capability(State) when is_record(State,imapd_fsm) -> greeting_capability(State#imapd_fsm.options);
greeting_capability(Options) ->
	case erlmail_conf:lookup_atom(server_imap_greeting_capability,Options) of
		true -> true;
		_ -> false
	end.

greeting(State) when is_record(State,imapd_fsm) -> greeting(State#imapd_fsm.options);
greeting(Options) ->
	case erlmail_conf:lookup(server_imap_greeting,Options) of
		[] -> "ErlMail IMAP4 Server ready";
		Greeting -> Greeting
	end.




inbox(MailBox) ->
	case list_to_atom(http_util:to_lower(MailBox)) of
		inbox -> "INBOX";
		_ -> MailBox
	end.

heirachy_char() ->
	case erlmail_conf:lookup(server_imap_hierarchy) of
		[] -> "/";
		Heirarchy -> Heirarchy
	end.



clean({UserName,Password}) ->
	{clean(UserName),clean(Password)};
clean(String) ->
	S = string:strip(String,both,32),
	S2 = string:strip(S,both,34),
	string:strip(S2,both,32).


seq_to_list([I|_] = Seq) when is_integer(I) -> seq_to_list(string:tokens(Seq,","));
seq_to_list(Seq) -> seq_to_list(Seq,[]).

seq_to_list([],Acc) -> lists:flatten(lists:reverse(Acc));
seq_to_list([H|T],Acc) ->
	case catch list_to_integer(H) of
		Int when is_integer(Int) -> seq_to_list(T,[Int|Acc]);
		_ ->
			[S,E] = string:tokens(H,":"),
			Start = list_to_integer(S),
			End = list_to_integer(E),
			seq_to_list(T,[lists:seq(Start,End)|Acc])
	end.

list_to_seq(List) -> list_to_seq(List,0,[]).
list_to_seq([],_,Acc) -> lists:flatten(lists:reverse(Acc));


list_to_seq([H],Start,Acc) when is_integer(H), Start > 0 ->
	String = integer_to_list(Start) ++ ":" ++ integer_to_list(H),
	list_to_seq([],0,[String|Acc]);

list_to_seq([H],_,Acc) when is_integer(H) ->
	list_to_seq([],0,[integer_to_list(H)|Acc]);
list_to_seq([H|[I|_] = T],Start,Acc) when H == I - 1, Start == 0 ->
	list_to_seq(T,H,Acc);

list_to_seq([H|[I] = _T],Start,Acc) when H == I - 1, is_integer(I) ->
	?D({Start,H,I}),
	String = integer_to_list(Start) ++ ":" ++ integer_to_list(I),
	list_to_seq([],Start,[String|Acc]);

list_to_seq([H|[I|_J] = T],Start,Acc) when H == I - 1 ->
	list_to_seq(T,Start,Acc);

list_to_seq([H|[I|_J] = T],Start,Acc) when H /= I - 1, Start > 0 ->
	String = integer_to_list(Start) ++ ":" ++ integer_to_list(H),
	list_to_seq(T,0,[44,String|Acc]);


list_to_seq([H|[_I|_] = T],_Start,Acc) ->
	list_to_seq(T,0,[44,integer_to_list(H)|Acc]).








send(Resp,State) when is_record(Resp,imap_resp)  -> send(response(Resp),State);
send(Msg,State)  when is_record(State,imapd_fsm) -> send(Msg,State#imapd_fsm.socket);
send(Message,Socket) ->
%	?D(Message),
	Msg = case string:right(Message,2) of
		?CRLF -> [Message];
		_      -> [Message,?CRLF]
	end,
%	?D(Msg),
	gen_tcp:send(Socket,Msg).

out(Command,State) -> io:format("~p ~p~n",[State#imapd_fsm.addr,Command]).
out(Command,Param,State) -> io:format("~p ~p ~p~n",[State#imapd_fsm.addr,Command,Param]).