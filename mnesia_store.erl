%%%---------------------------------------------------------------------------------------
%%% File        : mnesia_store
%%% Author      : Stuart Jackson <sjackson@simpleenigma.com> [http://www.simpleenigma.com]
%%% Purpose     : MNESIA email store
%%% Created     : 2007-10-20
%%% Initial Rel : 0.0.6
%%% Updated     : 2007-10-18
%%%---------------------------------------------------------------------------------------
-module(mnesia_store).
-author('sjackson@simpleenigma.com').
-include("erlmail.hrl").
-behavior(gen_store).

-export([create/1,drop/1,select/2,insert/1,delete/1,update/1,message_name/1]).
-export([info/1,config/0,list/0,list/1]).
-export([deliver/1,ensure_inbox/1,check/1]).
-export([unseen/1,recent/1]).

-export([select/1,info/2,filename/1]).
-export([init/1,create/2]).





create(Type) -> create(Type,[node()]).
create(Type,NodeList) when Type =:= domain; Type =:= user; Type =:= message; Type =:= mailbox_store ->
	{TableName,TableDef} = case Type of
		domain  -> {erlmail_conf:lookup_atom(mnesia_table_domain), [{disc_copies,NodeList},{type,set},{record_name,domain}, {attributes,record_info(fields,domain)}]};
		user    -> {erlmail_conf:lookup_atom(mnesia_table_user),   [{disc_copies,NodeList},{type,set},{record_name,user},   {attributes,record_info(fields,user)}]};
		message -> {erlmail_conf:lookup_atom(mnesia_table_message),[{disc_copies,NodeList},{type,set},{record_name,message},{attributes,record_info(fields,message)}]};
		mailbox_store -> {erlmail_conf:lookup_atom(mnesia_table_mailbox_store),
			[{disc_copies,NodeList},{type,set},{record_name,mailbox_store},{attributes,record_info(fields,mailbox_store)}]}
	end,
	case mnesia:create_table(TableName,TableDef) of
		{atomic,ok} -> ok;
		{aborted,Reason} -> {error,Reason}
	end.
	


delete(Domain)  when is_record(Domain,domain)         -> delete(Domain#domain.name,erlmail_conf:lookup_atom(mnesia_table_domain));
delete(User)    when is_record(User,user)             -> delete(User#user.name,erlmail_conf:lookup_atom(mnesia_table_user));
delete(MailBox) when is_record(MailBox,mailbox_store) -> delete(MailBox#mailbox_store.name,erlmail_conf:lookup_atom(mnesia_table_mailbox_store));
delete(Message) when is_record(Message,message)       -> delete(Message#message.name,erlmail_conf:lookup_atom(mnesia_table_message)).
delete(Name,TableName) ->
	F = fun() ->
		mnesia:delete({TableName,Name})
		end,
	case mnesia:sync_transaction(F) of
		{atomic,ok} -> ok;
		{aborted,Reason} -> {error,Reason}
	end.



drop(Type) when Type =:= domain; Type =:= user; Type =:= message; Type =:= mailbox_store -> 
	TableName = erlmail_conf:lookup_atom(list_to_atom("mnesia_table_" ++ atom_to_list(Type))),
	case mnesia:delete_table(TableName) of
		{atomic,ok} -> ok;
		{aborted,Reason} -> {error,Reason}
	end.

insert(Domain)  when is_record(Domain,domain)         -> insert(Domain,erlmail_conf:lookup_atom(mnesia_table_domain));
insert(User)    when is_record(User,user)             -> insert(User,erlmail_conf:lookup_atom(mnesia_table_user));
insert(MailBox) when is_record(MailBox,mailbox_store) -> insert(MailBox,erlmail_conf:lookup_atom(mnesia_table_mailbox_store));
insert(Message) when is_record(Message,message)       -> insert(Message,erlmail_conf:lookup_atom(mnesia_table_message)).
insert(Record,TableName) -> 
	F = fun() ->
		mnesia:write(TableName,Record,write)
		end,
	case mnesia:sync_transaction(F) of
		{atomic,ok} -> ok;
		{aborted,Reason} -> {error,Reason}
	end.



message_name(_Args) -> gen_store:message_name(_Args).
update(Record) -> insert(Record).




list() ->
	MatchHead = #domain{name = '$1', _ = '_'},
	Guard = [],
	Result = '$1',
	MatchSpec = [{MatchHead, Guard, [Result]}],
	TableName = erlmail_conf:lookup_atom(mnesia_table_domain),
	F = fun() ->
		mnesia:select(TableName,MatchSpec)
		end,
	case mnesia:sync_transaction(F) of
		{atomic,List} -> List;
		{aborted,Reason} -> {error,Reason}
	end.
list(domain)  -> list();
list(domains) -> list();
list(DomainName) when is_list(DomainName) -> 
	MatchHead = #user{name = {'$1',DomainName}, _ = '_'},
	Guard = [],
	Result = '$1',
	MatchSpec = [{MatchHead, Guard, [Result]}],
	TableName = erlmail_conf:lookup_atom(mnesia_table_user),
	F = fun() ->
		mnesia:select(TableName,MatchSpec)
		end,
	case mnesia:sync_transaction(F) of
		{atomic,List} -> List;
		{aborted,Reason} -> {error,Reason}
	end;
list({UserName,DomainName}) -> 
	MatchHead = #message{name = {'$1',UserName,DomainName}, _ = '_'},
	Guard = [],
	Result = '$1',
	MatchSpec = [{MatchHead, Guard, [Result]}],
	TableName = erlmail_conf:lookup_atom(mnesia_table_message),
	F = fun() ->
		mnesia:select(TableName,MatchSpec)
		end,
	case mnesia:sync_transaction(F) of
		{atomic,List} -> List;
		{aborted,Reason} -> {error,Reason}
	end.



info(domain)  -> info(erlmail_conf:lookup_atom(mnesia_table_domain));
info(user)    -> info(erlmail_conf:lookup_atom(mnesia_table_user));
info(message) -> info(erlmail_conf:lookup_atom(mnesia_table_message));
info(TableName) when is_atom(TableName) ->
	F = fun() ->
		mnesia:table_info(TableName,all)
		end,
	case mnesia:sync_transaction(F) of
		{atomic,Info} -> Info;
		{aborted,Reason} -> {error,Reason}
	end.

info(Type,Key) ->
	Info = info(Type),
	case lists:keysearch(Key,1,Info) of
		{value,{Key,Value}} -> Value;
		_ -> []
	end.

select(Domain)  when is_list(Domain)                       -> select(erlmail_conf:lookup_atom(mnesia_table_domain),Domain);
select({MailBoxName,{UserName,DomainName}})                -> select(erlmail_conf:lookup_atom(mnesia_table_mailbox_store),{MailBoxName,UserName,DomainName});
select(User)    when is_tuple(User), size(User) == 2       -> select(erlmail_conf:lookup_atom(mnesia_table_user),User);
select(Message) when is_tuple(Message), size(Message) == 3 -> select(erlmail_conf:lookup_atom(mnesia_table_message),Message).

select(TableName,Key) ->
	F = fun() ->
		mnesia:read({TableName,Key})
		end,
	case mnesia:sync_transaction(F) of
		{atomic,[]} -> [];
		{atomic,[Record]} -> Record;
		{atomic,RecordList} when is_list(RecordList) -> RecordList;
		{aborted,Reason} -> {error,Reason}
	end.



deliver(Message) when is_record(Message,message) -> 
	{MessageName,UserName,DomainName} = Message#message.name,
	MailBoxTableName = erlmail_conf:lookup_atom(mnesia_table_mailbox_store),
	MessageTableName = erlmail_conf:lookup_atom(mnesia_table_message),
	ensure_inbox({UserName,DomainName}),
	F = fun() ->
		[MailBox] = mnesia:read({MailBoxTableName,{"INBOX",UserName,DomainName}}),
		NewMailBox = MailBox#mailbox_store{messages = [MessageName|MailBox#mailbox_store.messages]},
		UID = MailBox#mailbox_store.uidnext,
		mnesia:write(MailBoxTableName,NewMailBox#mailbox_store{uidnext = UID + 1},write),
		mnesia:write(MessageTableName,Message#message{uid = UID},write)	
	end,
	case mnesia:sync_transaction(F) of
		{atomic,ok} -> ok;
		{aborted,Reason} -> {error,Reason}
	end.


ensure_inbox(User) when is_record(User,user) -> ensure_inbox(User#user.name);
ensure_inbox({UserName,DomainName}) ->
	TableName = erlmail_conf:lookup_atom(mnesia_table_mailbox_store),
	F = fun() ->
		case mnesia:read({TableName,{"INBOX",UserName,DomainName}}) of
			[] -> 
				insert(#mailbox_store{name = {"INBOX",UserName,DomainName}, subscribed = true }),
				created;
			_ -> exists
		end
		
	end,
	case mnesia:sync_transaction(F) of
		{atomic,created} -> {ok,cretated};
		{atomic,exists} -> {ok,exists};
		{aborted,Reason} -> {error,Reason}
	end.

unseen({MailBoxName,UserName,DomainName}) -> 
	MailBoxTableName = erlmail_conf:lookup_atom(mnesia_table_mailbox_store),
	MessageTableName = erlmail_conf:lookup_atom(mnesia_table_message),
	F = fun() ->
		[MailBox] = mnesia:read({MailBoxTableName,{MailBoxName,UserName,DomainName}}),
		lists:foldl(fun(MessageName,{S,U}) -> 
			[Message] = mnesia:read({MessageTableName,{MessageName,UserName,DomainName}}),
			case lists:member(seen,Message#message.flags) of
				true -> {[MessageName|S],U};
				false -> {S,[MessageName|U]}
			end
		end,{[],[]},MailBox#mailbox_store.messages)
	end,
	case mnesia:sync_transaction(F) of
		{atomic,Results} -> Results;
		{aborted,Reason} -> {error,Reason}
	end.

recent({MailBoxName,UserName,DomainName}) -> 
	MailBoxTableName = erlmail_conf:lookup_atom(mnesia_table_mailbox_store),
	MessageTableName = erlmail_conf:lookup_atom(mnesia_table_message),
	F = fun() ->
		[MailBox] = mnesia:read({MailBoxTableName,{MailBoxName,UserName,DomainName}}),
		lists:foldl(fun(MessageName,Recent) -> 
			[Message] = mnesia:read({MessageTableName,{MessageName,UserName,DomainName}}),
			case lists:member(recent,Message#message.flags) of
				true -> [MessageName|Recent];
				false -> Recent
			end
		end,[],MailBox#mailbox_store.messages)
	end,
	case mnesia:sync_transaction(F) of
		{atomic,Results} -> Results;
		{aborted,Reason} -> {error,Reason}
	end.



config() -> undefined.
filename(_) -> undefined.
init(NodeList) -> mnesia:create_schema(NodeList).
check(_Type) -> undefined.