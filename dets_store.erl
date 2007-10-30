%%%---------------------------------------------------------------------------------------
%%% File        : dets_store
%%% Author      : Stuart Jackson <sjackson@simpleenigma.com> [http://www.simpleenigma.com]
%%% Purpose     : DETS email store
%%% Created     : 2007-10-18
%%% Initial Rel : 0.0.5
%%% Updated     : 2007-10-18
%%%---------------------------------------------------------------------------------------
-module(dets_store).
-author('sjackson@simpleenigma.com').
-include("erlmail.hrl").
-behavior(gen_store).

-export([create/1,drop/1,select/2,insert/1,delete/1,delete/2,update/1,message_name/1]).
-export([info/1,config/0,list/0,list/1]).
-export([deliver/1,ensure_inbox/1,check/1]).
-export([unseen/1,recent/1]).

-export([select/1,info/2,filename/1]).


config() -> undefined.


create(Type) when Type =:= domain; Type =:= user; Type =:= message ->
	case dets:open_file(Type,[{access,read_write},{file,filename(Type)},{type,set},{keypos,2}]) of 
		{ok,Name} -> 
			dets:close(Name),
			ok;
		{error,Reason} -> {error,Reason}
	end.
drop(Type) when Type =:= domain; Type =:= user; Type =:= message -> 
	FileName = filename(Type),
	case filelib:is_regular(FileName) of
		true ->
			case dets:is_dets_file(FileName) of
				true ->
					file:delete(FileName),
					{ok,FileName};
				false -> {error,not_dets_file}
			end;
		false -> {error,file_not_found}
	end.

delete(_) -> undefined.
delete(Type,Key) when Type =:= domain; Type =:= user; Type =:= message -> 
	case dets:open_file(filename(Type)) of 
		{ok,Name} -> 
			dets:delete(Name,Key),
			dets:close(Name);
		{error,Reason} -> {error,Reason}
	end.

select(Domain)  when is_list(Domain)                       -> select(domain,Domain);
select(User)    when is_tuple(User), size(User) == 2       -> select(user,User);
select(Message) when is_tuple(Message), size(Message) == 3 -> select(message,Message).

select(Type,Key) when Type =:= domain; Type =:= user; Type =:= message ->
	case dets:open_file(filename(Type)) of 
		{ok,Name} -> 
			case dets:lookup(Name,Key) of
				[Record] when is_record(Record,domain); is_record(Record,user); is_record(Record,message) -> 
					dets:close(Name),
					Record;
				_ -> 
					dets:close(Name),
					[]
			end;
		{error,Reason} -> {error,Reason}
	end.

insert(Domain)  when is_record(Domain,domain)   -> insert(Domain,domain);
insert(User)    when is_record(User,user)       -> insert(User,user);
insert(MailBox)    when is_record(MailBox,mailbox_store)       -> insert(MailBox,mailbox_store);
insert(Message) when is_record(Message,message) -> insert(Message,message).
insert(Record,Type) when Type =:= domain; Type =:= user; Type =:= message -> 
	case dets:open_file(filename(Type)) of 
		{ok,Name} -> 
			case dets:insert(Name,Record) of
				ok -> dets:close(Name);
				{error,Reason} -> 
					dets:close(Name),
					{error,Reason}
			end;
		{error,Reason} -> {error,Reason}
	end.

update(Record) -> insert(Record).

message_name(_Args) -> gen_store:message_name(_Args).


info(Type) when Type =:= domain; Type =:= user; Type =:= message ->
	case dets:open_file(filename(Type)) of 
		{ok,Name} -> 
			Info = dets:info(Name),
			dets:close(Name),
			Info;
		{error,Reason} -> {error,Reason}
	end.

info(Type,Key) ->
	Info = info(Type),
	case lists:keysearch(Key,1,Info) of
		{value,{Key,Value}} -> Value;
		_ -> []
	end.
	

list() ->
	case dets:open_file(filename(domain)) of 
		{ok,Name} -> 
			case dets:match(Name,{domain,'$1','_'}) of
				[] -> 
					dets:close(Name),
					[];
				Match ->
					dets:close(Name),
					lists:map(fun([DomainName]) -> 
						DomainName
						end,Match)
			end;
		{error,Reason} -> {error,Reason}
	end.

list(domain)  -> list();
list(domains) -> list();
list(DomainName) when is_list(DomainName) ->
	case dets:open_file(filename(user)) of 
		{ok,Name} -> 
			case dets:match(Name,{user,{'$1',DomainName},'_'}) of
				[] -> 
					dets:close(Name),
					[];
				Match ->
					dets:close(Name),
					lists:map(fun([UserName]) -> 
						UserName
						end,Match)
			end;
		{error,Reason} -> {error,Reason}
	end;
list({UserName,DomainName}) ->
	case dets:open_file(filename(message)) of 
		{ok,Name} -> 
			case dets:match(Name,{message,{'$1',UserName,DomainName},'_','_'}) of
				[] -> 
					dets:close(Name),
					[];
				Match ->
					dets:close(Name),
					lists:map(fun([MessageName]) -> 
						MessageName
						end,Match)
			end;
		{error,Reason} -> {error,Reason}
	end.


deliver(Message) when is_record(Message,message) -> undefined.
ensure_inbox(User) when is_record(User,user) -> undefined.
unseen({_MailBoxName,_UserName,_DomainName}) -> undefined.
recent({_MailBoxName,_UserName,_DomainName}) -> undefined.
check(_Type) -> undefined.










filename(Type) when is_atom(Type) and ((Type =:= domain) or (Type =:= user) or (Type =:= message)) -> filename(atom_to_list(Type));
filename(FileName) -> erlmail_conf:lookup(dets_default_path) ++ FileName.
