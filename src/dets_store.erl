%%%---------------------------------------------------------------------------------------
%%% @author     Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc        DETS email store
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version    0.0.6
%%% @since      0.0.5
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
-module(dets_store).
-author('sjackson@simpleenigma.com').
-include("../include/erlmail.hrl").
-behavior(gen_store).

-export([create/1,drop/1,select/2,insert/1,delete/1,delete/2,update/1,message_name/1]).
-export([info/1,list/0,list/1]).
-export([deliver/1,ensure_inbox/1,check/1]).
-export([unseen/1,recent/1,mlist/3]).

-export([select/1,info/2,filename/1]).

%%-------------------------------------------------------------------------
%% @spec (Type::store_type()) -> ok | undefined | {error,string()}
%% @type store_type() = domain | user | message | mailbox_store
%% @doc Performs check command on store of Type. Commits and cleans up any
%%      actions that are not writen to disk on a regualar basis
%% @end
%%-------------------------------------------------------------------------
check(_Type) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Type::store_type()) -> ok | undefined | {error,string()}
%% @doc Creates store for Type.
%% @end
%%-------------------------------------------------------------------------
create(Type) when Type =:= domain; Type =:= user; Type =:= message ->
	case dets:open_file(Type,[{access,read_write},{file,filename(Type)},{type,set},{keypos,2}]) of 
		{ok,Name} -> 
			dets:close(Name),
			ok;
		{error,Reason} -> {error,Reason}
	end.

%%-------------------------------------------------------------------------
%% @spec (Type::store_type()) -> ok | undefined | {error,string()}
%% @doc Deletes entry in store of Type.
%% @end
%%-------------------------------------------------------------------------
delete(_) -> undefined.
delete(Type,Key) when Type =:= domain; Type =:= user; Type =:= message -> 
	case dets:open_file(filename(Type)) of 
		{ok,Name} -> 
			dets:delete(Name,Key),
			dets:close(Name);
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


mlist(_MailBoxName,{_UserName,_DomainName},_Subscribed) -> undefined.










filename(Type) when is_atom(Type) and ((Type =:= domain) or (Type =:= user) or (Type =:= message)) -> filename(atom_to_list(Type));
filename(FileName) -> erlmail_util:get_app_env(dets_default_path,"/tmp/") ++ FileName.
