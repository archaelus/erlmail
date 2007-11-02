%%%---------------------------------------------------------------------------------------
%%% File        : mysql_store
%%% Author      : Stuart Jackson <sjackson@simpleenigma.com> [http://www.simpleenigma.com]
%%% Purpose     : MNESIA email store
%%% Created     : 2007-10-20
%%% Initial Rel : 0.0.6
%%% Updated     : 2007-10-18
%%%---------------------------------------------------------------------------------------
-module(mysql_store).
-author('sjackson@simpleenigma.com').
-include("erlmail.hrl").
-behavior(gen_store).

-export([create/1,drop/1,select/1,select/2,insert/1,delete/1,update/1,message_name/1]).
-export([list/0,list/1,check/1,unseen/1,recent/1,mlist/3]).
-export([deliver/1,ensure_inbox/1]).

%%% Create all SQL statements in macros for easy replacement
-define(MYSQL_STORE_SQL_SELECT_USER,"").
%%%


%%-------------------------------------------------------------------------
%% @spec (Type::store_type()) -> ok | undefined | {error,string()}
%% @type store_type() = domain | user | message | mailbox_store
%% @doc Performs check command on store of Type. Commits and cleans up any
%%      actions that are not writen to disk on a regualr basis
%% @end
%%-------------------------------------------------------------------------
check(_Type) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Type::store_type()) -> ok | undefined | {error,string()}
%% @doc Creates store for Type.
%% @end
%%-------------------------------------------------------------------------
create(_) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Type::store_type()) -> ok | undefined | {error,string()}
%% @doc Deletes entry in store of Type.
%% @end
%%-------------------------------------------------------------------------
delete(_) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Message::tuple()) -> ok | undefined | {error,string()}
%% @doc Takes a #message{} record and deilvers it to a users INBOX
%% @end
%%-------------------------------------------------------------------------
deliver(Message) when is_record(Message,message) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Type::store_type()) -> ok | undefined | {error,string()}
%% @doc  Creates store for Type.
%% @end
%%-------------------------------------------------------------------------
drop(_) -> undefined.

%%-------------------------------------------------------------------------
%% @spec ({UserName::string(),DomainName::string()}) -> {ok,create} | {ok,exists} | undefined | {error,string()}
%% @doc  Makes sure that the specified user has an INBOX. Used in deliver command.
%% @end
%%-------------------------------------------------------------------------
ensure_inbox(User) when is_record(User,user) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Record::tuple()) -> ok | undefined | {error,string()}
%% @doc  Inserts Record into correct store. Records can be of types:
%%       #domain{}, #user{}, #message{} or #mailbox_store{}
%% @end
%%-------------------------------------------------------------------------
insert(_) -> undefined.

%%-------------------------------------------------------------------------
%% @spec () -> ok | undefined | {error,string()}
%% @doc  Lists all domains.
%% @end
%%-------------------------------------------------------------------------
list() -> undefined.
%%-------------------------------------------------------------------------
%% @spec (Type::any()) -> ok | undefined | {error,string()}
%% @doc  Lists all domains, users, messges or mailboxes depnding on 
%%       term that is given
%% @end
%%-------------------------------------------------------------------------
list(_) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Args::any()) -> string() | undefined | {error,string()}
%% @doc  Generates message name. May use gen_store:message_name/1 or
%%       be specific to the message store.
%% @end
%%-------------------------------------------------------------------------
message_name(Args) -> gen_store:message_name(Args).

%%-------------------------------------------------------------------------
%% @spec (MailBoxName::string(),{UserName::string(),DomainName::string()},Subscribed::bool()) -> list()
%% @doc Lists mailbox information for LIST and LSUB commands
%% @end
%%-------------------------------------------------------------------------
mlist(_MailBoxName,{_UserName,_DomainName},_Subscribed) -> undefined.

%%-------------------------------------------------------------------------
%% @spec ({MailBoxName::string(),UserName::string(),DomainName::string()}) -> list() | {error,Reason}
%% @doc Generates a list of message names that have the \Recent flag set
%% @end
%%-------------------------------------------------------------------------
recent({_MailBoxName,_UserName,_DomainName}) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Name::any()) -> ok | undefined | {error,string()}
%% @doc  Retrives Record from the correct store.
%% @end
%%-------------------------------------------------------------------------
select(_) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Record::tuple(),TableName::list()) -> ok | undefined | {error,string()}
%% @doc  MySQL Store internal SELECT implimentation
%% @hidden
%% @end
%%-------------------------------------------------------------------------
select(_,_) -> undefined.

%%-------------------------------------------------------------------------
%% @spec ({MailBoxName::string(),UserName::string(),DomainName::string()}) -> list() | {error,Reason}
%% @doc Generates a list of message names that have the \Unseen flag set
%% @end
%%-------------------------------------------------------------------------
unseen({_MailBoxName,_UserName,_DomainName}) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Record::tuple()) -> ok | undefined | {error,string()}
%% @doc  Update Record into correct store. Records can be of types:
%%       #domain{}, #user{}, #message{} or #mailbox_store{}
%% @end
%%-------------------------------------------------------------------------
update(_Record) -> undefined.
