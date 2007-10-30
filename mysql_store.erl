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

%%% Create all SQL statements in macros for easy replacement
-define(MYSQL_STORE_SQL_SELECT_USER,"").
%%%

create(_) -> undefined.
delete(_) -> undefined.
drop(_) -> undefined.
message_name(_) -> undefined.
select(_,_) -> undefined.
update(_) -> undefined.
config() -> undefined.
info(_) -> undefined.
list() -> undefined.
list(_) -> undefined.
insert(_) -> undefined.
deliver(Message) when is_record(Message,message) -> undefined.
ensure_inbox(User) when is_record(User,user) -> undefined.
filename(_) -> undefined.
info(_,_) -> undefined.
select(_) -> undefined.
unseen({_MailBoxName,_UserName,_DomainName}) -> undefined.
recent({_MailBoxName,_UserName,_DomainName}) -> undefined.
check(_Type) -> undefined.