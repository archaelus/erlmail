%%%----------------------------------------------------------------------
%%% File        : imapc
%%% Author      : Stuart Jackson <sjackson@simpleenigma.com> [http://www.simpleenigma.com]
%%% Purpose     : IMAP Client API - Use these funcations instead of the FSM
%%% Created     : 2006-11-04
%%% Initial Rel : 0.0.1
%%% Updated     : 2007-10-18
%%%----------------------------------------------------------------------
-module(imapc).
-author('sjackson@simpleenigma.com').
-include("erlmail.hrl").
-include("imap.hrl").

-export([connect/1,connect/2]).
-export([login/3,logout/1,noop/1,authenticate/2,capability/1]).
-export([select/1,select/2,close/1,examine/1,examine/2]).
-export([create/2,delete/2,subscribe/2,unsubscribe/2,rename/3,list/3,lsub/3]).
-export([append/3,append/4,check/1,store/4,uid/3]).
-export([switch/2,copy/3,move/3,expunge/1,status/3,status/2,search/2,fetch/3,sort/3,sort/4]).
-export([deletef/2,undeletef/2,draft/2,undraft/2,flag/2,unflag/2,seen/2,unseen/2]).
-export([info/1,info/2]).
-export([build/0,build/1]).

%%--------------------------------------------------------------------
%% Function: connect(IPAddress)
%%         : connect(IPAddress,Port)
%%           IPAddress = term() tuple, i.e {10,1,1,3}
%%           Port      = integer(), Default 143  
%% Descrip.: Connects to an IMAP server and starts a FSM
%% Returns : {ok, Pid} | {error, Error}
%%--------------------------------------------------------------------

connect(IPAddress) -> connect(IPAddress,143).
connect(IPAddress,Port) -> imapc_fsm:start_link(IPAddress,Port).

%%--------------------------------------------------------------------
%% Function: noop(Pid)
%%           Pid = pid()
%% Descrip.: Sends NOOP command to FSM: No Operation
%% Returns : ok
%%--------------------------------------------------------------------

noop(Pid)     -> noop(Pid,imapc_util:tag()).
noop(Pid,Tag) -> gen_fsm:sync_send_event(Pid,{noop,Tag}).

%%--------------------------------------------------------------------
%% Function: capability(Pid)
%%           Pid = pid()
%% Descrip.: Sends CAPABILITY command to FSM: No Operation
%% Returns : List of Capabilities
%%--------------------------------------------------------------------

capability(Pid)     -> capability(Pid,imapc_util:tag()).
capability(Pid,Tag) -> gen_fsm:sync_send_event(Pid,{capability,Tag}).

%%--------------------------------------------------------------------
%% Function: logout(Pid)
%%           Pid = pid()
%% Descrip.: Sends logout command to FSM, closes connection
%% Returns : ok
%%--------------------------------------------------------------------

logout(Pid)     -> logout(Pid,imapc_util:tag()).
logout(Pid,Tag) -> gen_fsm:sync_send_event(Pid,{logout,Tag}).


%%--------------------------------------------------------------------
%% Function: login(Pid,UserName,Password)
%%           Pid      = pid()
%%           UserName = string()
%%           Password = string()
%% Descrip.: Sends login command to FSM, changes from not_authenticated to authtneicated state
%% Returns : ok
%%--------------------------------------------------------------------

login(Pid,UserName,Password)     -> login(Pid,UserName,Password,imapc_util:tag()).
login(Pid,UserName,Password,Tag) -> gen_fsm:sync_send_event(Pid,{login,Tag,UserName,Password}).

%%--------------------------------------------------------------------
%% Function: authenticate(Pid,Method)
%%           Pid    = pid()
%%           Method = String() - Name of authentication method
%% Descrip.: Initiates authentification - NOT YET IMPLIMENTED
%% Returns : ok
%%--------------------------------------------------------------------

authenticate(Pid,Method)     -> authenticate(Pid,Method,imapc_util:tag()).
authenticate(Pid,Method,Tag) -> gen_fsm:sync_send_event(Pid,{authenticate,Tag,Method}).


%%--------------------------------------------------------------------
%% Function: select(Pid)
%%           select(Pid,MailBox)
%%           Pid     = pid()
%%           MailBox = string(), Default "INBOX"
%% Descrip.: Sends select command to select specified mailbox name
%% Returns : ok
%%--------------------------------------------------------------------

select(Pid) -> select(Pid,"INBOX").
select(Pid,MailBox) -> select(Pid,MailBox,imapc_util:tag()).
select(Pid,MailBox,Tag) -> gen_fsm:sync_send_event(Pid,{select,Tag,MailBox}).

%%--------------------------------------------------------------------
%% Function: examine(Pid)
%%           examine(Pid,MailBox)
%%           Pid     = pid()
%%           MailBox = string(), Default "INBOX"
%% Descrip.: Sends examine command to select (READ-ONLY) specified mailbox name
%% Returns : ok
%%--------------------------------------------------------------------

examine(Pid) -> examine(Pid,"INBOX").
examine(Pid,MailBox) -> examine(Pid,MailBox,imapc_util:tag()).
examine(Pid,MailBox,Tag) -> gen_fsm:sync_send_event(Pid,{examine,Tag,MailBox}).

%%--------------------------------------------------------------------
%% Function: create(Pid,Mailbox)
%%           Pid     = pid()
%%           Mailbox = string()
%% Descrip.: Creates a mailbox in the authenticated state
%% Returns : {ok, Pid} | {error, Error}
%%--------------------------------------------------------------------

create(Pid,MailBox) -> create(Pid,MailBox,imapc_util:tag()).
create(Pid,MailBox,Tag) -> gen_fsm:sync_send_event(Pid,{create,Tag,MailBox}).

%%--------------------------------------------------------------------
%% Function: delete(Pid,Mailbox)
%%           Pid     = pid()
%%           Mailbox = string()
%% Descrip.: Deletes a mailbox in the authenticated state
%% Returns : {ok, Pid} | {error, Error}
%%--------------------------------------------------------------------

delete(Pid,MailBox) -> delete(Pid,MailBox,imapc_util:tag()).
delete(Pid,MailBox,Tag) -> gen_fsm:sync_send_event(Pid,{delete,Tag,MailBox}).

%%--------------------------------------------------------------------
%% Function: rename(Pid,OrgMailBox,NewMailBox)
%%           Pid     = pid()
%%           MailBox = string()
%% Descrip.: RENAMEs first OrgMailBOx to second NewMailBox
%% Returns : ok
%%--------------------------------------------------------------------

rename(Pid,OrgMailBox,NewMailBox) -> rename(Pid,OrgMailBox,NewMailBox,imapc_util:tag()).
rename(Pid,OrgMailBox,NewMailBox,Tag) ->  gen_fsm:sync_send_event(Pid,{rename,Tag,OrgMailBox,NewMailBox}).

%%--------------------------------------------------------------------
%% Function: subscribe(Pid,Mailbox)
%%           Pid     = pid()
%%           Mailbox = string()
%% Descrip.: Subscribes to a mailbox in the authenticated state
%% Returns : {ok, Pid} | {error, Error}
%%--------------------------------------------------------------------

subscribe(Pid,MailBox) -> subscribe(Pid,MailBox,imapc_util:tag()).
subscribe(Pid,MailBox,Tag) -> gen_fsm:sync_send_event(Pid,{subscribe,Tag,MailBox}).

%%--------------------------------------------------------------------
%% Function: unsubscribe(Pid,Mailbox)
%%           Pid     = pid()
%%           Mailbox = string()
%% Descrip.: Unsubscribes to a mailbox in the authenticated state
%% Returns : {ok, Pid} | {error, Error}
%%--------------------------------------------------------------------

unsubscribe(Pid,MailBox) -> unsubscribe(Pid,MailBox,imapc_util:tag()).
unsubscribe(Pid,MailBox,Tag) -> gen_fsm:sync_send_event(Pid,{unsubscribe,Tag,MailBox}).

%%--------------------------------------------------------------------
%% Function: list(Pid,RefName,Mailbox)
%%           Pid     = pid()
%%           RefName = string()
%%           Mailbox = string()
%% Descrip.: lists mailboxes
%% Returns : {ok, Pid} | {error, Error}
%%--------------------------------------------------------------------

list(Pid,RefName,Mailbox) -> list(Pid,RefName,Mailbox,imapc_util:tag()).
list(Pid,RefName,Mailbox,Tag) -> gen_fsm:sync_send_event(Pid,{list,Tag,RefName,Mailbox}).

%%--------------------------------------------------------------------
%% Function: lsub(Pid,RefName,Mailbox)
%%           Pid     = pid()
%%           RefName = string()
%%           Mailbox = string()
%% Descrip.: Lists only subscribed mailboxes
%% Returns : {ok, Pid} | {error, Error}
%%--------------------------------------------------------------------

lsub(Pid,RefName,Mailbox) -> lsub(Pid,RefName,Mailbox,imapc_util:tag()).
lsub(Pid,RefName,Mailbox,Tag) -> gen_fsm:sync_send_event(Pid,{lsub,Tag,RefName,Mailbox}).

%%--------------------------------------------------------------------
%% Function: status(Pid,MailBox,StatusCodes)
%%           Pid         = pid()
%%           MailBox     = string()
%%           StatusCodes = term() - list of atoms [messages,recent,uidnext,uidvalidity,unseen]
%% Descrip.: Collects specified status codes
%% Returns : Status Information
%%--------------------------------------------------------------------

status(Pid,MailBox) -> status(Pid,MailBox,[messages,recent,uidnext,uidvalidity,unseen]).
status(Pid,MailBox,StatusCodes) -> status(Pid,MailBox,StatusCodes,imapc_util:tag()).
status(Pid,MailBox,StatusCodes,Tag) -> gen_fsm:sync_send_event(Pid,{status,Tag,MailBox,imapc_util:build_statuscodes(StatusCodes)}).

%%--------------------------------------------------------------------
%% Function: append(Pid,Mailbox,Message,Flags)
%%           Pid     = pid()
%%           Mailbox = String()
%%           Message = string()
%%           Flags   = term() - list of atoms
%% Descrip.: APPENDs message to Mailbox with listed Flags
%% Returns : ok
%%--------------------------------------------------------------------


append(Pid,Mailbox,Message) -> append(Pid,Mailbox,Message,imapc_util:tag()).
append(Pid,Mailbox,Message,[Atom|_Rest] = Flags) when is_atom(Atom) -> append(Pid,Mailbox,Message,Flags,imapc_util:tag());
append(Pid,Mailbox,Message,Tag) -> append(Pid,Mailbox,Message,[],Tag).
append(Pid,Mailbox,Message,Flags,Tag) ->
	gen_fsm:sync_send_event(Pid,{append,Tag,Mailbox,Message,imapc_util:build_flags(Flags)}).


%%--------------------------------------------------------------------
%% Function: check(Pid)
%%           Pid = pid()
%% Descrip.: Sends CHECK command to FSM
%% Returns : ok
%%--------------------------------------------------------------------

check(Pid)     -> check(Pid,imapc_util:tag()).
check(Pid,Tag) -> gen_fsm:sync_send_event(Pid,{check,Tag}).

%%--------------------------------------------------------------------
%% Function: close(Pid)
%%           Pid = pid()
%% Descrip.: Send CLOSE command, closes currently selected mailbox
%% Returns : ok
%%--------------------------------------------------------------------

close(Pid)     -> close(Pid,imapc_util:tag()).
close(Pid,Tag) -> gen_fsm:sync_send_event(Pid,{close,Tag}).

%%--------------------------------------------------------------------
%% Function: expunge(Pid)
%%           Pid     = pid()
%% Descrip.: perminately removes all messages with DELETED flag set
%% Returns : ok
%%--------------------------------------------------------------------

expunge(Pid)     -> expunge(Pid,imapc_util:tag()).
expunge(Pid,Tag) -> gen_fsm:sync_send_event(Pid,{expunge,Tag}).

%%--------------------------------------------------------------------
%% Function: search(Pid,Query)
%%           Pid   = pid()
%%           Query = Query Terms
%% Descrip.: Performs search query on selected mailbox
%% Returns : term(), list of integers
%%--------------------------------------------------------------------

search(Pid,Query) -> search(Pid,Query,imapc_util:tag()).
search(Pid,Query,Tag) -> gen_fsm:sync_send_event(Pid,{search,Tag,imapc_util:build_search(Query)}).



%%--------------------------------------------------------------------
%% Function: fetch(Pid,Set,Query)
%%           Pid   = pid()
%%           Set   = Integer or List of Integer
%%           Query = Fetch Query Terms
%% Descrip.: Performs search query on selected mailbox
%% Returns : term(), list of integers
%%--------------------------------------------------------------------

fetch(_Pid,[],_Query) -> [];
fetch(Pid,Set,Query) -> fetch(Pid,Set,Query,imapc_util:tag()).
fetch(Pid,Set,Query,Tag) -> 
	gen_fsm:sync_send_event(Pid,{fetch,Tag,imapc_util:to_seq(Set),imapc_util:build_fetch(Query)}).



%%--------------------------------------------------------------------
%% Function: store(Pid,Set,ItemName,Flags)
%%           Pid      = pid()
%%           Set      = Integer or List of Integer
%%           ItemName = Atom - one of [replace,add,remove,replace_silent,add_silent,remove_silent]
%%                      (delete is duplicate name for remove, including silent mode)
%%           Flags     = term() - List of atoms for flag names
%% Descrip.: Marks or unmarks all message in set with DELETED flag
%% Returns : ok
%%--------------------------------------------------------------------

store(_Pid,[],_ItemName,_Flags) -> [];
store(Pid,Set,ItemName,Flags) -> store(Pid,Set,ItemName,Flags,imapc_util:tag()).
store(Pid,Set,ItemName,Flags,Tag) -> 
	gen_fsm:sync_send_event(Pid,{store,Tag,imapc_util:to_seq(Set),imapc_util:build_store(ItemName),imapc_util:build_flags(Flags)}).

%%--------------------------------------------------------------------
%% Function: copy(Pid,MailBox,StatusCodes)
%%           Pid     = pid()
%%           Set     = list() of integer90
%%           MailBox = string()
%% Descrip.: Copies message set from selected mailbox to specified mailbox
%% Returns : ok
%%--------------------------------------------------------------------

copy(Pid,Set,MailBox) -> copy(Pid,Set,MailBox,imapc_util:tag()).
copy(Pid,Set,MailBox,Tag) -> gen_fsm:sync_send_event(Pid,{copy,Tag,imapc_util:to_seq(Set),MailBox}).


%%--------------------------------------------------------------------
%% Function: uid(Pid,Cmd,Opt)
%%           Pid      = pid()
%%           Set      = Integer or List of Integer
%%           ItemName = Atom - one of [replace,add,remove,replace_silent,add_silent,remove_silent]
%%                      (delete is duplicate name for remove, including silent mode)
%%           Flags     = term() - List of atoms for flag names
%% Descrip.: Marks or unmarks all message in set with DELETED flag
%% Returns : ok
%%--------------------------------------------------------------------

uid(Pid,fetch,{UIDSet,Query})          -> uid(Pid,fetch,{imapc_util:to_seq(UIDSet),imapc_util:build_fetch(Query)},imapc_util:tag());
uid(Pid,copy,{UIDSet,MailBox})         -> uid(Pid,copy, {imapc_util:to_seq(UIDSet),MailBox},imapc_util:tag());
uid(Pid,store,{UIDSet,ItemName,Flags}) -> uid(Pid,store,{imapc_util:to_seq(UIDSet),imapc_util:build_store(ItemName),imapc_util:build_flags(Flags)},imapc_util:tag());
uid(Pid,search,Query)                  -> uid(Pid,search,imapc_util:build_search(Query),imapc_util:tag()).

uid(Pid,Command,Args,Tag) -> gen_fsm:sync_send_event(Pid,{uid,Tag,Command,Args}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



























%%--------------------------------------------------------------------
%% Function: switch(Pid,MailBox)
%%           Pid     = pid()
%%           MailBox = string()
%% Descrip.: Closes any selected mailbox and selects specified mailbox
%% Returns : ok
%%--------------------------------------------------------------------

switch(Pid,MailBox) ->
	case info(Pid,state) of
		selected -> 
			Select = info(Pid,mailbox),
			if
				MailBox == Select -> ok;
				true -> 
					close(Pid),
					select(Pid,MailBox)
			end;
		_ -> select(Pid,MailBox)
	end.



%%--------------------------------------------------------------------
%% Function: move(Pid,MailBox,StatusCodes)
%%           Pid     = pid()
%%           Set     = list() of integer90
%%           MailBox = string()
%% Descrip.: Copies and deletes message set from selected mailbox to specified mailbox
%% Returns : ok
%%--------------------------------------------------------------------

move(Pid,Set,MailBox) ->
	copy(Pid,imapc_util:to_seq(Set),MailBox),
	delete(Pid,imapc_util:to_seq(Set)),
	ok.





%%--------------------------------------------------------------------
%% Function: sort(Pid,Order,Query,Charset)
%%           Pid     = pid()
%%           Order   = Order Terms
%%           Query   = Search Terms
%%           Charset = Charset (optional: default UTF-8)
%% Descrip.: Performs an ordered search query on selected mailbox
%% Returns : term(), list of integers
%%--------------------------------------------------------------------

sort(Pid,Order,Query) -> sort(Pid,Order,Query,"UTF-8"). 
sort(Pid,Order,Query,Charset) -> 
	Capability = info(Pid,capability),
	SortCap = lists:member("SORT",Capability),
	if
		SortCap -> imapc_fsm:sort(Pid,imapc_util:build_order(Order),imapc_util:build_search(Query),Charset);
		true    -> imapc_fsm:search(Pid,imapc_util:build_search(Query))
	end.



%%--------------------------------------------------------------------
%% Function: (un)deletef(Pid) 
%%           Pid     = pid()
%%           Set     = list() of integer()
%% Descrip.: Marks or unmarks all message in set with DELETED flag
%% Returns : ok
%%--------------------------------------------------------------------

deletef(Pid,Set)   -> store(Pid,Set,add_silent,[deleted]).
undeletef(Pid,Set) -> store(Pid,Set,remove_silent,[deleted]).

%%--------------------------------------------------------------------
%% Function: (un)draft(Pid)
%%           Pid     = pid()
%%           Set     = list() of integer()
%% Descrip.: Marks or unmarks all message in set with DRAFT flag
%% Returns : ok
%%--------------------------------------------------------------------

draft(Pid,Set)   -> store(Pid,Set,add_silent,[draft]).
undraft(Pid,Set) -> store(Pid,Set,remove_silent,[draft]).

%%--------------------------------------------------------------------
%% Function: (un)flag(Pid)
%%           Pid     = pid()
%%           Set     = list() of integer()
%% Descrip.: Marks or unmarks all message in set with FLAGGED flag
%% Returns : ok
%%--------------------------------------------------------------------

flag(Pid,Set)   -> store(Pid,Set,add_silent,[flagged]).
unflag(Pid,Set) -> store(Pid,Set,remove_silent,[flagged]).

%%--------------------------------------------------------------------
%% Function: (un)seen(Pid)
%%           Pid     = pid()
%%           Set     = list() of integer()
%% Descrip.: Marks or unmarks all message in set with SEEN flag
%% Returns : ok
%%--------------------------------------------------------------------

seen(Pid,Set)   -> store(Pid,Set,add_silent,[seen]).
unseen(Pid,Set) -> store(Pid,Set,remove_silent,[seen]).



%%--------------------------------------------------------------------
%% Function: info(Pid)
%%           info(Pid,Type)
%%           Pid  = pid()
%%           Type = atom(), Default all, options [all,state,mailbox,capability]
%% Descrip.: Returns current information ont eh state of the FSM
%% Returns : term()
%%--------------------------------------------------------------------
info(Pid) -> info(Pid,all).
info(Pid,Type) -> imapc_fsm:info(Pid,Type).

%%--------------------------------------------------------------------
%% Function: build(Type)
%%           Type = atom(), Default both, options [both,scanner,parser]
%% Descrip.: Returns current information ont eh state of the FSM
%% Returns : term() = results from yacc, leex or both
%%--------------------------------------------------------------------
-define(DEV_PATH,"/sfe/erlang/releases/lib/erlmail-" ++ ?ERLMAIL_VERSION).
build() -> build(both).
build(parser) -> yecc:file(?DEV_PATH ++ "/src/imap_parser.yrl",[{verbose,true}]);
build(scanner) -> leex:file(?DEV_PATH ++ "/src/imap_scan.xrl");
build(both) -> 
	P = build(parser),
	S = build(scanner),
	{P,S}.