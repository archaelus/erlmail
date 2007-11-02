%%%----------------------------------------------------------------------
%%% File        : gen_store
%%% Author      : Stuart Jackson <sjackson@simpleenigma.com> [http://www.simpleenigma.com]
%%% Purpose     : Generalized Messag Store behavior defination
%%% Created     : 2007-10-18
%%% Initial Rel : 0.0.5
%%% Updated     : 2007-10-18
%%%----------------------------------------------------------------------
-module(gen_store).
-author('sjackson@simpleenigma.com').
-include("erlmail.hrl").

-export([message_name/1,lookup/1,lookup/2]).
-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{create,1},{drop,1},{insert,1},{update,1},{delete,1},{select,2},
								{list,0},{list,1},
								{message_name,1},{ensure_inbox,1},{deliver,1},{check,1},
								{unseen,1},{recent,1},{mlist,3}
								];
behaviour_info(_Other) -> undefined.



lookup(Type) -> lookup(Type,erlmail_conf:read()).
lookup(domain,       State) -> erlmail_conf:lookup_atom(store_type_domain,       State);
lookup(user,         State) -> erlmail_conf:lookup_atom(store_type_user,         State);
lookup(message,      State) -> erlmail_conf:lookup_atom(store_type_message,      State);
lookup(mailbox_store,State) -> erlmail_conf:lookup_atom(store_type_mailbox_store,State);
lookup(all,State)           ->
	{lookup(domain,State),lookup(user,State),lookup(message,State),lookup(mailbox_store,State)}.




message_name(_Args) ->
	{Number,_} = random:uniform_s(9000,now()),
	Seconds = calendar:datetime_to_gregorian_seconds({date(),time()}) - 62167219200,
	Node = atom_to_list(node()),
	[_N,Host] = string:tokens(Node,[64]),
	integer_to_list(Seconds) ++ [46] ++ 						  % Seconds since UNIX Epoch (1-1-1970)
	[77] ++ integer_to_list(calendar:time_to_seconds(time())) ++  % M then Seconds in day
	[82] ++ integer_to_list(Number + 1000) ++ [46] ++             % R then random number between 1,001 and 10,000
	Host.