%%%----------------------------------------------------------------------
%%% File        : erlmail_util
%%% Author      : Stuart Jackson <sjackson@simpleenigma.com> [http://www.simpleenigma.com]
%%% Purpose     : Erlmail utility functions
%%% Created     : 2007-10-18
%%% Initial Rel : 0.0.5
%%% Updated     : 2007-10-18
%%%----------------------------------------------------------------------
-module(erlmail_util).
-author('sjackson@simpleenigma.com').

-include("smtp.hrl").

-export([split_email/1,combine_email/1,combine_email/2]).



split_email(Atom) when is_atom(Atom) -> split_email(atom_to_list(Atom));
split_email([]) -> {[],[]};
split_email(EmailAddress) ->
	case string:tokens(string:strip(EmailAddress),[64]) of
		[UserName,DomainName] -> {UserName,DomainName};
		_AnythingsElse -> {[],[]}
	end.

combine_email([])                                 -> [];
combine_email({[],[]})                            -> [];
combine_email({UsersName,DomainName})             -> combine_email(UsersName,DomainName).
combine_email(Atom,DomainName) when is_atom(Atom) -> combine_email(atom_to_list(Atom),DomainName);
combine_email(UsersName,Atom)  when is_atom(Atom) -> combine_email(UsersName,atom_to_list(Atom));
combine_email(UsersName,DomainName)               -> UsersName ++ [64] ++ DomainName.