%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       Erlmail utility functions
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.5
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
-module(erlmail_util).
-author('sjackson@simpleenigma.com').

-include("../include/smtp.hrl").

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