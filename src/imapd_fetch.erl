%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       IMAP Server fetch routines
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.6
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
-module(imapd_fetch).
-author('sjackson@simpleenigma.com').
-include("../include/imap.hrl").
-include("../include/erlmail.hrl").



-export([items/1,items/2,tokens/1,tokens/2]).










items(String) ->	
	% Tokenize string
	items(String,[]).

items(_List,_Acc) -> ok.

%%-------------------------------------------------------------------------
%% @spec (String::string()) -> list()
%% @doc Takes a fetch string and returns a list of the data items.
%%      The data Items are return as a list of lower case atoms, or tuples
%% @end
%%-------------------------------------------------------------------------
tokens(String) -> 
	S1 = http_util:to_lower(String),
	S2 = string:strip(S1,both,40),
	S3 = string:strip(S2,both,41),
	tokens(list_to_binary(S3),[]).
%%-------------------------------------------------------------------------
%% @spec (String::string(),Acc::list()) -> list()
%% @hidden
%% @end
%%-------------------------------------------------------------------------
tokens(<<>>,Acc) -> lists:usort(Acc);
tokens(<<32,Rest/binary>>,Acc) ->
	tokens(Rest,Acc);
tokens(<<"all",Rest/binary>>,Acc) ->
	tokens(Rest,[flags,internaldate,'rfc822.size',envelope|Acc]);
tokens(<<"fast",Rest/binary>>,Acc) ->
	tokens(Rest,[flags,internaldate,'rfc822.size'|Acc]);
tokens(<<"full",Rest/binary>>,Acc) ->
	tokens(Rest,[flags,internaldate,'rfc822.size',envelope,body|Acc]);
tokens(<<"body.peek[",Rest/binary>>,Acc) ->
	String = binary_to_list(Rest),
	Pos = string:chr(String,93),
	{Sec,New} = lists:split(Pos,String),
	{Next,Start,Count} = case regexp:match(New,"^<([0-9]+\.[0-9]+)>") of
		{match,1,Length} -> 
			{Size,New2} = lists:split(Length,New),
			[S,C] = string:tokens(Size,[46,60,62]),
			{New2,list_to_integer(S),list_to_integer(C)};
		nomatch -> {New,0,0}
	end,
	Sections = body_sections(Sec),
	tokens(list_to_binary(Next),[{body.peek,{Start,Count},Sections}|Acc]);
tokens(<<"body[",Rest/binary>>,Acc) ->
	String = binary_to_list(Rest),
	Pos = string:chr(String,93),
	{Sec,New} = lists:split(Pos,String),
	{Next,Start,Count} = case regexp:match(New,"^<([0-9]+\.[0-9]+)>") of
		{match,1,Length} -> 
			{Size,New2} = lists:split(Length,New),
			[S,C] = string:tokens(Size,[46,60,62]),
			{New2,list_to_integer(S),list_to_integer(C)};
		nomatch -> {New,0,0}
	end,
	Sections = body_sections(Sec),
	tokens(list_to_binary(Next),[{body,{Start,Count},Sections}|Acc]);

tokens(<<"body",Rest/binary>>,Acc) ->
	tokens(Rest,[body|Acc]);
tokens(<<"bodystructure",Rest/binary>>,Acc) ->
	tokens(Rest,[bodystructure|Acc]);
tokens(<<"envelope",Rest/binary>>,Acc) ->
	tokens(Rest,[envelope|Acc]);
tokens(<<"flags",Rest/binary>>,Acc) ->
	tokens(Rest,[flags|Acc]);
tokens(<<"internaldate",Rest/binary>>,Acc) ->
	tokens(Rest,[internaldate|Acc]);
tokens(<<"rfc822.header",Rest/binary>>,Acc) ->
	tokens(Rest,[rfc822.header|Acc]);
tokens(<<"rfc822.size",Rest/binary>>,Acc) ->
	tokens(Rest,[rfc822.size|Acc]);
tokens(<<"rfc822.text",Rest/binary>>,Acc) ->
	tokens(Rest,[rfc822.text|Acc]);
tokens(<<"rfc822",Rest/binary>>,Acc) ->
	tokens(Rest,[rfc822|Acc]);
tokens(<<"uid",Rest/binary>>,Acc) ->
	tokens(Rest,[uid|Acc]);

tokens(Bin,_Acc) -> 
	?D(Bin),
	{error,unknown_token}.


%%-------------------------------------------------------------------------
%% @spec (String::string()) -> list()
%% @doc Takes a BODY or BODY.PEEK string and returns a list of the data items.
%%      The data Items are return as a list of lower case atoms, or tuples
%% @end
%%-------------------------------------------------------------------------
body_sections(String) -> 
	Bin = list_to_binary(string:strip(http_util:to_lower(String),right,93)),
	body_sections(Bin,[],[]).
%%-------------------------------------------------------------------------
%% @spec (Bin::binary(),Part::list(),Acc::list()) -> list()
%% @hidden
%% @end
%%-------------------------------------------------------------------------
body_sections(<<>>,_Part,Acc) -> lists:reverse(Acc);


body_sections(<<32,Rest/binary>>,Part,Acc) ->
	body_sections(Rest,Part,Acc);
body_sections(<<"header.fields.not",Rest/binary>>,Part,Acc) ->
	String = binary_to_list(Rest),
	Pos = string:chr(String,41),
	{F,Next} = lists:split(Pos,String),
	Fields = lists:map(fun(S) ->
		list_to_atom(http_util:to_lower(S))
		end,string:tokens(F,[32,40,41])),
	body_sections(list_to_binary(Next),[],[{'header.fields.not',Part,Fields}|Acc]);
body_sections(<<"header.fields",Rest/binary>>,Part,Acc) ->
	String = binary_to_list(Rest),
	Pos = string:chr(String,41),
	{F,Next} = lists:split(Pos,String),
	Fields = lists:map(fun(S) ->
		list_to_atom(http_util:to_lower(S))
		end,string:tokens(F,[32,40,41])),
	body_sections(list_to_binary(Next),[],[{'header.fields',Part,Fields}|Acc]);
body_sections(<<"header",Rest/binary>>,Part,Acc) ->
	body_sections(Rest,[],[{header,Part}|Acc]);
body_sections(<<"mime",Rest/binary>>,Part,Acc) ->
	body_sections(Rest,[],[{mime,Part}|Acc]);
body_sections(<<"text",Rest/binary>>,Part,Acc) ->
	body_sections(Rest,[],[{text,Part}|Acc]);

body_sections(Bin,_Part,Acc) ->
	String = binary_to_list(Bin),
	RegExp = "^([0-9]\.)+",
	case regexp:match(String,RegExp) of
		{match,1,Length} -> 
			{P,Next} = lists:split(Length,String),
			Part = lists:map(fun(S) ->
				list_to_integer(S)
				end,string:tokens(P,[46])),
			body_sections(list_to_binary(Next),Part,Acc);
		
		nomatch -> 
			?D(Bin),
			{error,unknown_body_section}
	end.
